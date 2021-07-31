#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2018 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

from PyQt5.QtCore import QUrl, QTimer, QPointF, Qt
from PyQt5.QtGui import QColor
from PyQt5.QtWidgets import QApplication
from core.webengine import BrowserBuffer
from core.utils import PostGui, get_free_port, interactive, string_to_base64, eval_in_emacs, message_to_emacs, get_emacs_var
import os
import subprocess
import signal
import threading
import getpass
import json
from urllib import request
from http.server import SimpleHTTPRequestHandler
from socketserver import TCPServer

class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, False)

        # Get free port.
        self.port = get_free_port()
        self.http_url = "http://127.0.0.1:{0}".format(get_free_port())
        self.url = url

        arguments_dict = json.loads(arguments)
        self.command = arguments_dict["command"]
        self.start_directory = arguments_dict["directory"].rstrip('/')
        self.current_directory = self.start_directory
        self.executing_command = ""
        self.index_file = "{0}/index.html".format(self.http_url)
        self.server_js = os.path.join(os.path.dirname(__file__), "server.js")

        self.buffer_widget.titleChanged.connect(self.change_title)

        http_thread = threading.Thread(target=self.run_http_server, args=())
        http_thread.start()

        self.search_term = ""

        # Start server process.
        args = ["node", self.server_js, str(self.port), self.start_directory, self.command]
        self.background_process = subprocess.Popen(
            args,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            shell=False)

        # The background process (server.js) might take time to start.
        # If we open the terminal page before it's up and running, we'll get a
        # "connection refused" error when connecting to the websocket port.
        # User will have to reload the page to get a terminal.
        # Adding this extra step seems to solve this timing problem.
        try:
            outs, errs = self.background_process.communicate(timeout=1)
        except Exception:
            print("Terminal: timed out when communicating with server.js.")

        self.open_terminal_page()

        QTimer.singleShot(250, self.focus_widget)

        self.build_all_methods(self)

        self.timer=QTimer()
        self.timer.start(250)
        self.timer.timeout.connect(self.checking_status)

    def run_http_server(self):
        class Handler(SimpleHTTPRequestHandler):
            def __init__(self, *args, **kwargs):
                # directory=os.path.dirname(__file__), This argument add in python3.7 after
                super().__init__(*args, **kwargs)
            def translate_path(self, path):
                # On python3.7 before version, http server don't support setting directory
                # default use the project path.
                path = super().translate_path(path)
                return os.path.dirname(__file__) + path[len(os.getcwd()):]
        with TCPServer(("127.0.0.1", int(self.http_url.split(":")[-1])), Handler) as h:
            h.serve_forever()

    @PostGui()
    def open_terminal_page(self):
        theme = "light"
        if (get_emacs_var("eaf-terminal-dark-mode") == "follow" and get_emacs_var("eaf-emacs-theme-mode") == "dark"):
            theme = "dark"

        with request.urlopen(self.index_file) as f:
            html = f.read().decode("utf-8").replace("%1", str(self.port))\
                                           .replace("%2", self.http_url)\
                                           .replace("%3", theme)\
                                           .replace("%4", str(get_emacs_var("eaf-terminal-font-size")))\
                                           .replace("%5", self.current_directory)\
                                           .replace("%6", get_emacs_var("eaf-terminal-font-family"))
            self.buffer_widget.setHtml(html)

    def checking_status(self):
        changed_directory = str(self.buffer_widget.execute_js("title"))
        changed_executing_command = str(self.buffer_widget.execute_js("executing_command"))
        if len(changed_executing_command) > 30:
            changed_executing_command = changed_executing_command[:30]

        if changed_executing_command != self.executing_command and changed_executing_command != "":
            self.change_title(changed_executing_command)
            self.executing_command = changed_executing_command
        elif changed_executing_command == "" and self.executing_command != "" or not changed_directory == self.current_directory:
            self.change_title(changed_directory)
            if not changed_directory == self.current_directory:
                eval_in_emacs('eaf--change-default-directory', [changed_directory])
                self.current_directory = changed_directory
            if self.executing_command != "":
                self.executing_command = ""

        if subprocess.Popen.poll(self.background_process) is not None:
            self.destroy_buffer()

    def destroy_buffer(self):
        self.close_buffer()
        self.timer.stop()

    @interactive
    def copy_text(self):
        text = self.buffer_widget.execute_js("get_selection();")
        if text == "":
            message_to_emacs("Nothing selected")
        else:
            self.set_clipboard_text(text)
            message_to_emacs("Copy text")

    @interactive
    def yank_text(self):
        text = self.get_clipboard_text()
        self.buffer_widget.eval_js("paste('{}');".format(string_to_base64(text)))

    @interactive
    def scroll_other_buffer(self, scroll_direction, scroll_type):
        if scroll_type == "page":
            if scroll_direction == "up":
                self.scroll_up_page()
            else:
                self.scroll_down_page()
        else:
            if scroll_direction == "up":
                self.scroll_up()
            else:
                self.scroll_down()

    @interactive
    def scroll_up(self):
        self.buffer_widget.eval_js("scroll_line(1);")

    @interactive
    def scroll_down(self):
        self.buffer_widget.eval_js("scroll_line(-1);")

    @interactive
    def scroll_up_page(self):
        self.buffer_widget.eval_js("scroll_page(1);")

    @interactive
    def scroll_down_page(self):
        self.buffer_widget.eval_js("scroll_page(-1);")

    @interactive
    def scroll_to_begin(self):
        self.buffer_widget.eval_js("scroll_to_begin();")

    @interactive
    def scroll_to_bottom(self):
        self.buffer_widget.eval_js("scroll_to_bottom();")

    def select_all(self):
        self.buffer_widget.eval_js("select_all();")

    def clear_selection(self):
        self.buffer_widget.eval_js("clear_selection();")

    def clear(self):
        self.buffer_widget.eval_js("clear();")

    def _search_text(self, text, is_backward = False):
        if self.search_term != text:
            self.search_term = text
        if is_backward:
            # self.web_page.findText(self.search_term, self.web_page.FindBackward)
            self.buffer_widget.eval_js("find_next('{}')".format(text))
        else:
            # self.web_page.findText(self.search_term)
            self.buffer_widget.eval_js("find_prev('{}')".format(text))

    @interactive
    def search_text_forward(self):
        if self.search_term == "":
            self.send_input_message("Forward Search Text: ", "search_text_forward")
        else:
            self._search_text(self.search_term)

    @interactive
    def search_text_backward(self):
        if self.search_term == "":
            self.send_input_message("Backward Search Text: ", "search_text_backward")
        else:
            self._search_text(self.search_term, True)

    @interactive
    def action_quit(self):
        if self.search_term != "":
            self._search_text("")

    def handle_input_response(self, callback_tag, result_content):
        if callback_tag == "search_text_forward":
            self._search_text(str(result_content))
        elif callback_tag == "search_text_backward":
            self._search_text(str(result_content), True)
