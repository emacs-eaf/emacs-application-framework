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

from PyQt5.QtCore import QUrl, QTimer, QEvent, QPointF, Qt
from PyQt5.QtGui import QColor, QMouseEvent
from PyQt5.QtWidgets import QApplication
from core.browser import BrowserBuffer
from core.utils import PostGui, get_free_port, interactive
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
        self.start_directory = arguments_dict["directory"]
        self.current_directory = self.start_directory
        self.executing_command = ""
        self.index_file = "{0}/index.html".format(self.http_url)
        self.server_js = os.path.join(os.path.dirname(__file__), "server.js")

        self.buffer_widget.titleChanged.connect(self.change_title)

        http_thread = threading.Thread(target=self.run_http_server, args=())
        http_thread.start()

        # Start server process.
        self.background_process = subprocess.Popen(
            "node {0} {1} '{2}' '{3}'".format(self.server_js, self.port, self.start_directory, self.command),
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            shell=True)

        self.open_terminal_page()

        self.reset_default_zoom()

        QTimer.singleShot(250, self.focus_terminal)

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

    def focus_terminal(self):
        event = QMouseEvent(QEvent.MouseButtonPress, QPointF(0, 0), Qt.LeftButton, Qt.LeftButton, Qt.NoModifier)
        QApplication.sendEvent(self.buffer_widget.focusProxy(), event)

    @PostGui()
    def open_terminal_page(self):
        theme = "light"
        if self.emacs_var_dict["eaf-terminal-dark-mode"] == "true" or \
           (self.emacs_var_dict["eaf-terminal-dark-mode"] == "follow" and self.emacs_var_dict["eaf-emacs-theme-mode"] == "dark"):
            theme = "dark"

        with request.urlopen(self.index_file) as f:
            html = f.read().decode("utf-8").replace("%1", str(self.port)).replace("%2", self.http_url).replace("%3", theme).replace("%4", self.emacs_var_dict["eaf-terminal-font-size"]).replace("%5", self.current_directory)
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
                self.eval_in_emacs.emit('''(setq default-directory "'''+ changed_directory +'''")''')
                self.current_directory = changed_directory
            if self.executing_command != "":
                self.executing_command = ""

        if subprocess.Popen.poll(self.background_process) is not None:
            self.destroy_buffer()

    def destroy_buffer(self):
        super().destroy_buffer()
        self.close_buffer()
        self.timer.stop()

    @interactive()
    def copy_text(self):
        text = self.buffer_widget.execute_js("get_selection();")
        if text == "":
            self.message_to_emacs.emit("Nothing selected")
        else:
            clipboard = QApplication.clipboard()
            clipboard.setText(text)
            self.message_to_emacs.emit("Copy text")

    @interactive()
    def yank_text(self):
        text = QApplication.clipboard().text()
        self.buffer_widget.eval_js("paste('{}');".format(text))

    @interactive()
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

    @interactive()
    def scroll_up(self):
        self.buffer_widget.eval_js("scroll_line(1);")

    @interactive()
    def scroll_down(self):
        self.buffer_widget.eval_js("scroll_line(-1);")

    @interactive()
    def scroll_up_page(self):
        self.buffer_widget.eval_js("scroll_page(1);")

    @interactive()
    def scroll_down_page(self):
        self.buffer_widget.eval_js("scroll_page(-1);")

    @interactive()
    def scroll_to_begin(self):
        self.buffer_widget.eval_js("scroll_to_begin();")

    @interactive()
    def scroll_to_bottom(self):
        self.buffer_widget.eval_js("scroll_to_bottom();")

    def select_all(self):
        self.buffer_widget.eval_js("select_all();")

    def clear_selection(self):
        self.buffer_widget.eval_js("clear_selection();")

    def _search_text(self, text, is_backward = False):
        if self.search_term != text:
            self.search_term = text
        if is_backward:
            # self.web_page.findText(self.search_term, self.web_page.FindBackward)
            self.buffer_widget.eval_js("find_next('{}')".format(text))
        else:
            # self.web_page.findText(self.search_term)
            self.buffer_widget.eval_js("find_prev('{}')".format(text))

    @interactive()
    def search_text_forward(self):
        if self.search_term == "":
            self.buffer.send_input_message("Forward Search Text: ", "search_text_forward")
        else:
            self._search_text(self.search_term)

    @interactive()
    def search_text_backward(self):
        if self.search_term == "":
            self.buffer.send_input_message("Backward Search Text: ", "search_text_backward")
        else:
            self._search_text(self.search_term, True)

    @interactive()
    def search_quit(self):
        if self.search_term != "":
            self._search_text("")

    def handle_input_message(self, result_tag, result_content):
        if result_tag == "search_text_forward":
            self.buffer_widget._search_text(str(result_content))
        elif result_tag == "search_text_backward":
            self.buffer_widget._search_text(str(result_content), True)
