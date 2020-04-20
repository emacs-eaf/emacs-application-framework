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
from core.utils import PostGui, get_free_port
import os
import subprocess
import signal
import threading
import getpass
import json

class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, False)

        # Get free port.
        self.port = get_free_port()
        self.url = url

        arguments_dict = json.loads(arguments)
        self.command = arguments_dict["command"]
        self.start_directory = arguments_dict["directory"]

        self.index_file = os.path.join(os.path.dirname(__file__), "index.html")
        self.server_js = os.path.join(os.path.dirname(__file__), "server.js")

        # Start server process.
        self.background_process = subprocess.Popen(
            "node {0} {1} '{2}' '{3}'".format(self.server_js, self.port, self.start_directory, self.command),
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            shell=True)

        self.open_terminal_page()

        self.reset_default_zoom()

        QTimer.singleShot(250, self.focus_terminal)

    def focus_terminal(self):
        event = QMouseEvent(QEvent.MouseButtonPress, QPointF(0, 0), Qt.LeftButton, Qt.LeftButton, Qt.NoModifier)
        QApplication.sendEvent(self.buffer_widget.focusProxy(), event)

    @PostGui()
    def open_terminal_page(self):
        theme = "light"
        if self.emacs_var_dict["eaf-terminal-dark-mode"] == "true" or \
           (self.emacs_var_dict["eaf-terminal-dark-mode"] == "" and self.emacs_var_dict["eaf-emacs-theme-mode"] == "dark"):
            theme = "dark"
        with open(self.index_file, "r") as f:
            html = f.read().replace("%1", str(self.port)).replace("%2", "file://" + os.path.join(os.path.dirname(__file__))).replace("%3", theme).replace("%4", self.emacs_var_dict["eaf-terminal-font-size"])
            self.buffer_widget.setHtml(html)

        self.update_title()

    def update_title(self):
        self.change_title("{0}-{1}".format(
            os.path.basename(os.path.normpath(os.path.expanduser(self.start_directory))),
            self.random_string()
        ))

    def destroy_buffer(self):
        os.kill(self.background_process.pid, signal.SIGKILL)

        if self.buffer_widget is not None:
            # NOTE: We need delete QWebEnginePage manual, otherwise QtWebEngineProcess won't quit.
            self.buffer_widget.web_page.deleteLater()
            self.buffer_widget.deleteLater()

    def random_string(self):
        import hashlib
        import time

        hash = hashlib.sha1()
        hash.update(str(time.time()).encode("utf-8"))
        return hash.hexdigest()[:4]

    def copy_text(self):
        text = self.buffer_widget.execute_js("get_selection();")
        if text == "":
            self.message_to_emacs.emit("Nothing selected")
        else:
            clipboard = QApplication.clipboard()
            clipboard.setText(text)
            self.message_to_emacs.emit("Copy text")

    def yank_text(self):
        text = QApplication.clipboard().text()
        self.buffer_widget.eval_js("paste('{}');".format(text))

    def scroll(self, scroll_direction, scroll_type):
        if scroll_type == "page":
            if scroll_direction == "up":
                self.buffer_widget.eval_js("scroll_page(1);")
            else:
                self.buffer_widget.eval_js("scroll_page(-1);")
        else:
            if scroll_direction == "up":
                self.buffer_widget.eval_js("scroll_line(1);")
            else:
                self.buffer_widget.eval_js("scroll_line(-1);")

    def scroll_up_page(self):
        self.buffer_widget.eval_js("scroll_page(1);")

    def scroll_down_page(self):
        self.buffer_widget.eval_js("scroll_page(-1);")

    def scroll_to_begin(self):
        self.buffer_widget.eval_js("scroll_to_begin();")

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

    def search_text_forward(self):
        if self.search_term == "":
            self.buffer.send_input_message("Forward Search Text: ", "search_text_forward")
        else:
            self._search_text(self.search_term)

    def search_text_backward(self):
        if self.search_term == "":
            self.buffer.send_input_message("Backward Search Text: ", "search_text_backward")
        else:
            self._search_text(self.search_term, True)

    def search_quit(self):
        if self.search_term != "":
            self._search_text("")

    def handle_input_message(self, result_tag, result_content):
        if result_tag == "search_text_forward":
            self.buffer_widget._search_text(str(result_content))
        elif result_tag == "search_text_backward":
            self.buffer_widget._search_text(str(result_content), True)
