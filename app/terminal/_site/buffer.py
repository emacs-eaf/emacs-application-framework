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

from PyQt5.QtCore import QUrl
from PyQt5.QtGui import QColor
from core.browser import BrowserBuffer
from core.utils import PostGui, get_free_port
import os
import subprocess
import signal
import threading
import getpass

class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, call_emacs):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, call_emacs, False, QColor(255, 255, 255, 255))

        # Get free port to render markdown.
        self.port = get_free_port()
        self.url = url

        argument_list = arguments.split("ᛡ")
        self.command = argument_list[0]
        self.start_directory = argument_list[1]

        self.server_js = (os.path.join(os.path.dirname(__file__), "server.js"))

        # Start server process.
        self.background_process = subprocess.Popen(
            "node {0} {1} {2} {3}".format(
                self.server_js,
                self.port,
                "'{}'".format(self.start_directory),
                "'{}'".format(self.command),
                stdout=subprocess.PIPE
            ),
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            shell=True)

        self.load_server()

        self.reset_default_zoom()

    @PostGui()
    def load_server(self):
        self.index_file = (os.path.join(os.path.dirname(__file__), "index.html"))
        with open(self.index_file, "r") as f:
            html = f.read().replace("%1", str(self.port)).replace("%2", "file://" + os.path.join(os.path.dirname(__file__)))
            self.buffer_widget.setHtml(html)

        self.update_title()

    def update_title(self):
        self.change_title("{0}-{1}".format(
            os.path.basename(os.path.normpath(os.path.expanduser(self.start_directory))),
            self.random_string()
        ))

    def resize_view(self):
        # self.buffer_widget.eval_js("sendSizeToServer();")
        pass

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
