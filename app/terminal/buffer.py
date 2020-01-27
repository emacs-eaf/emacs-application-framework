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
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, False, QColor(255, 255, 255, 255))

        # Get free port to render markdown.
        self.port = get_free_port()
        self.url = url

        # Start wetty process.
        self.background_process = subprocess.Popen(
            "wetty -p {0} --base / --sshuser {1} --sshauth publickey -c bash".format(self.port, getpass.getuser()),
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            shell=True)

        # Add timer make load markdown preview link after grip process start finish.
        threading.Timer(1, self.load_wetty_server).start()

        self.reset_default_zoom()

    @PostGui()
    def load_wetty_server(self):
        self.buffer_widget.setUrl(QUrl("http://localhost:{0}".format(self.port)))

        paths = os.path.split(self.url)
        if len(paths) > 0:
            self.change_title(paths[-1])

    def handle_destroy(self):
        os.killpg(os.getpgid(self.background_process.pid), signal.SIGTERM)

        self.before_destroy_hook.emit()

        if self.buffer_widget is not None:
            self.buffer_widget.destroy()
