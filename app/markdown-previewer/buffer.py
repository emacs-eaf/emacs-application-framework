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
import threading

class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, async_call_emacs):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, async_call_emacs, False)

        # Get free port to render markdown.
        self.port = get_free_port()
        self.url = url

        # Start markdown render process.
        if arguments == "":
            subprocess.Popen("grip {0} {1}".format(url, self.port), shell=True)
        else:
            subprocess.Popen("grip --pass {0} {1} {2}".format(arguments, url, self.port), shell=True)

        # Add timer make load markdown preview link after grip process start finish.
        timer = threading.Timer(2, self.load_markdown_server)
        timer.start()

    @PostGui()
    def load_markdown_server(self):
        self.buffer_widget.setUrl(QUrl("http://localhost:{0}".format(self.port)))

        paths = os.path.split(self.url)
        if len(paths) > 0:
            self.change_title(paths[-1])
