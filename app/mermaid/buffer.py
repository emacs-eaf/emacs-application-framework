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


from PyQt5 import QtCore
from PyQt5.QtCore import QUrl, QTimer, QFileSystemWatcher
from PyQt5.QtWidgets import QApplication
from PyQt5.QtGui import QColor
from core.browser import BrowserBuffer
from core.utils import string_to_base64
from core.utils import PostGui
import threading
import markdown
import os
from pathlib import Path

class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, async_call_emacs):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, async_call_emacs, False)

        self.url = url
        self.render()
        self.watcher = QFileSystemWatcher()
        self.watcher.addPath(self.url)
        self.watcher.fileChanged.connect(lambda : self.render())


    def render(self):
        with open(self.url, "r") as f:
            html = markdown.markdown(f.read(), extensions=['app.mermaid.md_mermaid'])
            self.buffer_widget.setHtml(html)
