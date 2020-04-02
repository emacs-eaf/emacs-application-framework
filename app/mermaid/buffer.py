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
from PyQt5.QtCore import QUrl, QTimer
from PyQt5.QtWidgets import QApplication
from PyQt5.QtGui import QColor
from core.browser import BrowserBuffer
from core.utils import string_to_base64
from core.utils import PostGui
import threading
import pyinotify
import markdown
import os

class AppBuffer(BrowserBuffer):

    update_content = QtCore.pyqtSignal()

    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, call_emacs):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, call_emacs, False, QColor(255, 255, 255, 255))

        self.url = url
        self.render()

        self.update_content.connect(self.render)
        threading.Timer(1, self.monitor_file_change).start()

    def monitor_file_change(self):
        parent = self

        class ModHandler(pyinotify.ProcessEvent):
            def process_IN_CLOSE_WRITE(self, evt):
                parent.update_content.emit()

        handler = ModHandler()
        wm = pyinotify.WatchManager()
        notifier = pyinotify.Notifier(wm, handler)
        wdd = wm.add_watch(self.url, pyinotify.IN_CLOSE_WRITE)
        notifier.loop()

    def render(self):
        with open(self.url, "r") as f:
            html = markdown.markdown(f.read(), extensions=['app.mermaid.md_mermaid'])
            self.buffer_widget.setHtml(html)
