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
from PyQt5.QtCore import QUrl, QFileSystemWatcher
from core.browser import BrowserBuffer
import os
import subprocess
import tempfile

class AppBuffer(BrowserBuffer):

    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, False)

        self.url = url
        self.preview_file = tempfile.mkstemp(prefix='eaf-', suffix='.html', text=True)[1]
        self.render_js = os.path.join(os.path.dirname(__file__), "render.js")
        self.dark_mode = "false"
        if emacs_var_dict["eaf-markdown-dark-mode"] == "true" or \
           (emacs_var_dict["eaf-markdown-dark-mode"] == "follow" and emacs_var_dict["eaf-emacs-theme-mode"] == "dark"):
            self.dark_mode = "true"

        self.render()
        self.file_watcher = QFileSystemWatcher()
        self.file_watcher.fileChanged.connect(self.render)
        self.file_watcher.addPath(url)

    def render(self):
        args = ["node", self.render_js, self.url, self.preview_file, self.dark_mode]
        with subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=False) as p:
            if p.wait() == 0:
                self.buffer_widget.load(QUrl.fromLocalFile(self.preview_file))
            else:
                self.message_to_emacs.emit("Preview failed")
