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
from core.utils import touch, string_to_base64
import os
import base64

class AppBuffer(BrowserBuffer):

    export_org_json = QtCore.pyqtSignal(str, str)

    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, False, QColor(255, 255, 255, 255))

        self.url = url
        index_file = "file://" + (os.path.join(os.path.dirname(__file__), "index.html"))
        self.buffer_widget.setUrl(QUrl(index_file))

        QTimer.singleShot(500, self.play_video)

    def play_video(self):
        self.buffer_widget.eval_js("play('{}');".format("file://" + self.url))
