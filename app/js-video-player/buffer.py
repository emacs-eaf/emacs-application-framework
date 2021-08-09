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
from core.webengine import BrowserBuffer
from core.utils import touch, string_to_base64, get_emacs_var
import os
import base64

class AppBuffer(BrowserBuffer):

    def __init__(self, buffer_id, url, arguments):
        BrowserBuffer.__init__(self, buffer_id, url, arguments, False)

        self.url = url
        index_file = os.path.join(os.path.dirname(__file__), "index.html")
        self.buffer_widget.setUrl(QUrl.fromLocalFile(index_file))

        QTimer.singleShot(500, self.play_video)

    def save_session_data(self):
        return str(self.buffer_widget.execute_js("get_current_time();"))

    def restore_session_data(self, session_data):
        self.position = session_data
        QTimer.singleShot(600, self.restore_seek_position)

    def restore_seek_position(self):
        self.buffer_widget.eval_js("set_current_time('{}');".format(self.position))

    def play_video(self):
        self.buffer_widget.eval_js("play(\"{}\");".format(QUrl.fromLocalFile(self.url).toString()))

    def dark_mode_is_enabled(self):
        ''' Return bool of whether dark mode is enabled.'''
        return (get_emacs_var("eaf-js-video-player-dark-mode") == "force" or \
                get_emacs_var("eaf-js-video-player-dark-mode") == True or \
                (get_emacs_var("eaf-js-video-player-dark-mode") == "follow" and \
                 get_emacs_var("eaf-emacs-theme-mode") == "dark"))
