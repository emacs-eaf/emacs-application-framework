#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2018 Andy Stewart
#
# Author:     SpringHan <springchohaku@qq.com>
# Maintainer: SpringHan <springchohaku@qq.com>
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
from PyQt5.QtGui import QColor
from PyQt5.QtCore import QUrl
from core.webengine import BrowserBuffer
from core.utils import interactive, eval_in_emacs, message_to_emacs, get_emacs_var
import os

class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, arguments):
        BrowserBuffer.__init__(self, buffer_id, url, arguments, False)

        self.backgroundColor = QColor(get_emacs_var("eaf-emacs-theme-background-color")).darker(110).name()
        
        self.buffer_widget.loadFinished.connect(self.init_app)
        self.load_index_html(__file__)

    @QtCore.pyqtSlot(list)
    def play_song(self, song):
        eval_in_emacs("netease-cloud-music-play", song)

    @QtCore.pyqtSlot(bool)
    def play_next_or_prev(self, prev):
        if prev:
            play_function = "netease-cloud-music-play-previous-song"
        else:
            play_function = "netease-cloud-music-play-next-song"

        eval_in_emacs(play_function, [])

    @QtCore.pyqtSlot(list)
    def change_playlist(self, pid):
        eval_in_emacs('''eaf--netease-cloud-music-change-playlist''', pid)

    @interactive(insert_or_do=True)
    @QtCore.pyqtSlot()
    def change_repeat_mode(self):
        eval_in_emacs("netease-cloud-music-change-repeat-mode", [])

    @interactive(insert_or_do=True)
    @QtCore.pyqtSlot()
    def play_or_pause(self):
        if get_emacs_var("eaf-netease-cloud-music-play-status") == "":
            message_to_emacs("You've never started to play a song.")
            return
        eval_in_emacs("netease-cloud-music-pause-or-continue", [])

    @QtCore.pyqtSlot(list)
    def switch_enter(self, index):
        eval_in_emacs('''eaf--netease-cloud-music-switch-enter''', index)

    def init_app(self):
        self.buffer_widget.execute_js('initColor(\"{}\", \"{}\")'.format(
            get_emacs_var("eaf-emacs-theme-background-color"),
            get_emacs_var("eaf-emacs-theme-foreground-color")
        ))
        # Init
        eval_in_emacs('''eaf--netease-cloud-music-init''', [])
