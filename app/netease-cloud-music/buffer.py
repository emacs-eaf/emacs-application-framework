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
from core.utils import interactive
from core.utils import eval_in_emacs
from core.utils import message_to_emacs
import os
class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, False)

        self.index_file_dir = os.path.join(os.path.dirname(__file__), "dist")
        self.index_file = os.path.join(self.index_file_dir, "index.html")
        self.url = url

        self.backgroundColor = QColor(self.emacs_var_dict["eaf-emacs-theme-background-color"]).darker(110).name()

        self.buffer_widget.loadFinished.connect(self.init_app)

        with open(self.index_file, "r") as f:
            html = self.convert_index_html(f.read(), self.index_file_dir)
            self.buffer_widget.setHtml(html, QUrl("file://"))

        for (python_method_name, js_method_name) in [("play_next", "playNext"),
                                                     ("play_prev", "playPrev"),
                                                     ("scroll_up", "scrollUp"),
                                                     ("scroll_down", "scrollDown"),
                                                     ("scroll_to_begin", "scrollToBegin"),
                                                     ("scroll_to_bottom", "scrollToBottom")
                                                     ]:
            self.build_js_bridge_method(python_method_name, js_method_name)

    @QtCore.pyqtSlot(list)
    def play_song(self, song):
        eval_in_emacs("netease-cloud-music-play", song)

    def init_app(self):
        self.buffer_widget.execute_js('initColor(\"{}\", \"{}\")'.format(
            self.emacs_var_dict["eaf-emacs-theme-background-color"],
            self.emacs_var_dict["eaf-emacs-theme-foreground-color"]
        ))

    @QtCore.pyqtSlot(bool)
    def play_next_or_prev(self, prev):
        if prev:
            play_function = "netease-cloud-music-play-previous-song"
        else:
            play_function = "netease-cloud-music-play-next-song"

        eval_in_emacs(play_function, [])

    @QtCore.pyqtSlot(int)
    def change_playlist(pid):
        eval_in_emacs('''(eaf--netease-cloud-music-change-playlist \"{}\")'''
                      .format(str(pid)))
