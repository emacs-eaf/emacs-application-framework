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
from core.utils import list_string_to_list
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

    @QtCore.pyqtSlot()
    def set_playlist(self, playlist=None):
        if playlist:
            self.buffer_widget.execute_js('''setPlaylist({})'''.format(
                list_string_to_list(playlist)))
        else:
            if self.emacs_var_dict["eaf-netease-cloud-music-playlist-id"] == "0":
                playlist_var = 'eaf-netease-cloud-music-playlist'
            else:
                playlist_var = 'eaf-netease-cloud-music-playlists-songs'

            self.buffer_widget.execute_js('''setPlaylist({})'''.format(
                list_string_to_list(self.emacs_var_dict[playlist_var])))

    @QtCore.pyqtSlot()
    def change_repeat_mode(self):
        eval_in_emacs("netease-cloud-music-change-repeat-mode", [])

    @QtCore.pyqtSlot()
    def play_or_pause(self):
        if self.emacs_var_dict["eaf-netease-cloud-music-play-status"] == "":
            message_to_emacs("You've never started to play a song.")
            return
        eval_in_emacs("netease-cloud-music-pause-or-continue", [])

    @QtCore.pyqtSlot()
    def update_play_status(self):
        if (self.emacs_var_dict["eaf-netease-cloud-music-play-status"] == "") or (self.emacs_var_dict["eaf-netease-cloud-music-play-status"] == "paused"):
            icon = 'play-circle'
        else:
            icon = 'pause-circle'
            
        self.buffer_widget.execute_js('''setPlayIconStatus(\"{}\")'''.format(icon))

    @interactive(insert_or_do=True)
    def back_to_last_buffer(self):
        eval_in_emacs('''netease-cloud-music-back''', [])

    @interactive(insert_or_do=True)
    def quit(self):
        eval_in_emacs('''netease-cloud-music-quit''', [])

    def init_app(self):
        self.buffer_widget.execute_js('initColor(\"{}\", \"{}\")'.format(
            self.emacs_var_dict["eaf-emacs-theme-background-color"],
            self.emacs_var_dict["eaf-emacs-theme-foreground-color"]
        ))
        self.update_user_info()
        self.refresh_user_playlist()
        self.update_playlist_style(True)
        self.set_repeat_mode()
        self.set_panel_song()
        self.update_play_status()

        # Init
        eval_in_emacs('''eaf--netease-cloud-music-init''', [])
        self.set_playlist()

    def update_user_info(self):
        self.buffer_widget.execute_js('''updateUserInfo({})'''.format(
            list_string_to_list(self.emacs_var_dict["eaf-netease-cloud-music-user"])))

    def refresh_user_playlist(self):
        '''Only refresh the value.'''
        self.buffer_widget.execute_js('''setUserPlaylists({})'''.format(
            list_string_to_list(self.emacs_var_dict["eaf-netease-cloud-music-playlists"])))

    def update_playlist_style(self, init=False):
        if init:
            func_string = '''changePlaylistStyle({}, true)'''
        else:
            func_string = '''changePlaylistStyle({})'''

        self.buffer_widget.execute_js(func_string.format(
            self.emacs_var_dict["eaf-netease-cloud-music-playlist-id"]))

    def set_panel_song(self):
        '''For ELisp to call'''
        self.buffer_widget.execute_js('''setPanelSongInfo({})'''.format(
            list_string_to_list(self.emacs_var_dict["eaf-netease-cloud-music-current-song"])))

    def set_repeat_mode(self):
        self.buffer_widget.execute_js('''setRepeatMode(\"{}\")'''.format(
            self.emacs_var_dict["eaf-netease-cloud-music-repeat-mode"]))

    def change_song_style(self, index):
        if index == -1:
            self.buffer_widget.execute_js('''resetSongStyle()''')
        else:
            self.buffer_widget.execute_js('''changeSongStyle({})'''.format(index))
