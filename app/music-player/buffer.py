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

from PyQt5.QtGui import QColor
from PyQt5.QtCore import QUrl
from core.webengine import BrowserBuffer
from core.utils import interactive
import os
import glob
import json
import mimetypes
import taglib

class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, False)

        self.index_file_dir = os.path.join(os.path.dirname(__file__), "dist")
        self.index_file = os.path.join(self.index_file_dir, "index.html")
        self.url = url
        self.first_file = arguments

        self.panel_background_color = QColor(self.emacs_var_dict["eaf-emacs-theme-background-color"]).darker(110).name()

        self.buffer_widget.loadFinished.connect(self.load_first_file)

        with open(self.index_file, "r") as f:
            html = f.read().replace(
                '''href="''', '''href="''' + self.index_file_dir).replace(
                    '''<script src="''', '''<script src="''' + self.index_file_dir).replace(
                        '''<body>''', '''<body style="background: {}">'''.format(self.emacs_var_dict["eaf-emacs-theme-background-color"])
                    )
            self.buffer_widget.setHtml(html, QUrl("file://"))

        for (python_method_name, js_method_name) in [("play_next", "playNextItem"),
                                                     ("play_prev", "playPrevItem"),
                                                     ("forward", "forward"),
                                                     ("backward", "backward"),
                                                     ("toggle", "toggle")]:
            self.build_js_bridge_method(python_method_name, js_method_name)

    def load_first_file(self):
        self.buffer_widget.execute_js('''initPlaylistColor(\"{}\", \"{}\")'''.format(
            self.emacs_var_dict["eaf-emacs-theme-background-color"],
            self.emacs_var_dict["eaf-emacs-theme-foreground-color"]
        ))

        self.buffer_widget.execute_js('''initPanelColor(\"{}\", \"{}\")'''.format(
            self.panel_background_color,
            self.emacs_var_dict["eaf-emacs-theme-foreground-color"]
        ))

        files = []

        if os.path.isdir(self.first_file):
            files = list(filter(lambda f : os.path.isfile(f), glob.glob(os.path.join(self.first_file, "**"), recursive=True)))
        elif os.path.isfile(self.first_file):
            files.append(self.first_file)

        self.buffer_widget.execute_js('''addFiles({});'''.format(json.dumps(self.pick_music_info(files))))

    def pick_music_info(self, files):
        infos = []

        for file in files:
            file_type = mimetypes.guess_type(file)[0]
            if file_type and file_type.startswith("audio/"):
                tags = taglib.File(file).tags

                info = {
                    "name": os.path.splitext(os.path.basename(file))[0],
                    "path": file,
                    "artist": tags["ARTIST"][0].strip() if "ARTIST" in tags and len(tags["ARTIST"]) > 0 else "",
                    "album": tags["ALBUM"][0].strip() if "ALBUM" in tags and len(tags["ALBUM"]) > 0 else ""
                }
                infos.append(info)

        return infos
