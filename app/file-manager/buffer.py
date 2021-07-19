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
from core.webengine import BrowserBuffer
from pathlib import Path
from functools import cmp_to_key
import os
import json

class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, False)

        self.index_file_dir = os.path.join(os.path.dirname(__file__), "dist")
        self.index_file = os.path.join(self.index_file_dir, "index.html")
        self.url = url

        with open(self.index_file, "r") as f:
            html = self.convert_index_html(f.read(), self.index_file_dir)
            self.buffer_widget.setHtml(html, QUrl("file://"))

        self.buffer_widget.loadFinished.connect(self.init_path)

    def init_path(self):
        path = os.path.expanduser("~")
        search_path = Path(path)

        file_infos = []
        for p in search_path.glob("*"):
            if not p.name.startswith("."):
                file_type = "file"

                if p.is_dir():
                    file_type = "directory"
                elif p.is_symlink():
                    file_type = "symlink"

                file_info = {
                    "path": str(p.absolute()),
                    "name": p.name,
                    "type": file_type
                }

                file_infos.append(file_info)

        file_infos.sort(key=cmp_to_key(self.file_compare))

        self.buffer_widget.execute_js('''addPath(\"{}\");'''.format(path))
        self.buffer_widget.execute_js('''addFiles({});'''.format(json.dumps(file_infos)))

    def file_compare(self, a, b):
        type_sort_weights = ["directory", "file", "symlink"]

        a_type_weights = type_sort_weights.index(a["type"])
        b_type_weights = type_sort_weights.index(b["type"])

        if a_type_weights == b_type_weights:
            if a["name"] < b["name"]:
                return -1
            elif a["name"] > b["name"]:
                return 1
            else:
                return 0
        else:
            return a_type_weights - b_type_weights
