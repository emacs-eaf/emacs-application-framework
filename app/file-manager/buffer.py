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

from PyQt5.QtCore import QUrl, QThread
from PyQt5 import QtCore
from core.webengine import BrowserBuffer
from core.utils import get_emacs_var
from pathlib import Path
from functools import cmp_to_key
from core.utils import eval_in_emacs, PostGui
import magic
import os
import json

class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, arguments):
        BrowserBuffer.__init__(self, buffer_id, url, arguments, False)

        self.index_file_dir = os.path.join(os.path.dirname(__file__), "dist")
        self.index_file = os.path.join(self.index_file_dir, "index.html")
        self.url = url
        self.arguments = json.loads(arguments)

        with open(self.index_file, "r") as f:
            html = self.convert_index_html(f.read(), self.index_file_dir)
            self.buffer_widget.setHtml(html, QUrl("file://"))

        self.buffer_widget.loadFinished.connect(self.init_path)

        for (python_method_name, js_method_name) in [("select_next_file", "selectNextFile"),
                                                     ("select_prev_file", "selectPrevFile"),
                                                     ("open_file", "openFile"),
                                                     ("up_directory", "upDirectory"),
                                                     ]:
            self.build_js_bridge_method(python_method_name, js_method_name)

        self.fetch_preview_info_thread = None

    def init_path(self):
        self.buffer_widget.execute_js('''initColors(\"{}\", \"{}\", \"{}\", \"{}\", \"{}\", \"{}\", \"{}\")'''.format(
            get_emacs_var("eaf-emacs-theme-background-color"),
            get_emacs_var("eaf-emacs-theme-foreground-color"),
            self.arguments["header-color"],
            get_emacs_var("eaf-emacs-theme-foreground-color"),
            self.arguments["directory-color"],
            self.arguments["symlink-color"],
            self.arguments["select-color"],
        ))

        self.change_directory(self.url, "")

    def get_files(self, path):
        self.url = os.path.expanduser(path)
        search_path = Path(self.url)

        file_infos = []
        for p in search_path.glob("*"):
            if not p.name.startswith("."):
                file_type = ""
                file_size = ""

                if p.is_file():
                    file_type = "file"
                    file_size = self.file_size_format(os.path.getsize(p.absolute()))
                elif p.is_dir():
                    file_type = "directory"
                    file_size = str(self.get_dir_file_number(p.absolute()))
                elif p.is_symlink():
                    file_type = "symlink"
                    file_size = "1"

                file_info = {
                    "path": str(p.absolute()),
                    "name": p.name,
                    "type": file_type,
                    "size": file_size
                }

                file_infos.append(file_info)

        file_infos.sort(key=cmp_to_key(self.file_compare))

        return file_infos

    def file_size_format(self, num, suffix='B'):
        for unit in ['','K','M','G','T','P','E','Z']:
            if abs(num) < 1024.0:
                return "%3.1f%s%s" % (num, unit, suffix)
            num /= 1024.0
        return "%.1f%s%s" % (num, 'Yi', suffix)

    def get_dir_file_number(self, dir):
        return len(list(filter(lambda f: not f.startswith("."), (os.listdir(dir)))))

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

    @QtCore.pyqtSlot(str)
    def open_file(self, file):
        eval_in_emacs("find-file", [file])

    @QtCore.pyqtSlot(str, str)
    def change_directory(self, dir, current_dir):
        file_infos = self.get_files(dir)
        select_index = 0

        if current_dir != "":
            files = list(map(lambda file: file["path"], file_infos))
            select_index = files.index(current_dir)

        self.buffer_widget.execute_js('''changePath(\"{}\", {}, {});'''.format(
            self.url,
            json.dumps(file_infos),
            select_index))

        if file_infos == []:
            self.update_preview("")
        else:
            self.update_preview(file_infos[select_index]["path"])

    @QtCore.pyqtSlot(str)
    def change_up_directory(self, file):
        current_dir = os.path.dirname(file)
        up_directory_path = Path(current_dir).parent.absolute()
        if up_directory_path != current_dir:
            self.change_directory(up_directory_path, current_dir)
        else:
            eval_in_emacs("message", ["Already in root directory"])

    @QtCore.pyqtSlot(str)
    def update_preview(self, file):
        self.exit_preview_thread()

        self.fetch_preview_info_thread = FetchPreviewInfoThread(file, self.get_files)
        self.fetch_preview_info_thread.fetch_finish.connect(self.update_preview_info)
        self.fetch_preview_info_thread.start()

    def exit_preview_thread(self):
        if self.fetch_preview_info_thread != None and self.fetch_preview_info_thread.isRunning():
            self.fetch_preview_info_thread.exit()

    def update_preview_info(self, file, file_type, file_infos):
        self.buffer_widget.execute_js('''setPreview(\"{}\", \"{}\", {});'''.format(file, file_type, file_infos))

    def destroy_buffer(self):
        self.exit_preview_thread()

class FetchPreviewInfoThread(QThread):

    fetch_finish = QtCore.pyqtSignal(str, str, str)

    def __init__(self, file, get_files_callback):
        QThread.__init__(self)

        self.file = file
        self.get_files_callback = get_files_callback

    def run(self):
        path = ""
        if self.file != "":
            path = Path(self.file)

        file_type = ""
        file_infos = []

        if path.is_file():
            file_type = "file"
            file_infos = [{
                "mime": magic.Magic(mime=True).from_file(str(path.absolute()))
            }]
        elif path.is_dir():
            file_type = "directory"
            file_infos = self.get_files_callback(self.file)
        elif path.is_symlink():
            file_type = "symlink"

        self.fetch_finish.emit(self.file, file_type, json.dumps(file_infos))
