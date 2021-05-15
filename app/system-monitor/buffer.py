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
from PyQt5.QtCore import QUrl, QTimer
from core.webengine import BrowserBuffer
from core.utils import interactive
import os
import json
import psutil

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

        for (python_method_name, js_method_name) in [("scroll_up", "scrollUp"),
                                                     ("scroll_down", "scrollDown"),
                                                     ("scroll_up_page", "scrollUpPage"),
                                                     ("scroll_down_page", "scrollDownPage"),
                                                     ("scroll_to_begin", "scrollToBegin"),
                                                     ("scroll_to_bottom", "scrollToBottom")
                                                     ]:
            self.build_js_bridge_method(python_method_name, js_method_name)

    def load_first_file(self):
        self.buffer_widget.execute_js('''initProcesslistColor(\"{}\", \"{}\")'''.format(
            self.emacs_var_dict["eaf-emacs-theme-background-color"],
            self.emacs_var_dict["eaf-emacs-theme-foreground-color"]
        ))

        self.buffer_widget.execute_js('''initPanelColor(\"{}\", \"{}\")'''.format(
            self.panel_background_color,
            self.emacs_var_dict["eaf-emacs-theme-foreground-color"]
        ))

        self.update_process_info()

        self.timer = QTimer(self)
        self.timer.timeout.connect(self.update_process_info)
        self.timer.start(2000)

    def update_process_info(self):
        infos = []

        for proc in psutil.process_iter(['cpu_percent', 'pid', 'name', 'username', 'cmdline']):
            info = proc.info
            info["memory"] = self.format_memory(psutil.Process(info["pid"]).memory_info().rss)
            info["cmdline"] = " ".join(info["cmdline"])
            infos.append(proc.info)

        infos.sort(key=lambda info: info["cpu_percent"], reverse=True)

        self.buffer_widget.execute_js('''updateProcessInfo({});'''.format(json.dumps(infos)))

        mem = psutil.virtual_memory()
        panel_info = {
            "memory": {
                "total": self.format_memory(mem.total),
                "used": self.format_memory(mem.used),
                "percent": mem.percent
            }
        }
        print(mem)

        self.buffer_widget.execute_js('''updatePanelInfo({});'''.format(json.dumps(panel_info)))

    def format_memory(self, memory):
        if memory < 1024:
            return str(memory) + "B"
        elif memory > 1024 * 1024 * 1024:
            return "{:.1f}".format(memory / 1024 / 1024 / 1024) + "GB"
        elif memory > 1024 * 1024:
            return "{:.1f}".format(memory / 1024 / 1024) + "MB"
        else:
            return "{:.1f}".format(memory / 1024) + "KB"

    def destroy_buffer(self):
        self.close_buffer()
        self.timer.stop()
