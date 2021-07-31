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
from functools import cmp_to_key
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

        self.panel_background_color = QColor(get_emacs_var("eaf-emacs-theme-background-color")).darker(110).name()

        self.buffer_widget.loadFinished.connect(self.load_first_file)

        with open(self.index_file, "r") as f:
            html = self.convert_index_html(f.read(), self.index_file_dir)
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
            get_emacs_var("eaf-emacs-theme-background-color"),
            get_emacs_var("eaf-emacs-theme-foreground-color")
        ))

        self.buffer_widget.execute_js('''initPanelColor(\"{}\", \"{}\")'''.format(
            self.panel_background_color,
            get_emacs_var("eaf-emacs-theme-foreground-color")
        ))

        self.update_process_info()

        self.timer = QTimer(self)
        self.timer.timeout.connect(self.update_process_info)
        self.timer.start(1000)

    def update_process_info(self):
        infos = []

        for proc in psutil.process_iter(['cpu_percent', 'memory_info', 'pid', 'name', 'username', 'cmdline']):
            info = proc.info
            memory_number = info["memory_info"].rss
            info["memory_number"] = memory_number
            info["memory"] = self.format_memory(memory_number)
            info["cmdline"] = " ".join(info["cmdline"])
            infos.append(proc.info)

        infos.sort(key=cmp_to_key(self.process_compare), reverse=True)

        self.buffer_widget.execute_js('''updateProcessInfo({});'''.format(json.dumps(infos)))

        mem = psutil.virtual_memory()
        cpu_percent = psutil.cpu_percent()
        cpu_percents = psutil.cpu_percent(percpu=True)
        cpu_count = psutil.cpu_count()
        panel_info = {
            "cpu": {
                "count": cpu_count,
                "percent": cpu_percent,
                "percents": cpu_percents
            },
            "memory": {
                "total": self.format_memory(mem.total),
                "used": self.format_memory(mem.used),
                "percent": mem.percent
            }
        }

        self.buffer_widget.execute_js('''updatePanelInfo({});'''.format(json.dumps(panel_info)))

    def process_compare(self, a, b):
        if a["cpu_percent"] < b["cpu_percent"]:
            return -1
        elif a["cpu_percent"] > b["cpu_percent"]:
            return 1
        else:
            if a["memory_number"] < b["memory_number"]:
                return -1
            elif a["memory_number"] > b["memory_number"]:
                return 1
            else:
                return 0

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
