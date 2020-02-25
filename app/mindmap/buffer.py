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
from PyQt5.QtGui import QColor
from core.browser import BrowserBuffer
import os

class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, False, QColor(255, 255, 255, 255))

        self.url = "file://" + (os.path.join(os.path.dirname(__file__), "index.html"))
        self.buffer_widget.setUrl(QUrl(self.url))

    def add_sub_node(self):
        self.buffer_widget.eval_js("add_node();")

    def remove_node(self):
        self.buffer_widget.eval_js("remove_node();")

    def update_node_topic(self):
        self.send_input_message("Update topic: ", "update_node_topic")

    def handle_update_node_topic(self, topic):
        self.buffer_widget.eval_js("update_node_topic('{}');".format(topic))

    def handle_input_message(self, result_type, result_content):
        if result_type == "update_node_topic":
            self.handle_update_node_topic(str(result_content))
