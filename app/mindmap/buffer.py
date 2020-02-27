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

from PyQt5.QtCore import QUrl, QTimer
from PyQt5.QtGui import QColor
from core.browser import BrowserBuffer
from core.utils import touch, string_to_base64
import os
import base64

class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, False, QColor(255, 255, 255, 255))

        self.url = url
        index_file = "file://" + (os.path.join(os.path.dirname(__file__), "index.html"))
        self.buffer_widget.setUrl(QUrl(index_file))

        for method_name in ["zoom_in", "zoom_out", "zoom_reset", "add_sub_node", "remove_node",
                            "select_up_node", "select_down_node", "select_left_node", "select_right_node",
                            "toggle_node", "save_screenshot"]:
            self.build_js_method(method_name)

        for method_name in ["zoom_in", "zoom_out", "zoom_reset", "remove_node", "update_node_topic", "refresh_page",
                            "select_up_node", "select_down_node", "select_left_node", "select_right_node",
                            "toggle_node", "save_screenshot", "save_file", "change_node_background"]:
            self.build_insert_or_do(method_name)

        QTimer.singleShot(500, self.init_file)

    def init_file(self):
        self.url = os.path.expanduser(self.url)

        if os.path.exists(self.url):
            with open(self.url, "r") as f:
                self.buffer_widget.execute_js("open_file('{}');".format(string_to_base64(f.read())))
        else:
            self.buffer_widget.eval_js("init_root_node();")

        self.change_title(self.get_root_node_topic())

    def build_js_method(self, method_name):
        def _do ():
            self.buffer_widget.eval_js("{}();".format(method_name))
        setattr(self, method_name, _do)

    def change_node_background(self):
        self.send_input_message("Change node background: ", "change_node_background", "file")

    def update_node_topic(self):
        self.send_input_message("Update topic: ", "update_node_topic")

    def handle_update_node_topic(self, topic):
        self.buffer_widget.eval_js("update_node_topic('{}');".format(topic))

    def handle_input_message(self, result_type, result_content):
        if result_type == "update_node_topic":
            self.handle_update_node_topic(str(result_content))
        elif result_type == "change_node_background":
            print(str(result_content))
            self.buffer_widget.eval_js("change_node_background('{}');".format(str(result_content)))

    def is_focus(self):
        return self.buffer_widget.execute_js("node_is_focus();")

    def build_insert_or_do(self, method_name):
        def _do ():
            if self.is_focus():
                self.fake_key_event(self.current_event_string)
            else:
                getattr(self, method_name)()
        setattr(self, "insert_or_{}".format(method_name), _do)

    def get_root_node_topic(self):
        return self.buffer_widget.execute_js("get_root_node_topic();")

    def handle_download_request(self, download_item):
        download_data = download_item.url().toString()

        # Note:
        # Set some delay to make get_root_node_topic works expect.
        # get_root_node_topic will return None if execute immediately.
        QTimer.singleShot(200, lambda : self.save_screenshot_data(download_data))

    def save_screenshot_data(self, download_data):
        image_path = os.path.join(os.path.expanduser(self.emacs_var_dict["eaf-mindmap-save-path"]), self.get_root_node_topic() + ".png")
        touch(image_path)
        with open(image_path, "wb") as f:
            f.write(base64.decodestring(download_data.split("data:image/png;base64,")[1].encode("utf-8")))

        self.message_to_emacs.emit("Save image: " + image_path)

    def save_file(self):
        file_path = os.path.join(os.path.expanduser(self.emacs_var_dict["eaf-mindmap-save-path"]), self.get_root_node_topic() + ".emm")
        touch(file_path)
        with open(file_path, "w") as f:
            f.write(self.buffer_widget.execute_js("save_file();"))
        self.message_to_emacs.emit("Save file: " + file_path)
