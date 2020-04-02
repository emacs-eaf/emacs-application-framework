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

from PyQt5 import QtCore
from PyQt5.QtCore import QUrl, QTimer
from PyQt5.QtWidgets import QApplication
from PyQt5.QtGui import QColor
from core.browser import BrowserBuffer
from core.utils import touch, string_to_base64
import os
import base64

class AppBuffer(BrowserBuffer):

    export_org_json = QtCore.pyqtSignal(str, str)

    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, call_emacs):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, call_emacs, False, QColor(255, 255, 255, 255))

        self.url = url
        index_file = "file://" + (os.path.join(os.path.dirname(__file__), "index.html"))
        self.buffer_widget.setUrl(QUrl(index_file))

        self.cut_node_id = None

        for method_name in ["add_sub_node", "add_brother_node", "remove_node", "remove_middle_node", "add_middle_node", "update_node_topic_inline"]:
            self.build_js_method(method_name, True)

        for method_name in ["zoom_in", "zoom_out", "zoom_reset",
                            "select_up_node", "select_down_node", "select_left_node", "select_right_node",
                            "toggle_node", "save_screenshot"]:
            self.build_js_method(method_name)

        for method_name in ["zoom_in", "zoom_out", "zoom_reset", "remove_node",
                            "remove_middle_node", "add_middle_node", "update_node_topic",
                            "copy_node_topic", "paste_node_topic", "refresh_page",
                            "select_up_node", "select_down_node", "select_left_node", "select_right_node",
                            "toggle_node", "save_screenshot", "save_file", "save_org_file",
                            "change_node_background", "cut_node_tree", "paste_node_tree"]:
            self.build_insert_or_do(method_name)

        QTimer.singleShot(500, self.init_file)

    def resize_view(self):
        self.buffer_widget.eval_js("relayout();")

    def init_file(self):
        self.url = os.path.expanduser(self.url)

        if os.path.exists(self.url):
            with open(self.url, "r") as f:
                self.buffer_widget.execute_js("open_file('{}');".format(string_to_base64(f.read())))
        else:
            self.buffer_widget.eval_js("init_root_node();")

        color = "#FFFFFF"
        if self.emacs_var_dict["eaf-mindmap-dark-mode"] == "true" or \
           (self.emacs_var_dict["eaf-mindmap-dark-mode"] == "" and self.call_emacs("GetThemeMode") == "dark"):
            color = "#242525"
        self.buffer_widget.eval_js("init_background('{}');".format(color))

        self.change_title(self.get_root_node_topic())

    def build_js_method(self, method_name, auto_save=False):
        def _do ():
            self.buffer_widget.eval_js("{}();".format(method_name))

            if auto_save:
                self.save_file(False)
        setattr(self, method_name, _do)

    def build_insert_or_do(self, method_name):
        def _do ():
            if self.is_focus():
                self.fake_key_event(self.current_event_string)
            else:
                getattr(self, method_name)()
        setattr(self, "insert_or_{}".format(method_name), _do)

    def copy_node_topic(self):
        node_topic = self.buffer_widget.execute_js("get_node_topic();")
        self.eval_in_emacs.emit('''(kill-new "{}")'''.format(node_topic))
        self.message_to_emacs.emit("Copy: {}".format(node_topic))

    def paste_node_topic(self):
        text = QApplication.clipboard().text()
        if text.strip() != "":
            self.buffer_widget.eval_js("update_node_topic('{}');".format(text))
            self.message_to_emacs.emit("Paste: {}".format(text))

            self.save_file(False)
        else:
            self.message_to_emacs.emit("Nothing in clipboard, can't paste.")

    def cut_node_tree(self):
        self.cut_node_id = self.buffer_widget.execute_js("get_selected_nodeid();")
        if self.cut_node_id:
            if self.cut_node_id != "root":
                self.message_to_emacs.emit("Root node not allowed cut.")
            else:
                self.message_to_emacs.emit("Cut node tree: {}".format(self.cut_node_id))

    def paste_node_tree(self):
        if self.cut_node_id:
            self.buffer_widget.eval_js("paste_node_tree('{}');".format(self.cut_node_id))
            self.save_file(False)
            self.message_to_emacs.emit("Paste node tree: {}".format(self.cut_node_id))

    def change_node_background(self):
        self.send_input_message("Change node background: ", "change_node_background", "file")

    def update_node_topic(self):
        self.send_input_message(
            "Update topic: ",
            "update_node_topic",
            "string",
            self.buffer_widget.execute_js("get_node_topic();"))

    def handle_update_node_topic(self, topic):
        self.buffer_widget.eval_js("update_node_topic('{}');".format(topic))

        self.change_title(self.get_root_node_topic())

        self.save_file(False)

    def handle_input_message(self, result_type, result_content):
        if result_type == "update_node_topic":
            self.handle_update_node_topic(str(result_content))
        elif result_type == "change_node_background":
            print(str(result_content))
            self.buffer_widget.eval_js("change_node_background('{}');".format(str(result_content)))

    def is_focus(self):
        return self.buffer_widget.execute_js("node_is_focus();")

    def get_root_node_topic(self):
        return self.buffer_widget.execute_js("get_root_node_topic();")

    def handle_download_request(self, download_item):
        download_data = download_item.url().toString()

        # Note:
        # Set some delay to make get_root_node_topic works expect.
        # get_root_node_topic will return None if execute immediately.
        QTimer.singleShot(200, lambda : self.save_screenshot_data(download_data))

    def get_save_path(self, extension_name):
        if self.url.strip() == "":
            return os.path.join(os.path.expanduser(self.emacs_var_dict["eaf-mindmap-save-path"]), self.get_root_node_topic() + "." + extension_name)
        else:
            return os.path.splitext(self.url)[0] + "." + extension_name

    def save_screenshot_data(self, download_data):
        image_path = self.get_save_path("png")
        touch(image_path)
        with open(image_path, "wb") as f:
            f.write(base64.decodestring(download_data.split("data:image/png;base64,")[1].encode("utf-8")))

        self.message_to_emacs.emit("Save image: " + image_path)

    def save_file(self, notify=True):
        file_path = self.get_save_path("emm")
        touch(file_path)
        with open(file_path, "w") as f:
            f.write(self.buffer_widget.execute_js("save_file();"))

        if notify:
            self.message_to_emacs.emit("Save file: " + file_path)

    def save_org_file(self):
        file_path = self.get_save_path("org")
        touch(file_path)
        self.export_org_json.emit(self.buffer_widget.execute_js("save_file();"), file_path)
        self.message_to_emacs.emit("Save org file: " + file_path)
