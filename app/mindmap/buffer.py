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
from PyQt5.QtCore import QUrl, QTimer, QEvent, QPointF, Qt
from PyQt5.QtWidgets import QApplication
from PyQt5.QtGui import QColor, QMouseEvent
from core.browser import BrowserBuffer
from core.utils import touch, string_to_base64, interactive, eval_in_emacs, message_to_emacs
from html import escape, unescape
import os
import base64
import time

class AppBuffer(BrowserBuffer):

    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, False)

        self.url = url
        index_file = os.path.join(os.path.dirname(__file__), "index.html")
        self.buffer_widget.setUrl(QUrl.fromLocalFile(index_file))

        self.cut_node_id = None

        edit_mode = "true" if self.emacs_var_dict["eaf-mindmap-edit-mode"] == "true" else "false"
        for method_name in ["add_sub_node", "add_brother_node", "add_middle_node"]:
            self.build_js_method(method_name, True, js_kwargs={"inline": edit_mode})

        for method_name in ["remove_node", "remove_middle_node", "update_node_topic_inline"]:
            self.build_js_method(method_name, True)

        for method_name in ["zoom_in", "zoom_out", "zoom_reset",
                            "select_up_node", "select_down_node", "select_left_node", "select_right_node",
                            "toggle_node", "toggle_node_selection", "save_screenshot"]:
            self.build_js_method(method_name)

        for method_name in ["zoom_in", "zoom_out", "zoom_reset", "remove_node",
                            "remove_middle_node", "add_middle_node", "refresh_page",
                            "select_up_node", "select_down_node", "select_left_node", "select_right_node",
                            "toggle_node", "toggle_node_selection", "save_screenshot"]:
            self.build_insert_or_do(method_name)

        self.build_all_methods(self)

        self.buffer_widget.loadFinished.connect(lambda _: self.initialize())

    def resize_view(self):
        self.buffer_widget.eval_js("relayout();")

    def initialize(self):
        self.init_file()

        # The .jsmind-inner element is move right and bottom 30px,
        # so we must use a point greater than (30, 30), ex (100, 100).
        self.focus_widget(QMouseEvent(QEvent.MouseButtonPress, QPointF(100, 100), Qt.LeftButton, Qt.LeftButton, Qt.NoModifier))

    def init_file(self):
        self.url = os.path.expanduser(self.url)

        if os.path.exists(self.url):
            with open(self.url, "r") as f:
                _, ext = os.path.splitext(self.url)
                is_freemind = "true" if ext == ".mm" else "false"
                self.buffer_widget.execute_js("open_file('{}', {});".format(string_to_base64(f.read()), is_freemind))
        else:
            self.buffer_widget.eval_js("init_root_node();")

        QTimer.singleShot(200, lambda: self.buffer_widget.eval_js("select_root_node();"))

        color = "#FFFFFF"
        if self.emacs_var_dict["eaf-mindmap-dark-mode"] == "true" or \
           (self.emacs_var_dict["eaf-mindmap-dark-mode"] == "follow" and self.emacs_var_dict["eaf-emacs-theme-mode"] == "dark"):
            color = "#242525"
        self.buffer_widget.eval_js("init_background('{}');".format(color))

        self.change_title(self.get_title())

    def build_js_method(self, method_name, auto_save=False, js_kwargs=None):
        js_kwargs = js_kwargs or {}
        js_func_args = ", ".join('{}={}'.format(k, v) for k, v in js_kwargs.items())
        def _do ():
            self.buffer_widget.eval_js("{}({});".format(method_name, js_func_args))

            if auto_save:
                self.save_file(False)
        setattr(self, method_name, _do)

    @interactive
    def refresh_page(self):
        self.url = os.path.expanduser(self.url)

        if os.path.exists(self.url):
            with open(self.url, "r") as f:
                self.buffer_widget.execute_js("refresh('{}');".format(string_to_base64(f.read())))

            color = "#FFFFFF"
            if self.emacs_var_dict["eaf-mindmap-dark-mode"] == "true" or \
               (self.emacs_var_dict["eaf-mindmap-dark-mode"] == "follow" and self.emacs_var_dict["eaf-emacs-theme-mode"] == "dark"):
                color = "#242525"
            self.buffer_widget.eval_js("init_background('{}');".format(color))

            self.change_title(self.get_title())

    @interactive(insert_or_do=True)
    def change_background_color(self):
        self.send_input_message("Change node background color(Input color): ", "change_background_color")

    @interactive(insert_or_do=True)
    def change_text_color(self):
        self.send_input_message("Change node text color(Input color): ", "change_text_color")

    @interactive(insert_or_do=True)
    def copy_node_topic(self):
        node_topic = self.buffer_widget.execute_js("get_node_topic();")
        eval_in_emacs('kill-new', [node_topic])
        message_to_emacs("Copy: {}".format(node_topic))

    @interactive(insert_or_do=True)
    def paste_node_topic(self):
        text = self.get_clipboard_text()
        if text.strip() != "":
            self.buffer_widget.eval_js("update_node_topic('{}');".format(text))
            message_to_emacs("Paste: {}".format(text))

            self.save_file(False)
        else:
            message_to_emacs("Nothing in clipboard, can't paste.")

    @interactive(insert_or_do=True)
    def cut_node_tree(self):
        self.cut_node_id = self.buffer_widget.execute_js("get_selected_nodeid();")
        if self.cut_node_id:
            if self.cut_node_id != "root":
                message_to_emacs("Root node not allowed cut.")
            else:
                message_to_emacs("Cut node tree: {}".format(self.cut_node_id))

    @interactive(insert_or_do=True)
    def paste_node_tree(self):
        if self.cut_node_id:
            self.buffer_widget.eval_js("paste_node_tree('{}');".format(self.cut_node_id))
            self.save_file(False)
            message_to_emacs("Paste node tree: {}".format(self.cut_node_id))

    @interactive(insert_or_do=True)
    def change_node_background(self):
        self.send_input_message("Change node background: ", "change_node_background", "file")

    @interactive(insert_or_do=True)
    def update_node_topic(self):
        self.send_input_message(
            "Update topic: ",
            "update_node_topic",
            "string",
            unescape(self.buffer_widget.execute_js("get_node_topic();")))

    def handle_update_node_topic(self, topic):
        self.buffer_widget.eval_js("update_node_topic('{}');".format(escape(topic)))

        self.change_title(self.get_title())

        self.save_file(False)

    def handle_input_response(self, callback_tag, result_content):
        if callback_tag == "update_node_topic":
            self.handle_update_node_topic(str(result_content))
        elif callback_tag == "change_node_background":
            print(str(result_content))
            self.buffer_widget.eval_js("change_node_background('{}');".format(str(result_content)))
        elif callback_tag == "change_background_color":
            self.buffer_widget.eval_js("change_background_color('{}');".format(str(result_content)))
        elif callback_tag == "change_text_color":
            self.buffer_widget.eval_js("change_text_color('{}');".format(str(result_content)))

    def add_multiple_sub_nodes(self):
        node_id = self.buffer_widget.execute_js("_jm.get_selected_node();")
        if node_id != None:
            eval_in_emacs('eaf--add-multiple-sub-nodes', [self.buffer_id])
        else:
            message_to_emacs("No selected node.")

    def add_multiple_brother_nodes(self):
        node_id = self.buffer_widget.execute_js("_jm.get_selected_node();")
        if node_id == None:
            message_to_emacs("No selected node.")
        elif not self.buffer_widget.execute_js("_jm.get_selected_node().parent;"):
            message_to_emacs("No parent node for selected node.")
        else:
            eval_in_emacs('eaf--add-multiple-brother-nodes', [self.buffer_id])

    def add_multiple_middle_nodes(self):
        node_id = self.buffer_widget.execute_js("_jm.get_selected_node();")
        if node_id == None:
            message_to_emacs("No selected node.")
        elif not self.buffer_widget.execute_js("_jm.get_selected_node().parent;"):
            message_to_emacs("No parent node for selected node.")
        else:
            eval_in_emacs('eaf--add-multiple-middle-nodes', [self.buffer_id])

    @interactive
    def add_texted_sub_node(self,text):
        self.buffer_widget.eval_js("add_texted_sub_node('{}');".format(str(text)))

    @interactive
    def add_texted_brother_node(self,text):
        self.buffer_widget.eval_js("add_texted_brother_node('{}');".format(str(text)))

    @interactive
    def add_texted_middle_node(self,text):
        self.buffer_widget.eval_js("add_texted_middle_node('{}');".format(str(text)))

    def is_focus(self):
        return self.buffer_widget.execute_js("node_is_focus();")

    def get_title(self):
        return os.path.basename(self.url) or self.get_root_node_topic()

    def get_root_node_topic(self):
        return self.buffer_widget.execute_js("get_root_node_topic();")

    def handle_download_request(self, download_item):
        download_data = download_item.url().toString()
        if self.should_skip_download_item(download_item) or not download_data.startswith("data:image/"):
            return

        # Note:
        # Set some delay to make get_root_node_topic works expect.
        # get_root_node_topic will return None if execute immediately.
        QTimer.singleShot(200, lambda : self.save_screenshot_data(download_data))

    def get_save_path(self, extension_name):
        if self.url.strip() == "":
            return os.path.join(os.path.expanduser(self.emacs_var_dict["eaf-mindmap-save-path"]), self.get_root_node_topic().replace(" ", "_") + time.strftime("_%Y%m%d_%H%M%S", time.localtime(int(time.time()))) + "." + extension_name)
        else:
            return os.path.splitext(self.url)[0] + "." + extension_name

    def save_screenshot_data(self, download_data):
        image_path = self.get_save_path("png")
        touch(image_path)
        with open(image_path, "wb") as f:
            f.write(base64.decodestring(download_data.split("data:image/png;base64,")[1].encode("utf-8")))

        message_to_emacs("Save image: " + image_path)

    @interactive(insert_or_do=True)
    def save_file(self, notify=True):
        file_path = self.get_save_path("emm")
        with open(file_path, "w") as f:
            f.write(self.buffer_widget.execute_js("save_file();"))

        if notify:
            message_to_emacs("Save file: " + file_path)

    @interactive(insert_or_do=True)
    def save_org_file(self):
        file_path = self.get_save_path("org")
        touch(file_path)
        eval_in_emacs('eaf--export-org-json', [self.buffer_widget.execute_js("save_file();"), file_path])
        message_to_emacs("Save org file: " + file_path)

    @interactive(insert_or_do=True)
    def save_freemind_file(self, notify=True):
        file_path = self.get_save_path("mm")
        with open(file_path, "w") as f:
            f.write(self.buffer_widget.execute_js("save_freemind_file();"))

        if notify:
            message_to_emacs("Save freemind file: " + file_path)
