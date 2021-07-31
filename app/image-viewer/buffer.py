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
from core.webengine import BrowserBuffer
from core.utils import interactive, message_to_emacs
from pathlib import Path
import os

class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, config_dir, arguments, module_path):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, module_path, False)

        self.index_file = os.path.join(os.path.dirname(__file__), "index.html")
        self.url = url
        self.parent_dir = os.path.abspath(os.path.join(url, os.pardir))
        self.image_name = os.path.basename(url)

        # Set background color.
        self.background_color_str = "#7F7F7F"
        self.buffer_widget.web_page.setBackgroundColor(QColor(self.background_color_str))

        with open(self.index_file, "r") as f:
            html = f.read().replace("%1", os.path.join(os.path.dirname(__file__))).replace("%2", os.path.join("file://", url)).replace("%3", self.background_color_str)
            self.buffer_widget.setHtml(html, QUrl("file://"))

    def load_image(self, url):
        self.url = url
        self.parent_dir = os.path.abspath(os.path.join(url, os.pardir))
        self.image_name = os.path.basename(url)

        load_image_js = "document.getElementById('image').setAttribute('src', '{0}');viewer.update();".format(
            os.path.join("file://", self.url).replace("\\", "/"))

        self.buffer_widget.eval_js(load_image_js)

    def is_image_file(self, f):
        return Path(f).suffix[1:].lower() in ["jpg", "jpeg", "png", "bmp", "gif", "svg", "webp"]

    def get_same_dir_images(self):
        files = [f for f in os.listdir(self.parent_dir) if os.path.isfile(os.path.join(self.parent_dir, f))]
        return list(filter(self.is_image_file, files))

    def load_next_image(self):
        images = self.get_same_dir_images()
        if self.image_name in images:
            image_index = images.index(self.image_name)
            if image_index == len(images) - 1:
                image_index = 0
            else:
                image_index += 1

            self.image_name = images[image_index]
            self.load_image(os.path.join(self.parent_dir, self.image_name))
            self.change_title(self.image_name)

    def load_prev_image(self):
        images = self.get_same_dir_images()
        if self.image_name in images:
            image_index = images.index(self.image_name)
            if image_index == 0:
                image_index = len(images) - 1
            else:
                image_index -= 1

            self.image_name = images[image_index]
            self.load_image(os.path.join(self.parent_dir, self.image_name))
            self.change_title(self.image_name)

    @interactive
    def rotate_left(self):
        self.buffer_widget.eval_js("rotate_left();")

    @interactive
    def rotate_right(self):
        self.buffer_widget.eval_js("rotate_right();")

    @interactive
    def zoom_out(self):
        self.buffer_widget.eval_js("zoom_out();")

    @interactive
    def zoom_in(self):
        self.buffer_widget.eval_js("zoom_in();")

    @interactive
    def zoom_reset(self):
        self.buffer_widget.eval_js("zoom_reset();")

    @interactive
    def zoom_toggle(self):
        self.buffer_widget.eval_js("zoom_toggle();")

    @interactive
    def flip_horizontal(self):
        self.buffer_widget.eval_js("flip_horizontal();")

    @interactive
    def flip_vertical(self):
        self.buffer_widget.eval_js("flip_vertical();")

    @interactive
    def move_up(self):
        self.buffer_widget.eval_js("move_up();")

    @interactive
    def move_down(self):
        self.buffer_widget.eval_js("move_down();")

    @interactive
    def move_left(self):
        self.buffer_widget.eval_js("move_left();")

    @interactive
    def move_right(self):
        self.buffer_widget.eval_js("move_right();")

    @interactive
    def delete_current_image(self):
        self.send_input_message("Are you sure you want to delete {0}?".format(self.image_name), "delete_image",  "yes-or-no")

    def handle_input_response(self, callback_tag, result_content):
        if callback_tag == "delete_image":
            image = self.url
            self.load_next_image()
            os.remove(image)
            message_to_emacs("Delete image {} success.".format(image))
