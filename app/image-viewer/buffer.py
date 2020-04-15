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
from core.utils import PostGui
from pathlib import Path
import os

class AppBuffer(BrowserBuffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path):
        BrowserBuffer.__init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, False)

        self.load_image(url)

    def load_image(self, url):
        self.url = url
        self.parent_dir = os.path.abspath(os.path.join(url, os.pardir))
        self.image_name = os.path.basename(url)
        self.buffer_widget.setUrl(QUrl.fromLocalFile(self.url))

    def is_image_file(self, f):
        return Path(f).suffix.lower() in [".jpg", ".jpeg", ".png", ".bmp", ".gif", ".svg", ".webp"]

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

            self.load_image(os.path.join(self.parent_dir, images[image_index]))

    def load_prev_image(self):
        images = self.get_same_dir_images()
        if self.image_name in images:
            image_index = images.index(self.image_name)
            if image_index == 0:
                image_index = len(images) - 1
            else:
                image_index -= 1

            self.load_image(os.path.join(self.parent_dir, images[image_index]))
