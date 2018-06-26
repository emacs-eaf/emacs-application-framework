#!/usr/bin/env python
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
from PyQt5.QtGui import QBrush
from PyQt5.QtWidgets import QGraphicsScene
import abc

class Buffer(QGraphicsScene):
    __metaclass__ = abc.ABCMeta

    update_title = QtCore.pyqtSignal(str, str)
    open_url = QtCore.pyqtSignal(str)
    before_destroy_hook = QtCore.pyqtSignal()

    def __init__(self, buffer_id, url, width, height, background_color):
        super(QGraphicsScene, self).__init__()

        self.width = width
        self.height = height

        self.buffer_id = buffer_id
        self.url = url

        self.buffer_widget = None
        self.background_color = background_color

        self.setBackgroundBrush(QBrush(self.background_color))

        self.fit_to_view = True

    def add_widget(self, widget):
        self.buffer_widget = widget
        self.addWidget(self.buffer_widget)

    def resize_buffer(self, width, height):
        pass

    def handle_destroy(self):
        self.before_destroy_hook.emit()

        if self.buffer_widget != None:
            self.buffer_widget.destroy()

    def change_title(self, title):
        self.update_title.emit(self.buffer_id, title)
