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
from PyQt5.QtGui import QBrush
from PyQt5.QtWidgets import QGraphicsScene, QApplication
import abc

class Buffer(QGraphicsScene):
    __metaclass__ = abc.ABCMeta

    update_title = QtCore.pyqtSignal(str, str)
    open_url = QtCore.pyqtSignal(str)
    before_destroy_hook = QtCore.pyqtSignal()
    input_message = QtCore.pyqtSignal(str, str, str)

    def __init__(self, buffer_id, url, fit_to_view, background_color):
        super(QGraphicsScene, self).__init__()

        self.buffer_id = buffer_id
        self.url = url
        self.fit_to_view = fit_to_view
        self.background_color = background_color
        self.setBackgroundBrush(QBrush(self.background_color))

        self.buffer_widget = None

    def add_widget(self, widget):
        self.buffer_widget = widget
        self.addWidget(self.buffer_widget)

    def handle_destroy(self):
        self.before_destroy_hook.emit()

        if self.buffer_widget != None:
            self.buffer_widget.destroy()

    def change_title(self, title):
        self.update_title.emit(self.buffer_id, title)

    def all_views_hide(self):
        pass

    def some_view_show(self):
        pass

    def send_key_event(self, event):
        QApplication.sendEvent(self.buffer_widget, event)

    def send_key_event(self, keystroke):
        pass

    def send_input_message(self, message, callback_type):
        self.input_message.emit(self.buffer_id, message, callback_type)

    def handle_input_message(self, result_type, result_content):
        pass

    def scroll(self, scroll_direction, scroll_type):
        pass

    def save_session_data(self):
        return ""

    def restore_session_data(self, session_data):
        pass

    def update_with_data(self, update_data):
        pass
