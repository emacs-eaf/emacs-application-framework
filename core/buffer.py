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
from PyQt5.QtGui import QBrush, QColor
from PyQt5.QtWidgets import QGraphicsScene, QApplication
import abc
from core.utils import PostGui

class Buffer(QGraphicsScene):
    __metaclass__ = abc.ABCMeta

    update_title = QtCore.pyqtSignal(str, str)
    open_url = QtCore.pyqtSignal(str)
    before_destroy_hook = QtCore.pyqtSignal()
    input_message = QtCore.pyqtSignal(str, str, str)
    close_buffer = QtCore.pyqtSignal(str)
    message_to_emacs = QtCore.pyqtSignal(str)

    def __init__(self, buffer_id, url, arguments, fit_to_view, background_color):
        super(QGraphicsScene, self).__init__()

        self.buffer_id = buffer_id
        self.url = url
        self.arguments = arguments
        self.fit_to_view = fit_to_view
        self.background_color = background_color
        self.setBackgroundBrush(QBrush(self.background_color))

        self.buffer_widget = None

        self.draw_progressbar = False
        self.progressbar_progress = 0
        self.progressbar_color = QColor(233, 129, 35, 255)
        self.progressbar_height = 2

    def drawForeground(self, painter, rect):
        if self.draw_progressbar:
            painter.setBrush(self.progressbar_color)
            painter.setPen(self.progressbar_color)
            painter.drawRect(0, 0, rect.width() * self.progressbar_progress * 1.0 / 100, self.progressbar_height)

    @QtCore.pyqtSlot()
    def start_progress(self):
        self.progressbar_progress = 0
        self.draw_progressbar = True
        self.update()

    @QtCore.pyqtSlot()
    def stop_progress(self):
        self.draw_progressbar = False
        self.update()

    @QtCore.pyqtSlot(int)
    def update_progress(self, progress):
        self.progressbar_progress = progress
        self.draw_progressbar = True
        self.update()

        if progress == 100:
            QtCore.QTimer.singleShot(500, self.stop_progress)

    def add_widget(self, widget):
        self.buffer_widget = widget
        self.addWidget(self.buffer_widget)

        self.buffer_widget.installEventFilter(self)

        self.buffer_widget.message_to_emacs = self.message_to_emacs

    def handle_destroy(self):
        self.before_destroy_hook.emit()

        if self.buffer_widget != None:
            self.buffer_widget.destroy()

    def change_title(self, title):
        self.update_title.emit(self.buffer_id, title)

    def request_close_buffer(self):
        self.close_buffer.emit(self.buffer_id)

    def all_views_hide(self):
        pass

    def some_view_show(self):
        pass

    def get_key_event_widgets(self):
        return [self.buffer_widget]

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
