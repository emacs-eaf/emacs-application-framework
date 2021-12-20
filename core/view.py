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
from PyQt5.QtCore import Qt, QEvent, QPoint
from PyQt5.QtGui import QPainter, QWindow, QBrush, QColor
from PyQt5.QtWidgets import QWidget, QVBoxLayout, QGraphicsView, QFrame
from core.utils import eval_in_emacs, focus_emacs_buffer
import platform
import os

class View(QWidget):

    def __init__(self, buffer, view_info):
        super(View, self).__init__()

        self.buffer = buffer

        # Init widget attributes.
        if platform.system() == "Darwin":
            self.setWindowFlags(Qt.FramelessWindowHint | Qt.WindowStaysOnTopHint | Qt.NoDropShadowWindowHint)
        else:
            self.setWindowFlags(Qt.FramelessWindowHint)

        self.setAttribute(Qt.WA_X11DoNotAcceptFocus, True)
        self.setContentsMargins(0, 0, 0, 0)
        self.installEventFilter(self)

        # Init attributes.
        self.view_info = view_info
        (self.buffer_id, self.emacs_xid, self.x, self.y, self.width, self.height) = view_info.split(":")
        self.emacs_xid = int(self.emacs_xid)
        self.x = int(self.x)
        self.y = int(self.y)
        self.width = int(self.width)
        self.height = int(self.height)

        # Build QGraphicsView.
        self.layout = QVBoxLayout(self)
        self.layout.setSpacing(0)
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.graphics_view = QGraphicsView(buffer, self)

        # Remove border from QGraphicsView.
        self.graphics_view.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.graphics_view.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.graphics_view.setRenderHints(QPainter.Antialiasing | QPainter.SmoothPixmapTransform | QPainter.TextAntialiasing)
        self.graphics_view.setFrameStyle(QFrame.NoFrame)

        # Fill background color.
        self.graphics_view.setBackgroundBrush(QBrush(buffer.background_color))

        # Add graphics view.
        self.layout.addWidget(self.graphics_view)

        # NOTE: show function must start before resize to trigger *first* resizeEvent after show.
        self.show()

        # Resize after show to trigger fit view operation.
        self.resize(self.width, self.height)

        self.buffer.aspect_ratio_change.connect(self.adjust_aspect_ratio)

    def resizeEvent(self, event):
        # Fit content to view rect just when buffer fit_to_view option is enable.
        if self.buffer.fit_to_view:
            if event.oldSize().isValid():
                self.graphics_view.fitInView(self.graphics_view.scene().sceneRect(), Qt.KeepAspectRatio)
                QWidget.resizeEvent(self, event)

    def adjust_aspect_ratio(self):
        widget_width = self.width
        widget_height = self.height

        if self.buffer.aspect_ratio == 0:
            self.buffer.buffer_widget.resize(self.width, self.height)

            self.layout.setContentsMargins(0, 0, 0, 0)
        else:
            view_height = widget_height * (1 - 2 * self.buffer.vertical_padding_ratio)
            view_width = view_height * self.buffer.aspect_ratio
            horizontal_padding = (widget_width - view_width) / 2
            vertical_padding = self.buffer.vertical_padding_ratio * widget_height

            self.buffer.buffer_widget.resize(view_width, view_height)

            self.layout.setContentsMargins(
                horizontal_padding, vertical_padding,
                horizontal_padding, vertical_padding)

    def eventFilter(self, obj, event):
        # import time
        # print(time.time(), event.type())

        if event.type() in [QEvent.ShortcutOverride]:
            eval_in_emacs('eaf-activate-emacs-window', [self.buffer_id])

        # Focus emacs buffer when user click view.
        event_type = [QEvent.MouseButtonPress, QEvent.MouseButtonRelease, QEvent.MouseButtonDblClick]
        if platform.system() != "Darwin":
            event_type += [QEvent.Wheel]

        if event.type() in event_type:
            focus_emacs_buffer(self.buffer_id)
            # Stop mouse event.
            return True

        return False

    def showEvent(self, event):
        # NOTE: we must reparent after widget show, otherwise reparent operation maybe failed.
        self.reparent()

        if platform.system() in ["Windows", "Darwin"]:
            eval_in_emacs('eaf-activate-emacs-window', [])

        # Make graphics view at left-top corner after show.
        self.graphics_view.verticalScrollBar().setValue(0)
        self.graphics_view.horizontalScrollBar().setValue(0)

    def reparent(self):
        # print("Reparent: ", self.buffer.url)
        qwindow = self.windowHandle()

        if platform.system() != "Darwin":
            qwindow.setParent(QWindow.fromWinId(self.emacs_xid))

        qwindow.setPosition(QPoint(self.x, self.y))

    def mac_handle_emacs_focus_in(self):
        self.setWindowFlag(Qt.WindowStaysOnTopHint, True)
        self.show()

    def mac_handle_emacs_focus_out(self):
        self.setWindowFlag(Qt.WindowStaysOnTopHint, False)
        self.hide()

    def destroy_view(self):
        # print("Destroy: ", self.buffer.url)
        self.destroy()
