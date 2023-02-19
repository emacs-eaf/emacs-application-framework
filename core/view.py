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

from PyQt6.QtCore import Qt, QEvent, QPoint
from PyQt6.QtGui import QPainter, QWindow, QBrush
from PyQt6.QtWidgets import QWidget, QVBoxLayout, QGraphicsView, QFrame
from core.utils import eval_in_emacs, focus_emacs_buffer, get_emacs_func_cache_result, hyprland_window_move

class View(QWidget):

    def __init__(self, buffer, view_info):
        super(View, self).__init__()

        self.buffer = buffer

        # Init widget attributes.
        if get_emacs_func_cache_result("eaf-emacs-running-in-wayland-native", []):
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint | Qt.WindowType.WindowStaysOnTopHint | Qt.WindowType.WindowOverridesSystemGestures | Qt.WindowType.BypassWindowManagerHint)
        elif get_emacs_func_cache_result("eaf-emacs-not-use-reparent-technology", []):
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint | Qt.WindowType.WindowStaysOnTopHint | Qt.WindowType.NoDropShadowWindowHint)
        else:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)

        self.setAttribute(Qt.WidgetAttribute.WA_X11DoNotAcceptFocus, True)
        self.setContentsMargins(0, 0, 0, 0)
        self.installEventFilter(self)

        # Init attributes.
        self.view_info = view_info
        (self.buffer_id, self.emacs_xid, self.x, self.y, self.width, self.height) = view_info.split(":")
        self.x: int = int(self.x)
        self.y: int = int(self.y)
        self.width: int = int(self.width)
        self.height: int = int(self.height)

        # Build QGraphicsView.
        self.layout: QVBoxLayout = QVBoxLayout(self)
        self.layout.setSpacing(0)
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.graphics_view = QGraphicsView(buffer, self)

        # Remove border from QGraphicsView.
        self.graphics_view.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.graphics_view.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.graphics_view.setRenderHints(QPainter.RenderHint.Antialiasing | QPainter.RenderHint.SmoothPixmapTransform | QPainter.RenderHint.TextAntialiasing)
        self.graphics_view.setFrameStyle(QFrame.Shape.NoFrame)

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
                self.graphics_view.fitInView(self.graphics_view.scene().sceneRect(), Qt.AspectRatioMode.KeepAspectRatio)
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

            self.buffer.buffer_widget.resize(int(view_width), int(view_height))

            self.layout.setContentsMargins(int(horizontal_padding), int(vertical_padding), int(horizontal_padding), int(vertical_padding))

    def eventFilter(self, obj, event):
        # import time
        # print(time.time(), event.type())

        import platform
        import os

        if os.getenv("XDG_CURRENT_DESKTOP") == "Hyprland" and event.type() in [QEvent.Type.Enter]:
            hyprland_window_move(self.x, self.y)

        if event.type() in [QEvent.Type.ShortcutOverride]:
            eval_in_emacs('eaf-activate-emacs-window', [self.buffer_id])

        # Focus emacs buffer when user click view.
        event_type = [QEvent.Type.MouseButtonPress, QEvent.Type.MouseButtonRelease, QEvent.Type.MouseButtonDblClick]
        if platform.system() != "Darwin":
            event_type += [QEvent.Type.Wheel]

        if event.type() in event_type:
            focus_emacs_buffer(self.buffer_id)
            # Stop mouse event.
            return True

        return False

    def showEvent(self, event):
        # NOTE: we must reparent after widget show, otherwise reparent operation maybe failed.
        import platform

        self.reparent()

        if platform.system() in ["Windows", "Darwin"]:
            eval_in_emacs('eaf-activate-emacs-window', [])

        # Make graphics view at left-top corner after show.
        self.graphics_view.verticalScrollBar().setValue(0)
        self.graphics_view.horizontalScrollBar().setValue(0)

    def reparent(self):
        # print("Reparent: ", self.buffer.url)
        qwindow = self.windowHandle()

        if not get_emacs_func_cache_result("eaf-emacs-not-use-reparent-technology", []):
            qwindow.setParent(QWindow.fromWinId(int(self.emacs_xid)))    # type: ignore

        qwindow.setPosition(QPoint(self.x, self.y))

    def try_show_top_view(self):
        if get_emacs_func_cache_result("eaf-emacs-not-use-reparent-technology", []):
            self.setWindowFlag(Qt.WindowType.WindowStaysOnTopHint, True)
            self.show()

    def try_hide_top_view(self):
        if get_emacs_func_cache_result("eaf-emacs-not-use-reparent-technology", []):
            self.setWindowFlag(Qt.WindowType.WindowStaysOnTopHint, False)
            self.hide()

    def destroy_view(self):
        # print("Destroy: ", self.buffer.url)
        self.destroy()

    def screen_shot(self):
        return self.grab()
