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

from core.browser import BrowserView, webview_scroll
from core.buffer import Buffer

class BrowserBuffer(Buffer):

    def __init__(self, buffer_id, url, arguments, fit_to_view, background_color):
        Buffer.__init__(self, buffer_id, url, arguments, fit_to_view, background_color)

        self.add_widget(BrowserView())

        self.buffer_widget.loadStarted.connect(self.start_progress)
        self.buffer_widget.loadProgress.connect(self.update_progress)
        self.buffer_widget.loadFinished.connect(self.stop_progress)

        self.buffer_widget.web_page.windowCloseRequested.connect(self.request_close_buffer)

    def get_key_event_widgets(self):
        # We need send key event to QWebEngineView's focusProxy widget, not QWebEngineView.
        return [self.buffer_widget.focusProxy()]

    def scroll(self, scroll_direction, scroll_type):
        webview_scroll(self, scroll_direction, scroll_type)

    def history_backward(self):
        self.buffer_widget.back()

    def history_forward(self):
        self.buffer_widget.forward()

    def clean_all_cookie(self):
        self.buffer_widget.clean_cookie()
        self.message_to_emacs.emit("Clean all cookie")

    def zoom_out(self):
        self.buffer_widget.zoom_out()

    def zoom_in(self):
        self.buffer_widget.zoom_in()

    def zoom_reset(self):
        self.buffer_widget.zoom_reset()

    def scroll_up(self):
        self.buffer_widget.web_page.runJavaScript("window.scrollBy(0, 50)")

    def scroll_down(self):
        self.buffer_widget.web_page.runJavaScript("window.scrollBy(0, -50)")

    def scroll_to_begin(self):
        self.buffer_widget.web_page.runJavaScript("window.scrollTo(0, 0)")

    def scroll_to_bottom(self):
        self.buffer_widget.web_page.runJavaScript("window.scrollBy(0, document.body.scrollHeight)")
