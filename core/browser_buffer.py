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

from core.browser import BrowserView
from core.buffer import Buffer
import os

class BrowserBuffer(Buffer):

    def __init__(self, buffer_id, url, config_dir, arguments, fit_to_view, background_color):
        Buffer.__init__(self, buffer_id, url, arguments, fit_to_view, background_color)

        self.add_widget(BrowserView(config_dir))

        self.buffer_widget.loadStarted.connect(self.start_progress)
        self.buffer_widget.loadProgress.connect(self.update_progress)
        self.buffer_widget.loadFinished.connect(self.stop_progress)

        self.buffer_widget.web_page.windowCloseRequested.connect(self.request_close_buffer)

    def get_key_event_widgets(self):
        # We need send key event to QWebEngineView's focusProxy widget, not QWebEngineView.
        return [self.buffer_widget.focusProxy()]

    def scroll(self, scroll_direction, scroll_type):
        if scroll_type == "page":
            if scroll_direction == "up":
                self.scroll_up_page()
            else:
                self.scroll_down_page()
        else:
            if scroll_direction == "up":
                self.scroll_up()
            else:
                self.scroll_down()

    def handle_input_message(self, result_type, result_content):
        if result_type == "search_text_forward":
            self.buffer_widget._search_text(str(result_content))
        elif result_type == "search_text_backward":
            self.buffer_widget._search_text(str(result_content), True)
        elif result_type == "jump_link":
            self.buffer_widget.jump_to_link(str(result_content))
        elif result_type == "jump_link_new_buffer":
            self.buffer_widget.jump_to_link(str(result_content), "true")

    def cancel_input_message(self, result_type):
        if result_type == "jump_link" or result_type == "jump_link_new_buffer":
            self.buffer_widget.cleanup_links()

    def search_text_forward(self):
        self.buffer_widget.search_text_forward()

    def search_text_backward(self):
        self.buffer_widget.search_text_backward()

    def history_backward(self):
        self.buffer_widget.back()

    def history_forward(self):
        self.buffer_widget.forward()

    def clean_all_cookie(self):
        self.buffer_widget.clean_cookie()
        self.message_to_emacs.emit("Cleared all cookies.")

    def action_quit(self):
        self.buffer_widget.search_quit()

    def zoom_out(self):
        self.buffer_widget.zoom_out()

    def zoom_in(self):
        self.buffer_widget.zoom_in()

    def zoom_reset(self):
        self.buffer_widget.zoom_reset()

    def scroll_left(self):
        self.buffer_widget.scroll_left()

    def scroll_right(self):
        self.buffer_widget.scroll_right()

    def scroll_up(self):
        self.buffer_widget.scroll_up()

    def scroll_down(self):
        self.buffer_widget.scroll_down()

    def scroll_up_page(self):
        self.buffer_widget.scroll_up_page()

    def scroll_down_page(self):
        self.buffer_widget.scroll_down_page()

    def scroll_to_begin(self):
        self.buffer_widget.scroll_to_begin()

    def scroll_to_bottom(self):
        self.buffer_widget.scroll_to_bottom()

    def refresh_page(self):
        self.buffer_widget.refresh_page()

    def copy_text(self):
        self.buffer_widget.copy_text()

    def yank_text(self):
        self.buffer_widget.yank_text()

    def kill_text(self):
        self.buffer_widget.kill_text()

    def undo_action(self):
        self.buffer_widget.undo_action()

    def redo_action(self):
        self.buffer_widget.redo_action()

    def get_url(self):
        return self.buffer_widget.get_url()

    def open_link(self):
        self.buffer_widget.open_link()
        self.send_input_message("Open Link: ", "jump_link");

    def open_link_new_buffer(self):
        self.buffer_widget.open_link_new_buffer()
        self.send_input_message("Open Link in New Buffer: ", "jump_link_new_buffer");
