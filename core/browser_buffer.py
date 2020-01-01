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

    def __init__(self, buffer_id, url, arguments, fit_to_view, background_color):
        Buffer.__init__(self, buffer_id, url, arguments, fit_to_view, background_color)

        self.add_widget(BrowserView())

        self.buffer_widget.loadStarted.connect(self.start_progress)
        self.buffer_widget.loadProgress.connect(self.update_progress)
        self.buffer_widget.loadFinished.connect(self.stop_progress)

        self.buffer_widget.web_page.windowCloseRequested.connect(self.request_close_buffer)

        self.search_term = ""

        with open(os.path.join(os.path.dirname(__file__), "js", "get_markers.js"), "r") as f:
            self.get_markers_js = f.read()

        with open(os.path.join(os.path.dirname(__file__), "js", "goto_marker.js"), "r") as f:
            self.goto_marker_raw = f.read()

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

    def _search_text(self, text, is_backward = False):
        if self.search_term != text:
            self.search_term = text
        if is_backward:
            self.buffer_widget.web_page.findText(self.search_term, self.buffer_widget.web_page.FindBackward)
        else:
            self.buffer_widget.web_page.findText(self.search_term)

    def handle_input_message(self, result_type, result_content):
        if result_type == "search_text_forward":
            self._search_text(str(result_content))
        elif result_type == "search_text_backward":
            self._search_text(str(result_content), True)
        elif result_type == "jump_link":
            self.jump_to_link(str(result_content))
        elif result_type == "jump_link_new_buffer":
            self.jump_to_link(str(result_content), "true")

    def cancel_input_message(self, result_type):
        if result_type == "jump_link" or result_type == "jump_link_new_buffer":
            self.cleanup_links()

    def search_text_forward(self):
        if self.search_term == "":
            self.send_input_message("Forward Search Text: ", "search_text_forward")
        else:
            self._search_text(self.search_term)

    def search_text_backward(self):
        if self.search_term == "":
            self.send_input_message("Backward Search Text: ", "search_text_backward")
        else:
            self._search_text(self.search_term, True)

    def eval_js(self, js):
        self.buffer_widget.web_page.runJavaScript(js)

    def history_backward(self):
        self.buffer_widget.back()

    def history_forward(self):
        self.buffer_widget.forward()

    def clean_all_cookie(self):
        self.buffer_widget.clean_cookie()
        self.message_to_emacs.emit("Cleared all cookies.")

    def action_quit(self):
        if self.search_term != "":
            self._search_text("")

    def zoom_out(self):
        self.buffer_widget.zoom_out()

    def zoom_in(self):
        self.buffer_widget.zoom_in()

    def zoom_reset(self):
        self.buffer_widget.zoom_reset()

    def scroll_left(self):
        self.eval_js("window.scrollBy(-50, 0)")

    def scroll_right(self):
        self.eval_js("window.scrollBy(50, 0)")

    def scroll_up(self):
        self.eval_js("window.scrollBy(0, 50)")

    def scroll_down(self):
        self.eval_js("window.scrollBy(0, -50)")

    def scroll_up_page(self):
        self.eval_js("window.scrollBy(0, document.documentElement.clientHeight)")

    def scroll_down_page(self):
        self.eval_js("window.scrollBy(0, -document.documentElement.clientHeight)")

    def scroll_to_begin(self):
        self.eval_js("window.scrollTo(0, 0)")

    def scroll_to_bottom(self):
        self.eval_js("window.scrollBy(0, document.body.scrollHeight)")

    def refresh_page(self):
        self.buffer_widget.reload()

    def copy_text(self):
        self.buffer_widget.triggerPageAction(self.buffer_widget.web_page.Copy)

    def yank_text(self):
        self.buffer_widget.triggerPageAction(self.buffer_widget.web_page.Paste)

    def kill_text(self):
        self.buffer_widget.triggerPageAction(self.buffer_widget.web_page.Cut)

    def undo_action(self):
        self.buffer_widget.triggerPageAction(self.buffer_widget.web_page.Undo)

    def redo_action(self):
        self.buffer_widget.triggerPageAction(self.buffer_widget.web_page.Redo)

    def get_url(self):
        return self.buffer_widget.web_page.executeJavaScript("window.location.href;")

    def open_link(self):
        self.eval_js(self.get_markers_js);
        self.send_input_message("Open Link: ", "jump_link");

    def open_link_new_buffer(self):
        self.eval_js(self.get_markers_js);
        self.send_input_message("Open Link in New Buffer: ", "jump_link_new_buffer");

    def jump_to_link(self, marker, new_buffer = "false"):
        self.goto_marker_js = self.goto_marker_raw.replace("%1", str(marker)).replace("%2", new_buffer);
        self.buffer_widget.web_page.executeJavaScript(self.goto_marker_js);
        self.cleanup_links()

    def cleanup_links(self):
        self.buffer_widget.web_page.executeJavaScript("document.querySelector('.markerContainer').remove();")
