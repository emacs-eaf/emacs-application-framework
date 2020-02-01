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
from PyQt5.QtCore import QUrl, Qt, QEvent, QPointF, QEventLoop, QVariant, QTimer
from PyQt5.QtNetwork import QNetworkCookie
from PyQt5.QtWebEngineWidgets import QWebEngineView, QWebEnginePage, QWebEngineContextMenuData
from PyQt5.QtWidgets import QApplication, QWidget
from core.utils import touch
from core.buffer import Buffer
from urllib.parse import urlparse, parse_qs, urlunparse, urlencode
import os
import base64

MOUSE_BACK_BUTTON = 8
MOUSE_FORWARD_BUTTON = 16

class BrowserView(QWebEngineView):

    open_url_in_new_tab = QtCore.pyqtSignal(str)
    open_url_in_background_tab = QtCore.pyqtSignal(str)
    translate_selected_text = QtCore.pyqtSignal(str)

    def __init__(self, config_dir):
        super(QWebEngineView, self).__init__()

        self.installEventFilter(self)
        self.config_dir = config_dir

        self.web_page = BrowserPage()
        self.setPage(self.web_page)

        self.cookie_store = self.page().profile().cookieStore()
        self.cookie_storage = BrowserCookieStorage(config_dir)
        self.cookie_store.cookieAdded.connect(self.cookie_storage.add_cookie)

        self.selectionChanged.connect(self.select_text_change)

        self.load_cookie()

        self.search_term = ""

        with open(os.path.join(os.path.dirname(__file__), "js", "get_markers.js"), "r") as f:
            self.get_markers_js = f.read()

        with open(os.path.join(os.path.dirname(__file__), "js", "goto_marker.js"), "r") as f:
            self.goto_marker_raw = f.read()

        with open(os.path.join(os.path.dirname(__file__), "js", "get_focus_text.js"), "r") as f:
            self.get_focus_text_js = f.read()

        with open(os.path.join(os.path.dirname(__file__), "js", "set_focus_text.js"), "r") as f:
            self.set_focus_text_raw = f.read()

        with open(os.path.join(os.path.dirname(__file__), "js", "clear_focus.js"), "r") as f:
            self.clear_focus_js = f.read()

    def filter_url(self, url):
        parsed = urlparse(url)
        qd = parse_qs(parsed.query, keep_blank_values=True)
        filtered = dict((k, v) for k, v in qd.items()
                        # Remove google' track parameters.
                        if not k.startswith("gs_l")
                        and not k.startswith("safe")
                        and not k.startswith("oq")
                        and not k.startswith("ved")
                        and not k.startswith("uact")
                        and not k.startswith("ei")
                        and not k.startswith("newwindow"))
        return urlunparse([
            parsed.scheme,
            parsed.netloc,
            parsed.path,
            parsed.params,
            urlencode(filtered, doseq=True), # query string
            parsed.fragment
        ])

    def _search_text(self, text, is_backward = False):
        if self.search_term != text:
            self.search_term = text
        if is_backward:
            self.web_page.findText(self.search_term, self.web_page.FindBackward)
        else:
            self.web_page.findText(self.search_term)

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

    def search_quit(self):
        if self.search_term != "":
            self._search_text("")

    def select_text_change(self):
        modifiers = QApplication.keyboardModifiers()
        if modifiers == Qt.ControlModifier:
            self.translate_selected_text.emit(self.selectedText())

    def load_cookie(self):
        for cookie in self.cookie_storage.load_cookie():
            self.cookie_store.setCookie(cookie)

    def clear_cookies(self):
        self.cookie_storage.clear_cookies(self.cookie_store)

    def createWindow(self, window_type):
        return self.create_new_browser_window_callback()

    def event(self, event):
        if event.type() == QEvent.ChildAdded:
            obj = event.child()
            if isinstance(obj, QWidget):
                obj.installEventFilter(self)

        return QWebEngineView.event(self, event)

    def eventFilter(self, obj, event):
        if event.type() == QEvent.MouseButtonRelease:
            hit = self.web_page.hitTestContent(event.pos())
            clicked_url = hit.linkUrl()
            base_url = hit.baseUrl()

            if clicked_url is not None and base_url is not None and clicked_url != base_url and clicked_url != '':
                result = ""

                if 'http://' in clicked_url or 'https://' in clicked_url:
                    result = clicked_url
                elif clicked_url == "#":
                    result = base_url + clicked_url
                else:
                    # Don't open url in EAF if clicked_url is not start with http/ftp or #
                    result = "http://" + base_url.split("/")[2] + clicked_url

                    event.accept()
                    return False

                modifiers = QApplication.keyboardModifiers()

                if modifiers == Qt.ControlModifier:
                    self.open_url_new_buffer(result)
                else:
                    self.open_url(result)

                return True

            event.accept()
            return False

        elif event.type() == QEvent.MouseButtonPress:
            if event.button() == MOUSE_FORWARD_BUTTON:
                self.forward()

                event.accept()
                return True
            elif event.button() == MOUSE_BACK_BUTTON:
                self.back()

                event.accept()
                return True

        return super(QWebEngineView, self).eventFilter(obj, event)

    def open_url(self, url):
        self.setUrl(QUrl(url))

    def open_url_new_buffer(self, url):
        self.open_url_in_new_tab.emit(url)

    def open_url_background_buffer(self, url):
        self.open_url_in_background_tab.emit(url)

    def zoom_in(self):
        self.setZoomFactor(min(5, self.zoomFactor() + 0.25))

    def zoom_out(self):
        self.setZoomFactor(max(0.25, self.zoomFactor() - 0.25))

    def zoom_reset(self):
        self.setZoomFactor(1)

    def eval_js(self, js):
        self.web_page.runJavaScript(js)

    def execute_js(self, js):
        self.web_page.executeJavaScript(js)

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
        self.reload()

    def copy_text(self):
        self.triggerPageAction(self.web_page.Copy)

    def yank_text(self):
        self.triggerPageAction(self.web_page.Paste)

    def kill_text(self):
        self.triggerPageAction(self.web_page.Cut)

    def undo_action(self):
        self.triggerPageAction(self.web_page.Undo)

    def redo_action(self):
        self.triggerPageAction(self.web_page.Redo)

    def get_url(self):
        return self.web_page.executeJavaScript("window.location.href;")

    def cleanup_links(self):
        self.web_page.executeJavaScript("document.querySelector('.markerContainer').remove();")

    def get_link_makers(self):
        self.eval_js(self.get_markers_js);

    def jump_to_link(self, marker):
        self.goto_marker_js = self.goto_marker_raw.replace("%1", str(marker));
        link = self.web_page.executeJavaScript(self.goto_marker_js)
        self.cleanup_links()
        if link != "":
            self.open_url(link)

    def jump_to_link_new_buffer(self, marker):
        self.goto_marker_js = self.goto_marker_raw.replace("%1", str(marker));
        link = self.web_page.executeJavaScript(self.goto_marker_js)
        self.cleanup_links()
        if link != "":
            self.open_url_new_buffer(link)

    def jump_to_link_background_buffer(self, marker):
        self.goto_marker_js = self.goto_marker_raw.replace("%1", str(marker));
        link = self.web_page.executeJavaScript(self.goto_marker_js)
        self.cleanup_links()
        if link != "":
            self.open_url_background_buffer(link)

    def get_focus_text(self):
        return self.web_page.executeJavaScript(self.get_focus_text_js)

    def set_focus_text(self, new_text):
        self.set_focus_text_js = self.set_focus_text_raw.replace("%1", str(base64.b64encode(new_text.encode("utf-8")), "utf-8"));
        self.web_page.executeJavaScript(self.set_focus_text_js)

    def clear_focus(self):
        self.web_page.executeJavaScript(self.clear_focus_js)

class BrowserPage(QWebEnginePage):
    def __init__(self):
        QWebEnginePage.__init__(self)

    def hitTestContent(self, pos):
        return WebHitTestResult(self, pos)

    def mapToViewport(self, pos):
        return QPointF(pos.x(), pos.y())

    def executeJavaScript(self, scriptSrc):
        self.loop = QEventLoop()
        self.result = QVariant()
        QTimer.singleShot(250, self.loop.quit)

        self.runJavaScript(scriptSrc, self.callbackJS)
        self.loop.exec_()
        self.loop = None
        return self.result

    def callbackJS(self, res):
        if self.loop is not None and self.loop.isRunning():
            self.result = res
            self.loop.quit()

class WebHitTestResult():
    def __init__(self, page, pos):
        self.page = page
        self.pos = pos
        self.m_linkUrl = self.page.url().toString()
        self.m_baseUrl = self.page.url().toString()
        self.viewportPos = self.page.mapToViewport(self.pos)
        with open(os.path.join(os.path.dirname(__file__), "js", "open_in_new_tab.js"), "r") as f:
            self.open_in_new_tab_raw = f.read()

        self.open_in_new_tab_js = self.open_in_new_tab_raw.replace("%1", str(self.viewportPos.x())).replace("%2", str(self.viewportPos.y()))
        self.dic = self.page.executeJavaScript(self.open_in_new_tab_js)
        if self.dic is None:
            return

        self.m_isNull = False
        self.m_baseUrl = self.dic["baseUrl"]
        self.m_alternateText = self.dic["alternateText"]
        self.m_imageUrl = self.dic["imageUrl"]
        self.m_isContentEditable = self.dic["contentEditable"]
        self.m_isContentSelected = self.dic["contentSelected"]
        self.m_linkTitle = self.dic["linkTitle"]
        self.m_linkUrl = self.dic["linkUrl"]
        self.m_mediaUrl = self.dic["mediaUrl"]
        try:
            self.m_mediaPaused = self.dic["mediaPaused"]
            self.m_mediaMuted = self.dic["mediaMuted"]
        except Exception:
            pass
        self.m_tagName = self.dic["tagName"]

    def linkUrl(self):
        return self.m_linkUrl

    def isContentEditable(self):
        return self.m_isContentEditable

    def isContentSelected(self):
        return self.m_isContentSelected

    def imageUrl(self):
        try:
            return self.m_imageUrl
        except Exception:
            return ""

    def mediaUrl(self):
        return self.m_mediaUrl

    def baseUrl(self):
        return self.m_baseUrl

    def updateWithContextMenuData(self, data):
        if data.isValid():
            pass
        else:
            return

        self.m_linkTitle = data.linkText()
        self.m_linkUrl = data.linkUrl().toString()
        self.m_isContentEditable = data.isContentEditable()
        if data.selectedText() == "":
            self.m_isContentSelected = False
        else:
            self.m_isContentSelected = True

        if data.mediaType() == QWebEngineContextMenuData.MediaTypeImage:
            self.m_imageUrl = data.mediaUrl().toString()
        elif data.mediaType() == QWebEngineContextMenuData.MediaTypeAudio or data.mediaType() == QWebEngineContextMenuData.MediaTypeVideo:
            self.m_mediaUrl = data.mediaUrl().toString()

class BrowserCookieStorage:
    def __init__(self, config_dir):
        self.cookie_file = os.path.join(config_dir, "browser", "cookie", "cookie")

        touch(self.cookie_file)

    def load_cookie(self):
        with open(self.cookie_file, 'rb+') as store:
            cookies = store.read()
            return QNetworkCookie.parseCookies(cookies)

    def save_cookie(self, cookie):
        with open(self.cookie_file, 'wb+') as store:
            store.write(cookie + b'\n' if cookie is not None else b'')

    def add_cookie(self, cookie):
        raw = cookie.toRawForm()
        self.save_cookie(raw)

    def clear_cookies(self, cookie_store):
        cookie_store.deleteAllCookies()

        open(self.cookie_file, 'w').close()

class BrowserBuffer(Buffer):

    get_focus_text = QtCore.pyqtSignal(str, str)

    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, fit_to_view, background_color):
        Buffer.__init__(self, buffer_id, url, arguments, emacs_var_dict, fit_to_view, background_color)

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
            self.buffer_widget.jump_to_link_new_buffer(str(result_content))
        elif result_type == "jump_link_background_buffer":
            self.buffer_widget.jump_to_link_background_buffer(str(result_content))

    def cancel_input_message(self, result_type):
        if result_type == "jump_link" or result_type == "jump_link_new_buffer" or result_type == "jump_link_background_buffer":
            self.buffer_widget.cleanup_links()

    def search_text_forward(self):
        self.buffer_widget.search_text_forward()

    def search_text_backward(self):
        self.buffer_widget.search_text_backward()

    def history_backward(self):
        self.buffer_widget.back()

    def history_forward(self):
        self.buffer_widget.forward()

    def clear_all_cookies(self):
        self.buffer_widget.clear_cookies()
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
        self.buffer_widget.get_link_makers()
        self.send_input_message("Open Link: ", "jump_link");

    def open_link_new_buffer(self):
        self.buffer_widget.get_link_makers()
        self.send_input_message("Open Link in New Buffer: ", "jump_link_new_buffer");

    def open_link_background_buffer(self):
        self.buffer_widget.get_link_makers()
        self.send_input_message("Open Link in Background Buffer: ", "jump_link_background_buffer");

    def reset_default_zoom(self):
        if hasattr(self, "buffer_widget"):
            self.buffer_widget.setZoomFactor(float(self.emacs_var_dict["eaf-browser-default-zoom"]))

    def edit_focus_text(self):
        text = self.buffer_widget.get_focus_text()
        if text != None:
            self.get_focus_text.emit(self.buffer_id, text)
        else:
            self.message_to_emacs.emit("No active input element.")

    def set_focus_text(self, new_text):
        self.buffer_widget.set_focus_text(new_text)

    def is_focus(self):
        return self.buffer_widget.get_focus_text() != None

    def insert_or_scroll_up(self):
        if self.is_focus():
            self.fake_key_event(self.current_event_string)
        else:
            self.scroll_up()

    def insert_or_scroll_down(self):
        if self.is_focus():
            self.fake_key_event(self.current_event_string)
        else:
            self.scroll_down()

    def insert_or_scroll_up_page(self):
        if self.is_focus():
            self.fake_key_event(self.current_event_string)
        else:
            self.scroll_up_page()

    def insert_or_scroll_to_begin(self):
        if self.is_focus():
            self.fake_key_event(self.current_event_string)
        else:
            self.scroll_to_begin()

    def insert_or_scroll_to_bottom(self):
        if self.is_focus():
            self.fake_key_event(self.current_event_string)
        else:
            self.scroll_to_bottom()

    def insert_or_open_link(self):
        if self.is_focus():
            self.fake_key_event(self.current_event_string)
        else:
            self.open_link()

    def insert_or_open_link_new_buffer(self):
        if self.is_focus():
            self.fake_key_event(self.current_event_string)
        else:
            self.open_link_new_buffer()

    def insert_or_open_link_background_buffer(self):
        if self.is_focus():
            self.fake_key_event(self.current_event_string)
        else:
            self.open_link_background_buffer()

    def insert_or_history_backward(self):
        if self.is_focus():
            self.fake_key_event(self.current_event_string)
        else:
            self.history_backward()

    def insert_or_history_forward(self):
        if self.is_focus():
            self.fake_key_event(self.current_event_string)
        else:
            self.history_forward()

    def insert_or_scroll_left(self):
        if self.is_focus():
            self.fake_key_event(self.current_event_string)
        else:
            self.scroll_left()

    def insert_or_scroll_right(self):
        if self.is_focus():
            self.fake_key_event(self.current_event_string)
        else:
            self.scroll_right()

    def insert_or_new_blank_page(self):
        if self.is_focus():
            self.fake_key_event(self.current_event_string)
        else:
            self.new_blank_page()

    def insert_or_refresh_page(self):
        if self.is_focus():
            self.fake_key_event(self.current_event_string)
        else:
            self.refresh_page()

    def clear_focus(self):
        self.buffer_widget.clear_focus()
