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

from PyQt5 import QtCore, QtWidgets
from PyQt5.QtCore import QUrl, Qt, QEvent, QPointF, QEventLoop, QVariant, QTimer, QRectF, QFile
from PyQt5.QtGui import QBrush, QColor
from PyQt5.QtNetwork import QNetworkCookie
from PyQt5.QtWebEngineWidgets import QWebEngineView, QWebEnginePage, QWebEngineScript, QWebEngineContextMenuData, QWebEngineProfile, QWebEngineSettings
from PyQt5.QtWidgets import QApplication, QWidget
from core.utils import touch, is_port_in_use, string_to_base64, popen_and_call, call_and_check_code, interactive
from core.buffer import Buffer
from urllib.parse import urlparse, parse_qs, urlunparse, urlencode
import os
import subprocess
import re
import base64
from functools import partial
import sqlite3


MOUSE_BACK_BUTTON = 8
MOUSE_FORWARD_BUTTON = 16

class BrowserView(QWebEngineView):

    open_url_in_new_tab = QtCore.pyqtSignal(str)
    duplicate_page_in_new_tab = QtCore.pyqtSignal(str)
    open_url_in_background_tab = QtCore.pyqtSignal(str)
    translate_selected_text = QtCore.pyqtSignal(str)
    trigger_focus_event = QtCore.pyqtSignal(str)

    def __init__(self, buffer_id, config_dir, emacs_var_dict):
        super(QWebEngineView, self).__init__()

        self.installEventFilter(self)
        self.buffer_id = buffer_id
        self.config_dir = config_dir
        self.emacs_var_dict = emacs_var_dict

        self.web_page = BrowserPage()
        self.setPage(self.web_page)

        self.cookie_store = self.page().profile().cookieStore()
        self.cookie_storage = BrowserCookieStorage(config_dir)
        self.cookie_store.cookieAdded.connect(self.cookie_storage.add_cookie)

        self.selectionChanged.connect(self.select_text_change)

        self.urlChanged.connect(lambda url: self.action_quit())

        self.load_cookie()

        self.search_term = ""

        self.marker_js_raw = self.read_js_content("marker.js")
        self.get_focus_text_js = self.read_js_content("get_focus_text.js")
        self.set_focus_text_raw = self.read_js_content("set_focus_text.js")
        self.clear_focus_js = self.read_js_content("clear_focus.js")
        self.select_input_text_js = self.read_js_content("select_input_text.js")
        self.dark_mode_js = self.read_js_content("dark_mode.js")
        self.caret_browsing_js_raw = self.read_js_content("caret_browsing.js")
        self.get_selection_text_js = self.read_js_content("get_selection_text.js")
        self.focus_input_js = self.read_js_content("focus_input.js")

    def load_css(self, path, name):
        path = QFile(path)
        if not path.open(QFile.ReadOnly | QtCore.QFile.Text):
            return
        css = path.readAll().data().decode("utf-8")
        SCRIPT = """
        (function() {
        try {
        css = document.createElement('style');
        css.type = 'text/css';
        css.id = "%s";
        document.head.appendChild(css);
        css.innerText = `%s`;
        } catch(e) {}
        })()
        """ % (name, css)

        script = QWebEngineScript()
        self.web_page.runJavaScript(SCRIPT, QWebEngineScript.ApplicationWorld)
        script.setName(name)
        script.setSourceCode(SCRIPT)
        script.setInjectionPoint(QWebEngineScript.DocumentReady)
        script.setRunsOnSubFrames(True)
        script.setWorldId(QWebEngineScript.ApplicationWorld)
        self.web_page.scripts().insert(script)

    def load_adblocker(self):
        self.load_css(os.path.join(os.path.dirname(__file__), "adblocker.css"),'adblocker')

    def remove_css(self, name, immediately):
        SCRIPT =  """
        (function() {
        var element = document.getElementById('%s');
        element.outerHTML = '';
        delete element;
        })()
         """ % (name)
        if immediately:
            self.web_page.runJavaScript(SCRIPT, QWebEngineScript.ApplicationWorld)
        script = self.web_page.scripts().findScript(name)
        self.web_page.scripts().remove(script)

    def open_downloads_setting(self):
        ''' Open aria2 download manage page. '''
        index_file = os.path.join(os.path.dirname(__file__), "aria2-ng", "index.html")
        self.open_url_new_buffer(QUrl.fromLocalFile(index_file).toString())

    def read_js_content(self, js_file):
        ''' Read content of JavaScript(js) files.'''
        return open(os.path.join(os.path.dirname(__file__), "js", js_file), "r").read()

    def filter_url(self, url):
        ''' Filter url, avoid duplicate url for the same keyword.'''
        parsed = urlparse(url)
        qd = parse_qs(parsed.query, keep_blank_values=True)
        if parsed.netloc.startswith("www.google.com"):
            # Only save search parameters for Google, avoid duplicate url for same keyword.
            filtered = dict((k, v) for k, v in qd.items() if k.startswith("q"))
        else:
            filtered = dict((k, v) for k, v in qd.items())

        return urlunparse([
            parsed.scheme,
            parsed.netloc,
            parsed.path,
            parsed.params,
            urlencode(filtered, doseq=True), # query string
            parsed.fragment
        ])

    def filter_title(self, title):
        "If title is google url, we should drop this history, it's a temp redirect url."
        try:
            parsed = urlparse(title)
            qd = parse_qs(parsed.query, keep_blank_values=True)
            if parsed.netloc.startswith("www.google.com"):
                return ""
            else:
                return title
        except Exception:
            return title

    def _search_text(self, text, is_backward = False):
        if self.search_term != text:
            self.search_term = text
        if is_backward:
            self.web_page.findText(self.search_term, self.web_page.FindBackward)
        else:
            self.web_page.findText(self.search_term)

    @interactive
    def toggle_adblocker(self):
        ''' Change adblocker status.'''
        if self.buffer.emacs_var_dict["eaf-browser-enable-adblocker"] == "true":
            self.buffer.set_emacs_var.emit("eaf-browser-enable-adblocker", "false", "true")
            self.remove_css('adblocker',True)
            self.buffer.message_to_emacs.emit("Successfully disabled adblocker!")
        elif self.buffer.emacs_var_dict["eaf-browser-enable-adblocker"] == "false":
            self.buffer.set_emacs_var.emit("eaf-browser-enable-adblocker", "true", "true")
            self.load_adblocker()
            self.buffer.message_to_emacs.emit("Successfully enabled adblocker!")

    @interactive
    def save_page_password(self):
        ''' Record form data.'''
        if self.buffer.emacs_var_dict["eaf-browser-enable-autofill"] == "true":
            self.buffer.add_password_entry()
            self.buffer.message_to_emacs.emit("Successfully recorded current page's password!")
        else:
            self.buffer.message_to_emacs.emit("Autofill is not enabled! Enable it with C-t")

    @interactive
    def toggle_password_autofill(self):
        ''' Toggle Autofill status for password data'''
        if self.buffer.emacs_var_dict["eaf-browser-enable-autofill"] == "false":
            self.buffer.set_emacs_var.emit("eaf-browser-enable-autofill", "true", "true")
            self.buffer.autofill_id = self.buffer.auto_fill(0)
            self.buffer.message_to_emacs.emit("Successfully enabled autofill!")
        else:
            self.buffer.autofill_id = self.buffer.auto_fill(self.buffer.autofill_id)
            if self.buffer.autofill_id == 0:
                self.buffer.set_emacs_var.emit("eaf-browser-enable-autofill", "false", "true")
                self.buffer.message_to_emacs.emit("Successfully disabled autofill!")
            else:
                self.buffer.message_to_emacs.emit("Successfully changed autofill data!")

    @interactive
    def search_text_forward(self):
        ''' Forward Search Text.'''
        if self.search_term == "":
            self.buffer.send_input_message("Forward Search Text: ", "search_text_forward")
        else:
            self._search_text(self.search_term)

    @interactive
    def search_text_backward(self):
        ''' Backward Search Text.'''
        if self.search_term == "":
            self.buffer.send_input_message("Backward Search Text: ", "search_text_backward")
        else:
            self._search_text(self.search_term, True)

    @interactive
    def action_quit(self):
        ''' Quit action.'''
        if self.search_term != "":
            self._search_text("")
        if self.buffer.caret_browsing_mode:
            if self.buffer.caret_browsing_mark_activated:
                self.buffer.caret_toggle_mark()
            else:
                self.buffer.caret_exit()

    def select_text_change(self):
        ''' Change selected text.'''
        # Not translate text if just exit caret mode.
        if self.buffer.caret_browsing_exit_flag:
            self.buffer.caret_browsing_exit_flag = False
            return

        # Only translate text when not in caret mode.
        if not self.buffer.caret_browsing_mode:
            modifiers = QApplication.keyboardModifiers()
            if modifiers == Qt.ControlModifier and self.selectedText().strip() != "":
                self.translate_selected_text.emit(self.selectedText())

    def load_cookie(self):
        ''' Load cookies.'''
        for cookie in self.cookie_storage.load_cookie():
            self.cookie_store.setCookie(cookie)

    def _clear_cookies(self):
        ''' Clear cookies.'''
        self.cookie_storage.clear_cookies(self.cookie_store)
        self.buffer.message_to_emacs.emit("Cleared all cookies.")

    def createWindow(self, window_type):
        ''' Create new browser window.'''
        return self.create_new_browser_window_callback()

    def event(self, event):
        ''' Catch event.'''
        if event.type() == QEvent.ChildAdded:
            obj = event.child()
            if isinstance(obj, QWidget):
                obj.installEventFilter(self)

        return QWebEngineView.event(self, event)

    def eventFilter(self, obj, event):
        ''' Handle event.'''
        # Control mouse cursor.
        # if event.type() != 1:
        #     import time
        #     print(time.time(), event.type(), self.rect())

        if event.type() in [QEvent.MouseMove]:
            # Hide cursor in fullscreen.
            if self.buffer.is_fullscreen:
                self.buffer.show_cursor()
                self.buffer.try_hide_cursor()
            # Show cursor if not in fullscreen.
            else:
                self.buffer.show_cursor()

        # Focus emacs buffer when user click view.
        if event.type() in [QEvent.MouseButtonPress, QEvent.MouseButtonRelease,
                            QEvent.MouseButtonDblClick, QEvent.Wheel]:
            self.trigger_focus_event.emit(self.buffer_id)

        if event.type() == QEvent.MouseButtonPress:
            if event.button() == MOUSE_FORWARD_BUTTON:
                self.forward()

                event.accept()
                return True
            elif event.button() == MOUSE_BACK_BUTTON:
                self.back()

                event.accept()
                return True

        if event.type() == QEvent.Wheel:
            modifiers = QApplication.keyboardModifiers()
            if modifiers == Qt.ControlModifier:
                if event.angleDelta().y() > 0:
                    self.zoom_in()
                else:
                    self.zoom_out()

        return super(QWebEngineView, self).eventFilter(obj, event)

    def open_url(self, url):
        ''' Configure current url.'''
        self.setUrl(QUrl(url))

    def open_url_new_buffer(self, url):
        ''' Open url in a new tab.'''
        self.open_url_in_new_tab.emit(url)

    def open_url_background_buffer(self, url):
        ''' Open url in background tab.'''
        self.open_url_in_background_tab.emit(url)

    @interactive(insert_or_do=True)
    def zoom_in(self):
        ''' Zoom in.'''
        self.setZoomFactor(min(5, self.zoomFactor() + 0.25))
        if float(self.buffer.emacs_var_dict["eaf-browser-default-zoom"]) == self.zoomFactor():
            self.buffer.zoom_data.delete_entry(urlparse(self.buffer.current_url).hostname)
        else:
            self.buffer.zoom_data.add_entry(urlparse(self.buffer.current_url).hostname, self.zoomFactor())

    @interactive(insert_or_do=True)
    def zoom_out(self):
        ''' Zoom out.'''
        self.setZoomFactor(max(0.25, self.zoomFactor() - 0.25))
        if float(self.buffer.emacs_var_dict["eaf-browser-default-zoom"]) == self.zoomFactor():
            self.buffer.zoom_data.delete_entry(urlparse(self.buffer.current_url).hostname)
        else:
            self.buffer.zoom_data.add_entry(urlparse(self.buffer.current_url).hostname, self.zoomFactor())

    @interactive(insert_or_do=True)
    def zoom_reset(self):
        ''' Reset the magnification.'''
        self.setZoomFactor(float(self.buffer.emacs_var_dict["eaf-browser-default-zoom"]))

    def eval_js(self, js):
        ''' Run JavaScript.'''
        self.web_page.runJavaScript(js)

    def eval_js_file(self, js_file):
        ''' Run JavaScript from JS file.'''
        self.eval_js(self.read_js_content(js_file))

    def execute_js(self, js):
        ''' Execute JavaScript.'''
        return self.web_page.execute_javascript(js)

    @interactive(insert_or_do=True)
    def scroll_left(self):
        ''' Scroll to left side.'''
        self.eval_js("document.scrollingElement.scrollBy(-35, 0)")

    @interactive(insert_or_do=True)
    def scroll_right(self):
        ''' Scroll to right side.'''
        self.eval_js("document.scrollingElement.scrollBy(35, 0)")

    @interactive(insert_or_do=True)
    def scroll_up(self):
        ''' Scroll up.'''
        self.eval_js("document.scrollingElement.scrollBy(0, 50)")

    @interactive(insert_or_do=True)
    def scroll_down(self):
        ''' Scroll down.'''
        self.eval_js("document.scrollingElement.scrollBy(0, -50)")

    @interactive
    def scroll_up_page(self):
        ''' Scroll page up.'''
        self.eval_js("document.scrollingElement.scrollBy({left: 0, top: window.innerHeight/2, behavior: '" + self.buffer.emacs_var_dict["eaf-browser-scroll-behavior"] + "'})")

    @interactive
    def insert_or_scroll_up_page(self):
        '''
If input is focus send space key to insert space.
If browser is fullscreen, send space key to play/pause video.

Otherwise, scroll page up.
        '''
        if self.buffer.is_focus() or self.buffer.is_fullscreen:
            self.buffer.fake_key_event(self.buffer.current_event_string)
        else:
            self.scroll_up_page()

    @interactive(insert_or_do=True)
    def scroll_down_page(self):
        ''' Scroll down a page.'''
        self.eval_js("document.scrollingElement.scrollBy({left: 0, top: -window.innerHeight/2, behavior: '" + self.buffer.emacs_var_dict["eaf-browser-scroll-behavior"] + "'})")

    @interactive(insert_or_do=True)
    def scroll_to_begin(self):
        ''' Scroll to the beginning.'''
        self.eval_js("document.scrollingElement.scrollTo({left: 0, top: 0, behavior: '" + self.buffer.emacs_var_dict["eaf-browser-scroll-behavior"] + "'})")

    @interactive(insert_or_do=True)
    def scroll_to_bottom(self):
        ''' Scroll to the bottom.'''
        self.eval_js("document.scrollingElement.scrollTo({left: 0, top: document.body.scrollHeight, behavior: '" + self.buffer.emacs_var_dict["eaf-browser-scroll-behavior"] + "'})")

    @interactive
    def get_selection_text(self):
        ''' Get the selected text.'''
        return self.execute_js(self.get_selection_text_js)

    @interactive(insert_or_do=True)
    def refresh_page(self):
        ''' Refresh the page.'''
        self.reload()

    @interactive(insert_or_do=True)
    def copy_text(self):
        ''' Copy selected text.'''
        self.triggerPageAction(self.web_page.Copy)
        if self.buffer.caret_browsing_mode and self.buffer.caret_browsing_mark_activated:
            self.buffer.caret_exit()

    @interactive(msg_emacs="Yank selected text.")
    def yank_text(self):
        ''' Paste selected text.'''
        self.triggerPageAction(self.web_page.Paste)

    @interactive(msg_emacs="Kill selected text.")
    def kill_text(self):
        ''' Cut selected text.'''
        self.triggerPageAction(self.web_page.Cut)

    @interactive
    def undo_action(self):
        ''' Undo action.'''
        self.triggerPageAction(self.web_page.Undo)

    @interactive
    def redo_action(self):
        ''' Redo action.'''
        self.triggerPageAction(self.web_page.Redo)

    @interactive
    def exit_fullscreen(self):
        ''' Exit full screen.'''
        self.triggerPageAction(self.web_page.ExitFullScreen)

    @interactive(insert_or_do=True)
    def view_source(self):
        ''' View source.'''
        self.triggerPageAction(self.web_page.ViewSource)

    def select_all(self):
        ''' Select all text.'''
        # We need window focus before select all text.
        self.eval_js("window.focus()")
        self.triggerPageAction(self.web_page.SelectAll)

    def select_input_text(self):
        ''' Select input text.'''
        self.eval_js(self.select_input_text_js)

    @interactive
    def get_url(self):
        ''' Get current url.'''
        return self.url().toString().replace(" ", "%20")

    def load_marker_file(self):
        self.eval_js(self.marker_js_raw.replace("%1", self.emacs_var_dict["eaf-marker-letters"]))

    def cleanup_links_dom(self):
        ''' Clean up links.'''
        self.load_marker_file()
        self.eval_js("Marker.cleanupLinks();")

    def get_link_markers(self):
        ''' Get link markers.'''
        self.load_marker_file()
        self.eval_js("Marker.generateMarker(Marker.generateClickMarkerList());")

    def get_text_markers(self):
        ''' Get visiable text markers.'''
        self.load_marker_file()
        self.eval_js("Marker.generateMarker(Marker.generateTextMarkerList());");

    def get_marker_link(self, marker):
        ''' Get marker's link.'''
        # print(self.execute_js("Marker.getMarkerSelector('%s')" % str(marker)))
        self.load_marker_file()
        link = self.execute_js("Marker.gotoMarker('%s', Marker.getMarkerAction)" % str(marker))
        self.cleanup_links_dom()
        if link is None or link.startswith("eaf::"):
            print(link)
            return False
        else:
            return link

    def _open_link(self, marker):
        ''' Jump to link according to marker.'''
        link = self.get_marker_link(marker)
        if link: self.open_url(link)

    def _open_link_new_buffer(self, marker):
        ''' Open the link at the markre in a new buffer.'''
        link = self.get_marker_link(marker)
        if link: self.open_url_new_buffer(link)

    def _open_link_background_buffer(self, marker):
        ''' Open link at the marker in the background.'''
        link = self.get_marker_link(marker)
        if link: self.open_url_background_buffer(link)

    def _copy_link(self, marker):
        ''' Copy the link.'''
        link = self.get_marker_link(marker)
        if link:
            self.buffer.set_clipboard_text(link)
            self.buffer.message_to_emacs.emit("Copied " + link)

    def get_code_markers(self):
        ''' Get the code markers.'''
        self.load_marker_file()
        self.eval_js("Marker.generateMarker(document.querySelectorAll('pre'))")

    def get_code_content(self, marker):
        ''' Get the code content according to marker.'''
        self.load_marker_file()
        content = self.execute_js("Marker.gotoMarker('%s', (e)=> e.textContent)" % str(marker))
        self.cleanup_links_dom()
        return content

    def _caret_at_line(self, marker):
        '''Enable caret by marker'''
        self.load_marker_file()
        self.execute_js("Marker.gotoMarker('%s', (e) => window.getSelection().collapse(e, 0))" % str(marker))
        self.cleanup_links_dom()

        self.eval_js("CaretBrowsing.setInitialCursor(true);")
        self.buffer.caret_browsing_mode = True
        self.buffer.eval_in_emacs.emit('eaf--toggle-caret-browsing', ["t" if self.buffer.caret_browsing_mode else "nil"])
        self.buffer.caret_toggle_mark()
        self.buffer.caret_next_word()


    def copy_code_content(self, marker):
        ''' Copy the code content according to marker.'''
        content = self.get_code_content(marker)
        if content != "":
            self.buffer.set_clipboard_text(content)
            self.buffer.message_to_emacs.emit("Copied code block!")

    def get_focus_text(self):
        ''' Get the focus text.'''
        return self.execute_js(self.get_focus_text_js)

    @interactive
    def set_focus_text(self, new_text):
        ''' Set the focus text.'''
        self.set_focus_text_js = self.set_focus_text_raw.replace("%1", string_to_base64(new_text));
        self.eval_js(self.set_focus_text_js)

    @interactive(insert_or_do=True)
    def focus_input(self):
        ''' input in focus.'''
        self.execute_js(self.focus_input_js)

    @interactive
    def clear_focus(self):
        ''' Clear the focus.'''
        self.eval_js(self.clear_focus_js)

    @interactive(insert_or_do=True)
    def dark_mode(self):
        ''' Dark mode support.'''
        self.eval_js(self.dark_mode_js)

class BrowserPage(QWebEnginePage):
    def __init__(self):
        QWebEnginePage.__init__(self)

    def execute_javascript(self, script_src):
        ''' Execute JavaScript.'''
        self.loop = QEventLoop()
        self.result = QVariant()
        QTimer.singleShot(250, self.loop.quit)

        self.runJavaScript(script_src, self.callback_js)
        self.loop.exec_()
        self.loop = None
        return self.result

    def callback_js(self, res):
        ''' Callback JS loop.'''
        if self.loop is not None and self.loop.isRunning():
            self.result = res
            self.loop.quit()

class BrowserCookieStorage:
    def __init__(self, config_dir):
        self.cookie_file = os.path.join(config_dir, "browser", "cookie", "cookie")

        touch(self.cookie_file)

    def load_cookie(self):
        ''' Load cookies.'''
        with open(self.cookie_file, 'rb+') as store:
            cookies = store.read()
            return QNetworkCookie.parseCookies(cookies)

    def save_cookie(self, cookie):
        ''' Save cookies.'''
        with open(self.cookie_file, 'wb+') as store:
            store.write(cookie + b'\n' if cookie is not None else b'')

    def add_cookie(self, cookie):
        ''' Add cookies.'''
        raw = cookie.toRawForm()
        self.save_cookie(raw)

    def clear_cookies(self, cookie_store):
        ''' Clear cookies.'''
        cookie_store.deleteAllCookies()
        open(self.cookie_file, 'w').close()

class HistoryPage():
    def __init__(self, title, url, hit):
        self.title = title
        self.url = url
        self.hit = float(hit)

class BrowserBuffer(Buffer):

    close_page = QtCore.pyqtSignal(str)
    get_focus_text = QtCore.pyqtSignal(str, str)
    open_dev_tools_tab = QtCore.pyqtSignal(object)

    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, fit_to_view):
        Buffer.__init__(self, buffer_id, url, arguments, emacs_var_dict, module_path, fit_to_view)

        self.add_widget(BrowserView(buffer_id, config_dir, emacs_var_dict))

        self.config_dir = config_dir
        self.page_closed = False

        self.autofill = PasswordDb(os.path.join(os.path.dirname(config_dir), "browser", "password.db"))
        self.autofill_id = 0

        self.zoom_data = ZoomSizeDb(os.path.join(os.path.dirname(config_dir), "browser", "zoom_data.db"))

        self.history_list = []
        if self.emacs_var_dict["eaf-browser-remember-history"] == "true":
            self.history_log_file_path = os.path.join(self.config_dir, "browser", "history", "log.txt")

            self.history_pattern = re.compile("^(.+)ᛝ(.+)ᛡ(.+)$")
            self.noprefix_url_pattern = re.compile("^(https?|file)://(.+)")
            self.nopostfix_url_pattern = re.compile("^[^#\?]*")
            self.history_close_file_path = os.path.join(self.config_dir, "browser", "history", "close.txt")
            touch(self.history_log_file_path)
            with open(self.history_log_file_path, "r", encoding="utf-8") as f:
                raw_list = f.readlines()
                for raw_his in raw_list:
                    his_line = re.match(self.history_pattern, raw_his)
                    if his_line is None: # Obsolete Old history format
                        old_his = re.match("(.*)\s((https?|file):[^\s]+)$", raw_his)
                        if old_his is not None:
                            self.history_list.append(HistoryPage(old_his.group(1), old_his.group(2), 1))
                    else:
                        self.history_list.append(HistoryPage(his_line.group(1), his_line.group(2), his_line.group(3)))

        # Set User Agent with Firefox's one to make EAF browser can login in Google account.
        self.pc_user_agent = "Mozilla/5.0 (X11; Linux i586; rv:31.0) Gecko/20100101 Firefox/72.0"
        self.phone_user_agent = "Mozilla/5.0 (iPhone; CPU iPhone OS 11_0 like Mac OS X) AppleWebKit/604.1.38 (KHTML, like Gecko) Version/11.0 Mobile/15A5370a Safari/604.1"
        self.profile = QWebEngineProfile(self.buffer_widget)
        self.profile.defaultProfile().setHttpUserAgent(self.pc_user_agent)

        self.draw_progressbar = False
        self.eval_dark_js = False
        self.eval_caret_js = False
        self.caret_browsing_mode = False
        self.caret_browsing_exit_flag = True
        self.caret_browsing_mark_activated = False
        self.caret_browsing_search_text = ""
        self.progressbar_progress = 0
        self.progressbar_color = QColor(self.emacs_var_dict["eaf-emacs-theme-foreground-color"])
        self.progressbar_height = 2
        self.light_mode_mask_color = QColor("#FFFFFF")
        self.dark_mode_mask_color = QColor("#242525")

        # Reverse background and foreground color, to help cursor recognition.
        self.caret_foreground_color = QColor(self.emacs_var_dict["eaf-emacs-theme-background-color"])
        self.caret_background_color = QColor(self.emacs_var_dict["eaf-emacs-theme-foreground-color"])

        self.current_url = ""
        self.request_url = ""
        self.no_need_draw_background = False
        self.try_hide_cursor_timer = None

        self.init_background_color()

        self.buffer_widget.loadStarted.connect(self.start_progress)
        self.buffer_widget.loadProgress.connect(self.update_progress)
        self.buffer_widget.urlChanged.connect(self.record_url)
        self.buffer_widget.web_page.windowCloseRequested.connect(self.close_buffer)
        self.buffer_widget.web_page.fullScreenRequested.connect(self.handle_fullscreen_request)
        self.buffer_widget.web_page.pdfPrintingFinished.connect(self.notify_print_message)
        self.profile.defaultProfile().downloadRequested.connect(self.handle_download_request)

        self.settings = QWebEngineSettings.globalSettings()
        try:
            self.settings.setAttribute(QWebEngineSettings.PluginsEnabled, self.emacs_var_dict["eaf-browser-enable-plugin"] == "true")
            self.settings.setAttribute(QWebEngineSettings.JavascriptEnabled, self.emacs_var_dict["eaf-browser-enable-javascript"] == "true")
            self.settings.setAttribute(QWebEngineSettings.FullScreenSupportEnabled, True)
            self.settings.setAttribute(QWebEngineSettings.PlaybackRequiresUserGesture, False)
            self.settings.setAttribute(QWebEngineSettings.DnsPrefetchEnabled, True)
            self.settings.setAttribute(QWebEngineSettings.FocusOnNavigationEnabled, True)

            if self.emacs_var_dict["eaf-browser-unknown-url-scheme-policy"] == "DisallowUnknownUrlSchemes":
                self.settings.setUnknownUrlSchemePolicy(self.settings.DisallowUnknownUrlSchemes)
            elif self.emacs_var_dict["eaf-browser-unknown-url-scheme-policy"] == "AllowUnknownUrlSchemesFromUserInteraction":
                self.settings.setUnknownUrlSchemePolicy(self.settings.AllowUnknownUrlSchemesFromUserInteraction)
            elif self.emacs_var_dict["eaf-browser-unknown-url-scheme-policy"] == "AllowAllUnknownUrlSchemes":
                self.settings.setUnknownUrlSchemePolicy(self.settings.AllowAllUnknownUrlSchemes)

            font_family = self.emacs_var_dict[ 'eaf-browser-font-family']
            if font_family:
                for ff in (
                    self.settings.StandardFont,
                    self.settings.FixedFont,
                    self.settings.SerifFont,
                    self.settings.SansSerifFont,
                    # What's these font families?
                    # self.settings.CursiveFont,
                    # self.settings.FantasyFont,
                    # self.settings.PictographFont
                ):
                    self.settings.setFontFamily(ff, font_family)
        except Exception:
            pass

        self.build_all_methods(self.buffer_widget)
        self.build_all_methods(self)
        self.build_interactive_method(self.buffer_widget, "back", "history_backward", insert_or_do=True)
        self.build_interactive_method(self.buffer_widget, "forward", "history_forward", insert_or_do=True)

        # Reset to default zoom when page init or page url changed.
        self.reset_default_zoom()
        self.buffer_widget.urlChanged.connect(lambda url: self.reset_default_zoom())

    def add_password_entry(self):
        password, form_data = self.buffer_widget.execute_js("""
        var password = "";
        var form_data = {};
        var input_list=document.getElementsByTagName("input");
        for(var i=0;i<input_list.length && input_list[i];i++){
            if(input_list[i].type === "password" && input_list[i].value != ""){
                password = input_list[i].value;
            }
            else if(input_list[i].type != "hidden" && input_list[i].value != "" && input_list[i].id != ""){
                form_data[input_list[i].id] = input_list[i].value;
            }
        }
        [password, form_data]
        """)
        self.autofill.add_entry(urlparse(self.current_url).hostname, password, form_data)

    def auto_fill(self, id):
        result = self.autofill.get_entries(urlparse(self.url).hostname, id)
        new_id = 0
        for row in result:
            new_id = row[0]
            password = row[2]
            form_data = row[3]
            self.buffer_widget.execute_js(
            """
            var form_data = %s;
            var input_list=document.getElementsByTagName("input");
            for(var i=0;i<input_list.length && input_list[i];i++){
                if(input_list[i].type === "password"){
                    input_list[i].value = "%s";
                }
                else if(input_list[i].type != "hidden" && input_list[i].id != ""){
                    input_list[i].value = form_data[input_list[i].id];
                }
            }
            """ % (form_data,password))
            break
        return new_id


    def init_auto_fill(self):
        if self.emacs_var_dict["eaf-browser-enable-autofill"] == "true":
            self.autofill_id = self.auto_fill(0)

    def notify_print_message(self, file_path, success):
        ''' Notify the print as pdf message.'''
        if success:
            # Try to rename pdf file with title.
            # Use host name if title include invalid file char.
            title_path = os.path.join(os.path.expanduser(self.emacs_var_dict["eaf-browser-download-path"]), "{}.pdf".format(self.title))
            try:
                os.rename(file_path, title_path)
                self.message_to_emacs.emit("Successfully saved current webpage as '{}'.".format(title_path))
            except Exception:
                self.message_to_emacs.emit("Successfully saved current webpage as '{}'.".format(file_path))
        else:
            self.message_to_emacs.emit("Failed to save current webpage as '{}'.".format(file_path))

    def notify_monolith_message(self, download_path, file_path, title, retcode):
        ''' Notify the save as html message.'''
        if retcode == 0:
            title_path = os.path.join(os.path.expanduser(download_path), "{}.html".format(title))
            try:
                os.rename(file_path, title_path)
                self.message_to_emacs.emit("Successfully saved current webpage as '{}'.".format(title_path))
            except Exception:
                self.message_to_emacs.emit("Successfully saved current webpage as '{}'.".format(file_path))
        else:
            self.message_to_emacs.emit("Failed to save current page as single file.")

    def record_url(self, url):
        ''' Record the url.'''
        self.request_url = url.toString()

        self.draw_background_filter()

    def draw_background_filter(self):
        '''Because Qt WebEngine will draw light background before loading new page.
        We draw dark background to avoid page flash when dark mode.

        This function include a white-list to control variable no_need_draw_background,
        we can add url to white-list if you found unnecessary loading at same page, such as, scroll to anchor.
        '''
        if self.dark_mode_is_enable():
            current_urls = self.current_url.rsplit("/", 1)
            request_urls = self.request_url.rsplit("/", 1)

            if self.request_url == "https://emacs-china.org/":
                self.no_need_draw_background = False
            if self.current_url.startswith("https://emacs-china.org/t/") and self.request_url.startswith("https://emacs-china.org/t/"):
                self.no_need_draw_background = current_urls[0] == request_urls[0] or self.request_url == current_urls[0]
            elif self.current_url.startswith("https://livebook.manning.com/book/") and self.request_url.startswith("https://livebook.manning.com/book/"):
                self.no_need_draw_background = current_urls[0] == request_urls[0]
            elif self.current_url.startswith("https://web.telegram.org") and self.request_url.startswith("https://web.telegram.org"):
                self.no_need_draw_background = True
            elif self.current_url.startswith("https://www.wikiwand.com/"):
                self.no_need_draw_background = True

    def dark_mode_is_enable(self):
        ''' Return bool of whether dark mode is enabled.'''
        module_name = self.module_path.split(".")[1]
        return (self.emacs_var_dict["eaf-browser-dark-mode"] == "true" or \
                (self.emacs_var_dict["eaf-browser-dark-mode"] == "follow" and self.emacs_var_dict["eaf-emacs-theme-mode"] == "dark")) \
                and module_name in ["browser", "terminal", "mindmap", "js-video-player"] \
                and self.url != "devtools://devtools/bundled/devtools_app.html" \
                and not self.url.startswith("https://www.reddit.com")

    def init_background_color(self):
        ''' Initialize the background colour.'''
        if self.dark_mode_is_enable():
            self.buffer_widget.web_page.setBackgroundColor(self.dark_mode_mask_color)
        else:
            self.buffer_widget.web_page.setBackgroundColor(self.light_mode_mask_color)

    def drawForeground(self, painter, rect):
        ''' Draw Foreground.'''
        if self.draw_progressbar:
            # Draw foreground over web page avoid white flash when eval dark_mode_js
            if self.dark_mode_is_enable() and not self.no_need_draw_background:
                painter.setBrush(self.dark_mode_mask_color)
                painter.drawRect(0, 0, rect.width(), rect.height())

            # Init progress bar brush.
            painter.setBrush(self.progressbar_color)

            if self.eval_dark_js:
                # Draw 100% when after eval dark_mode_js, avoid flash progressbar.
                painter.drawRect(0, 0, rect.width(), self.progressbar_height)
            else:
                # Draw progress bar.
                painter.drawRect(0, 0, rect.width() * self.progressbar_progress * 1.0 / 100, self.progressbar_height)

    @QtCore.pyqtSlot()
    def start_progress(self):
        ''' Initialize the Progress Bar.'''
        self.progressbar_progress = 0
        self.draw_progressbar = True
        self.update()

    @QtCore.pyqtSlot()
    def hide_progress(self):
        ''' Hide the Progress Bar.'''
        self.current_url = self.url
        self.no_need_draw_background = False

        self.draw_progressbar = False
        self.eval_dark_js = False
        self.update()

    @QtCore.pyqtSlot(int)
    def update_progress(self, progress):
        ''' Update the Progress Bar.'''
        if progress < 100:
            # Update progres.
            self.eval_caret_js = False
            self.progressbar_progress = progress
            self.update()
        elif progress == 100 and self.draw_progressbar:
            self.init_auto_fill()
            self.buffer_widget.load_marker_file()

            cursor_foreground_color = ""
            cursor_background_color = ""

            if self.dark_mode_is_enable():
                if self.emacs_var_dict["eaf-browser-dark-mode"] == "follow":
                    cursor_foreground_color = self.caret_background_color.name()
                    cursor_background_color = self.caret_foreground_color.name()
                else:
                    cursor_foreground_color = "#FFF"
                    cursor_background_color = "#000"
            else:
                if self.emacs_var_dict["eaf-browser-dark-mode"] == "follow":
                    cursor_foreground_color = self.caret_background_color.name()
                    cursor_background_color = self.caret_foreground_color.name()
                else:
                    cursor_foreground_color = "#000"
                    cursor_background_color = "#FFF"

            self.caret_browsing_js = self.buffer_widget.caret_browsing_js_raw.replace("%1", cursor_foreground_color).replace("%2", cursor_background_color)
            self.buffer_widget.eval_js(self.caret_browsing_js)
            self.eval_caret_js = True

            if self.emacs_var_dict["eaf-browser-enable-adblocker"] == "true":
                self.buffer_widget.load_adblocker()
            if self.dark_mode_is_enable():
                if not self.eval_dark_js:
                    self.dark_mode()
                    self.eval_dark_js = True

                    # We need show page some delay, avoid white flash when eval dark_mode_js
                    QtCore.QTimer.singleShot(1000, self.hide_progress)
            else:
                # Hide progress bar immediately if not dark mode.
                self.hide_progress()

    def handle_fullscreen_request(self, request):
        ''' Handle fullscreen request.'''
        if request.toggleOn():
            self.enter_fullscreen_request.emit()

            self.try_hide_cursor()
        else:
            self.exit_fullscreen_request.emit()

            self.show_cursor()

        request.accept()

    def handle_download_request(self, download_item):
        ''' Handle download request.'''
        download_data = download_item.url().toString()

        if download_data.startswith("data:image/"):
            image_path = os.path.join(os.path.expanduser(self.emacs_var_dict["eaf-browser-download-path"]), "image.png")
            touch(image_path)
            with open(image_path, "wb") as f:
                f.write(base64.decodestring(download_data.split(",")[1].encode("utf-8")))

            self.message_to_emacs.emit("Save image: " + image_path)
        else:
            self.try_start_aria2_daemon()

            from core.pyaria2 import Jsonrpc

            download_url = download_item.url().toString()
            jsonrpc = Jsonrpc('localhost', 6800)
            resp = jsonrpc.addUris(download_url)

            self.message_to_emacs.emit("Downloading: " + download_url)

    def show_cursor(self):
        if self.try_hide_cursor_timer:
            self.try_hide_cursor_timer.stop()

        QtWidgets.qApp.restoreOverrideCursor()

    def hide_cursor(self):
        QtWidgets.qApp.setOverrideCursor(Qt.BlankCursor)

    def try_hide_cursor(self):
        self.try_hide_cursor_timer = QTimer.singleShot(5000, self.hide_cursor)

    def _save_as_pdf(self):
        parsed = urlparse(self.url)
        qd = parse_qs(parsed.query, keep_blank_values=True)
        pdf_path = os.path.join(os.path.expanduser(self.emacs_var_dict["eaf-browser-download-path"]), "{}.pdf".format(parsed.netloc))
        self.message_to_emacs.emit("Saving as pdf...")
        self.buffer_widget.web_page.printToPdf(pdf_path)

    @interactive(insert_or_do=True)
    def save_as_pdf(self):
        ''' Request to save as pdf.'''
        self.send_input_message("Save current webpage as PDF?", "save_as_pdf", "yes-or-no")

    def _save_as_single_file(self):
        parsed = urlparse(self.url)
        qd = parse_qs(parsed.query, keep_blank_values=True)
        file_path = os.path.join(os.path.expanduser(self.emacs_var_dict["eaf-browser-download-path"]), "{}.html".format(parsed.netloc))
        self.message_to_emacs.emit("Saving as single file...")
        args = ["monolith", self.url, "-o", file_path]
        handler = partial(self.notify_monolith_message, self.emacs_var_dict["eaf-browser-download-path"], file_path, self.title)
        call_and_check_code(args, handler)

    @interactive(insert_or_do=True)
    def save_as_single_file(self):
        ''' Request to save current webpage as single html file.'''
        import shutil
        if shutil.which("monolith") is None:
            self.message_to_emacs.emit("Executable monolith not in PATH")
        else:
            self.send_input_message("Save current webpage as single html file?", "save_as_single_file", "yes-or-no")

    def destroy_buffer(self):
        ''' Destroy the buffer.'''
        # Record close page.
        self.close_page.emit(self.buffer_widget.get_url())

        # Load blank page to stop video playing, such as youtube.com.
        self.buffer_widget.open_url("about:blank")

        if self.buffer_widget is not None:
            # NOTE: We need delete QWebEnginePage manual, otherwise QtWebEngineProcess won't quit.
            self.buffer_widget.web_page.deleteLater()
            self.buffer_widget.deleteLater()

    def get_key_event_widgets(self):
        ''' Send key event to QWebEngineView's focusProxy widget.'''
        # We need send key event to QWebEngineView's focusProxy widget, not QWebEngineView.
        return [self.buffer_widget.focusProxy()]

    def scroll_other_buffer(self, scroll_direction, scroll_type):
        ''' Scroll.'''
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

    def handle_input_response(self, callback_tag, result_content):
        ''' Handle input message.'''
        result_content = str(result_content)
        if callback_tag == "search_text_forward":
            self.buffer_widget._search_text(result_content)
        elif callback_tag == "search_text_backward":
            self.buffer_widget._search_text(result_content, True)
        elif callback_tag == "caret_search_text_forward":
            self._caret_search_text(result_content)
        elif callback_tag == "caret_search_text_backward":
            self._caret_search_text(result_content, True)
        elif callback_tag == "caret_at_line":
            self.buffer_widget._caret_at_line(result_content.strip())
        elif callback_tag == "open_link" or callback_tag == "select_marker_text":
            self.buffer_widget._open_link(result_content.strip())
        elif callback_tag == "open_link_new_buffer":
            self.buffer_widget._open_link_new_buffer(result_content.strip())
        elif callback_tag == "jump_link_background_buffer":
            self.buffer_widget._open_link_background_buffer(result_content.strip())
        elif callback_tag == "copy_link":
            self.buffer_widget._copy_link(result_content.strip())
        elif callback_tag == "eval_js_file":
            self.buffer_widget.eval_js_file(result_content)
        elif callback_tag == "eval_js":
            self.buffer_widget.eval_js(result_content)
        elif callback_tag == "save_as_pdf":
            self._save_as_pdf()
        elif callback_tag == "save_as_single_file":
            self._save_as_single_file()
        elif callback_tag == "edit_url":
            self.buffer_widget.open_url(result_content)
        elif callback_tag == "copy_code":
            self.buffer_widget.copy_code_content(result_content.strip())
        elif callback_tag == "clear_history":
            self._clear_history()
        elif callback_tag == "import_chrome_history":
            self._import_chrome_history()
        elif callback_tag == "clear_cookies":
            self.buffer_widget._clear_cookies()

    def cancel_input_response(self, callback_tag):
        ''' Cancel input message.'''
        if callback_tag == "open_link" or \
           callback_tag == "open_link_new_buffer" or \
           callback_tag == "jump_link_background_buffer" or \
           callback_tag == "select_marker_text" or \
           callback_tag == "caret_at_line" or \
           callback_tag == "copy_link" or \
           callback_tag == "copy_code" or \
           callback_tag == "edit_url":
            self.buffer_widget.cleanup_links_dom()

    def try_start_aria2_daemon(self):
        ''' Try to start aria2 daemon.'''
        if not is_port_in_use(6800):
            with open(os.devnull, "w") as null_file:
                aria2_args = ["aria2c"]

                aria2_args.append("-d") # daemon
                aria2_args.append("-c") # continue download
                aria2_args.append("--auto-file-renaming=false") # not auto rename file
                aria2_args.append("-d {}".format(os.path.expanduser(str(self.emacs_var_dict["eaf-browser-download-path"]))))

                aria2_proxy_host = str(self.emacs_var_dict["eaf-browser-aria2-proxy-host"])
                aria2_proxy_port = str(self.emacs_var_dict["eaf-browser-aria2-proxy-port"])

                if aria2_proxy_host != "" and aria2_proxy_port != "":
                    aria2_args.append("--all-proxy")
                    aria2_args.append("http://{0}:{1}".format(aria2_proxy_host, aria2_proxy_port))

                aria2_args.append("--enable-rpc")
                aria2_args.append("--rpc-listen-all")

                subprocess.Popen(aria2_args, stdout=null_file)

    def caret_toggle_browsing(self):
        ''' Init caret browsing.'''
        if self.eval_caret_js:
            if self.caret_browsing_mode:
                self.buffer_widget.eval_js("CaretBrowsing.shutdown();")
                self.message_to_emacs.emit("Caret browsing deactivated.")
                self.caret_browsing_mode = False
            else:
                self.buffer_widget.eval_js("CaretBrowsing.setInitialCursor();")
                self.message_to_emacs.emit("Caret browsing activated.")
                self.caret_browsing_mode = True
            self.eval_in_emacs.emit('eaf--toggle-caret-browsing', ["t" if self.caret_browsing_mode else "nil"])

    def caret_exit(self):
        ''' Exit caret browsing.'''
        if self.caret_browsing_mode:
            # Avoid popup tranlsate tips when exit caret mode.
            self.caret_browsing_exit_flag = True

            self.caret_toggle_browsing()

    @interactive
    def caret_next_sentence(self):
        ''' Switch to next line in caret browsing.'''
        if self.caret_browsing_mode:
            self.buffer_widget.eval_js("CaretBrowsing.move('forward', 'sentence');")

    @interactive
    def caret_previous_sentence(self):
        ''' Switch to previous line in caret browsing.'''
        if self.caret_browsing_mode:
            self.buffer_widget.eval_js("CaretBrowsing.move('backward', 'sentence');")

    @interactive
    def caret_next_line(self):
        ''' Switch to next line in caret browsing.'''
        if self.caret_browsing_mode:
            self.buffer_widget.eval_js("CaretBrowsing.move('forward', 'line');")

    @interactive
    def caret_previous_line(self):
        ''' Switch to previous line in caret browsing.'''
        if self.caret_browsing_mode:
            self.buffer_widget.eval_js("CaretBrowsing.move('backward', 'line');")

    @interactive
    def caret_next_character(self):
        ''' Switch to next character in caret browsing.'''
        if self.caret_browsing_mode:
            self.buffer_widget.eval_js("CaretBrowsing.move('forward', 'character');")

    @interactive
    def caret_previous_character(self):
        ''' Switch to previous character in caret browsing.'''
        if self.caret_browsing_mode:
            self.buffer_widget.eval_js("CaretBrowsing.move('backward', 'character');")

    @interactive
    def caret_next_word(self):
        ''' Switch to next word in caret browsing.'''
        if self.caret_browsing_mode:
            self.buffer_widget.eval_js("CaretBrowsing.move('forward', 'word');")

    @interactive
    def caret_previous_word(self):
        ''' Switch to previous word in caret browsing.'''
        if self.caret_browsing_mode:
            self.buffer_widget.eval_js("CaretBrowsing.move('backward', 'word');")

    @interactive
    def caret_to_bottom(self):
        ''' Switch to next word in caret browsing.'''
        if self.caret_browsing_mode:
            self.buffer_widget.eval_js("CaretBrowsing.move('forward', 'documentboundary');")

    @interactive
    def caret_to_top(self):
        ''' Switch to previous word in caret browsing.'''
        if self.caret_browsing_mode:
            self.buffer_widget.eval_js("CaretBrowsing.move('backward', 'documentboundary');")

    @interactive
    def caret_rotate_selection(self):
        ''' Rotate selection.'''
        if self.caret_browsing_mode:
            if self.caret_browsing_mark_activated:
                self.buffer_widget.eval_js("CaretBrowsing.rotateSelection();")

    @interactive
    def caret_toggle_mark(self):
        ''' Toggle mark in caret browsing.'''
        if self.caret_browsing_mode:
            self.buffer_widget.eval_js("CaretBrowsing.toggleMark();")
            if self.buffer_widget.execute_js("CaretBrowsing.markEnabled"):
                self.caret_browsing_mark_activated = True
                self.message_to_emacs.emit("Caret Mark set")
            else:
                self.caret_browsing_mark_activated = False
                self.message_to_emacs.emit("Caret Mark deactivated")

    @interactive
    def caret_clear_search(self):
        ''' Clear search text in caret browsing.'''
        if self.caret_browsing_mode:
            if self.caret_browsing_mark_activated:
                self.caret_browsing_search_text = ""
                self.message_to_emacs.emit("Cleared caret search text.")

    @interactive
    def caret_search_forward(self):
        ''' Search Text forward in caret browsing.'''
        if self.caret_browsing_mode:
            if self.caret_browsing_mark_activated:
                if self.caret_browsing_search_text == "":
                    self.send_input_message("Forward Search Text and Select: ", "caret_search_text_forward")
                else:
                    self._caret_search_text(self.caret_browsing_search_text)

    @interactive
    def caret_search_backward(self):
        ''' Search Text backward in caret browsing.'''
        if self.caret_browsing_mode:
            if self.caret_browsing_mark_activated:
                if self.caret_browsing_search_text == "":
                    self.send_input_message("Backward Search Text and Select: ", "caret_search_text_backward")
                else:
                    self._caret_search_text(self.caret_browsing_search_text,True)

    def _caret_search_text(self, text, is_backward = False):
        if self.caret_browsing_search_text != text:
            self.caret_browsing_search_text = text
        if is_backward:
            if not self.buffer_widget.execute_js("window.find('"+text+"',false,true)"):
                self.message_to_emacs.emit("Unable to find more, please try forward search.")
        else:
            if not self.buffer_widget.execute_js("window.find('"+text+"')"):
                self.message_to_emacs.emit("Unable to find more, please try backward search.")

    @interactive
    def caret_translate_text(self):
        if self.buffer_widget.selectedText().strip() != "":
            self.buffer_widget.translate_selected_text.emit(self.buffer_widget.selectedText())

    @interactive(insert_or_do=True)
    def open_downloads_setting(self):
        ''' Open download manage page.'''
        self.try_start_aria2_daemon()
        self.buffer_widget.open_downloads_setting()

    def copy_text(self):
        ''' Copy selected text.'''
        self.buffer_widget.copy_text()
        self.message_to_emacs.emit("Copied selected text.")

    @interactive
    def copy_code(self):
        ''' Copy code.'''
        self.buffer_widget.get_code_markers()
        self.send_input_message("Copy code: ", "copy_code");

    @interactive(insert_or_do=True)
    def select_text(self):
        ''' Select Text.'''
        self.buffer_widget.get_text_markers()
        self.send_input_message("Select Text: ", "select_marker_text");

    @interactive(insert_or_do=True)
    def caret_at_line(self):
        self.buffer_widget.get_text_markers()
        self.send_input_message("Toggle Caret Browsing at Line: ", "caret_at_line");

    @interactive(insert_or_do=True)
    def open_link(self):
        ''' Open Link through a marker.'''
        self.buffer_widget.get_link_markers()
        self.send_input_message("Open Link: ", "open_link");

    @interactive(insert_or_do=True)
    def open_link_new_buffer(self):
        ''' Open Link in New Buffer.'''
        self.buffer_widget.get_link_markers()
        self.send_input_message("Open Link in New Buffer: ", "open_link_new_buffer");

    @interactive(insert_or_do=True)
    def open_link_background_buffer(self):
        ''' Open Link in Background Buffer.'''
        self.buffer_widget.get_link_markers()
        self.send_input_message("Open Link in Background Buffer: ", "jump_link_background_buffer");

    @interactive
    def copy_link(self):
        ''' Copy link.'''
        self.buffer_widget.get_link_markers()
        self.send_input_message("Copy link: ", "copy_link");

    @interactive(insert_or_do=True)
    def edit_url(self):
        ''' Edit link.'''
        self.send_input_message("Edit link: ", "edit_url", "string", self.url)

    def reset_default_zoom(self):
        ''' Reset default magnification.'''
        if hasattr(self, "buffer_widget"):
            result = self.zoom_data.get_entry(urlparse(self.url).hostname)
            zoom_factor = float(self.emacs_var_dict["eaf-browser-default-zoom"])
            for row in result:
                zoom_factor = float(row[0])
            self.buffer_widget.setZoomFactor(zoom_factor)

    def atomic_edit(self):
        ''' Edit the focus text.'''
        text = self.buffer_widget.get_focus_text()
        if text != None:
            self.get_focus_text.emit(self.buffer_id, text)
        else:
            self.message_to_emacs.emit("No active input element.")

    def is_focus(self):
        ''' Return bool of whether the buffer is focused.'''
        return self.buffer_widget.get_focus_text() != None or self.url == "devtools://devtools/bundled/devtools_app.html"

    def _record_history(self, new_title, new_url):
        # Throw traceback info if algorithm has bug and protection of historical record is not erased.
        try:
            noprefix_new_url_match = re.match(self.noprefix_url_pattern, new_url)
            if noprefix_new_url_match is not None:
                found = False
                for history in self.history_list:
                    noprefix_url_match = re.match(self.noprefix_url_pattern, history.url)
                    if noprefix_url_match is not None:
                        noprefix_url = noprefix_url_match.group(2)
                        noprefix_new_url = noprefix_new_url_match.group(2)
                        nopostfix_new_url_match = re.match(self.nopostfix_url_pattern, noprefix_new_url)

                        if noprefix_url == noprefix_new_url: # found unique url
                            history.title = new_title
                            history.url = new_url
                            history.hit += 0.5
                            found = True
                        elif nopostfix_new_url_match is not None and noprefix_url == nopostfix_new_url_match.group():
                            # also increment parent
                            history.hit += 0.25

                if not found:
                    self.history_list.append(HistoryPage(new_title, new_url, 1))

            self.history_list.sort(key = lambda x: x.hit, reverse = True)

            with open(self.history_log_file_path, "w", encoding="utf-8") as f:
                f.writelines(map(lambda history: history.title + "ᛝ" + history.url + "ᛡ" + str(history.hit) + "\n", self.history_list))
        except Exception:
            import traceback
            self.message_to_emacs.emit("Error in record_history: " + str(traceback.print_exc()))

    def record_history(self, new_title):
        ''' Record browser history.'''
        new_url = self.buffer_widget.filter_url(self.buffer_widget.get_url())
        if self.emacs_var_dict["eaf-browser-remember-history"] == "true" and self.buffer_widget.filter_title(new_title) != "" and \
           self.arguments != "temp_html_file" and new_title != "about:blank" and new_url != "about:blank":
            self._record_history(new_title, new_url)

    @interactive(insert_or_do=True)
    def new_blank_page(self):
        ''' Open new blank page.'''
        self.eval_in_emacs.emit('eaf-open', [self.emacs_var_dict["eaf-browser-blank-page-url"], "browser", "", 't'])

    def _clear_history(self):
        if os.path.exists(self.history_log_file_path):
            os.remove(self.history_log_file_path)
            self.message_to_emacs.emit("Cleared browsing history.")
        else:
            self.message_to_emacs.emit("There is no browsing history.")

    @interactive
    def clear_history(self):
        ''' Clear browsing history.'''
        self.send_input_message("Are you sure you want to clear all browsing history?", "clear_history", "yes-or-no")

    def _import_chrome_history(self):
        dbpath = os.path.expanduser(self.emacs_var_dict["eaf-browser-chrome-history-file"])
        if not os.path.exists(dbpath):
            self.message_to_emacs.emit("The chrome history file: '{}' not exist, please check your setting.".format(dbpath))
            return

        self.message_to_emacs.emit("Importing from {}...".format(dbpath))

        conn = sqlite3.connect(dbpath)
        # Keep lastest entry in dict by last_visit_time asc order.
        sql = 'select title, url from urls order by last_visit_time asc'
        # May fetch many by many not fetch all,
        # but this should called only once, so not important now.
        try:
            chrome_histories = conn.execute(sql).fetchall()
        except sqlite3.OperationalError as e:
            if e.args[0] == 'database is locked':
                self.message_to_emacs.emit("The chrome history file is locked, please close your chrome app first.")
            else:
                self.message_to_emacs.emit("Failed to read chrome history entries: {}.".format(e))
            return

        histories = dict(chrome_histories)  # Drop duplications with same title.
        total = len(histories)
        for i, (title, url) in enumerate(histories.items(), 1):
            self._record_history(title, url)
            self.message_to_emacs.emit("Importing {} / {} ...".format(i, total))
        self.message_to_emacs.emit("{} chrome history entries imported.".format(total))

    @interactive
    def import_chrome_history(self):
        ''' Import history entries from chrome history db.'''
        self.send_input_message("Are you sure you want to import all history from chrome?", "import_chrome_history", "yes-or-no")

    @interactive
    def clear_cookies(self):
        ''' Clear cookies.'''
        self.send_input_message("Are you sure you want to clear all browsing cookies?", "clear_cookies", "yes-or-no")

    def record_close_page(self, url):
        ''' Record closing pages.'''
        self.page_closed = True
        if self.emacs_var_dict["eaf-browser-remember-history"] == "true" and self.arguments != "temp_html_file" and url != "about:blank":
            touch(self.history_close_file_path)
            with open(self.history_close_file_path, "r") as f:
                close_urls = f.readlines()
                close_urls.append("{0}\n".format(url))
                if len(close_urls) > 30:
                    del close_urls[:len(close_urls)-30]
                open(self.history_close_file_path, "w").writelines(close_urls)

    @interactive(insert_or_do=True)
    def recover_prev_close_page(self):
        ''' Recover previous closed pages.'''
        if os.path.exists(self.history_close_file_path):
            with open(self.history_close_file_path, "r") as f:
                close_urls = f.readlines()
                if len(close_urls) > 0:
                    # We need use rstrip remove \n char from url record.
                    prev_close_url = close_urls.pop().rstrip()
                    self.open_url_in_new_tab.emit(prev_close_url)
                    open(self.history_close_file_path, "w").writelines(close_urls)

                    self.message_to_emacs.emit("Recovery {0}".format(prev_close_url))
                else:
                    self.message_to_emacs.emit("No page need recovery.")
        else:
            self.message_to_emacs.emit("No page need recovery.")

    @interactive(insert_or_do=True)
    def duplicate_page(self):
        self.buffer_widget.duplicate_page_in_new_tab.emit(self.current_url)

    @interactive(insert_or_do=True)
    def open_browser(self):
        ''' Open browser.'''
        self.eval_in_emacs.emit('call-interactively', ['\'eaf-open-browser-with-history'])

    def select_all_or_input_text(self):
        ''' Select all or input text.'''
        if self.is_focus():
            self.buffer_widget.select_input_text()
        else:
            self.buffer_widget.select_all()

    def eval_js_file(self):
        ''' Eval JS file.'''
        self.send_input_message("Eval JS file: ", "eval_js_file", "file")

    def eval_js(self):
        ''' Eval JS interactively.'''
        self.send_input_message("Eval JS: ", "eval_js")

    def open_devtools(self):
        ''' Open dev-tool page.'''
        self.open_dev_tools_tab.emit(self.buffer_widget.web_page)

    @interactive(insert_or_do=True)
    def toggle_device(self):
        ''' Toggle device.'''
        user_agent = self.profile.defaultProfile().httpUserAgent()
        if user_agent == self.pc_user_agent:
            self.profile.defaultProfile().setHttpUserAgent(self.phone_user_agent)
            self.set_aspect_ratio(2.0 / 3)
        else:
            self.profile.defaultProfile().setHttpUserAgent(self.pc_user_agent)
            self.set_aspect_ratio(0)

        self.refresh_page()

    @interactive(insert_or_do=True)
    def download_youtube_video(self):
        ''' Download Youtube Video.'''
        self.download_youtube_file()

    @interactive(insert_or_do=True)
    def download_youtube_audio(self):
        ''' Download Youtube Audio.'''
        self.download_youtube_file(True)

    def download_youtube_file(self, only_audio=False):
        ''' Download Youtube File.'''
        url = self.buffer_widget.get_url()
        if url.startswith("https://www.youtube.com"):
            import shutil

            if shutil.which("youtube-dl"):
                download_path = "{}/%(title)s-%(id)s.%(ext)s".format(os.path.expanduser(str(self.emacs_var_dict["eaf-browser-download-path"])))

                youtube_dl_args = ["youtube-dl"]
                youtube_dl_args.append("--proxy")
                youtube_dl_args.append(self.proxy_string)
                youtube_dl_args.append(url)
                youtube_dl_args.append("-o")
                youtube_dl_args.append(download_path)

                file_type = "video"
                if only_audio:
                    youtube_dl_args.append("-x")
                    file_type = "audio"

                with open(os.devnull, "w") as null_file:
                    popen_and_call(youtube_dl_args, lambda : self.message_to_emacs.emit("Downloaded: {0}".format(url)), null_file)

                self.message_to_emacs.emit("Downloading {0}: {1}".format(file_type, url))
            else:
                self.message_to_emacs.emit("Please install youtube-dl to use this feature.")
        else:
            self.message_to_emacs.emit("Only videos from YouTube can be downloaded for now.")

class PasswordDb(object):
    def __init__(self, dbpath):
        self._conn = sqlite3.connect(dbpath)
        self._conn.execute("""
        CREATE TABLE IF NOT EXISTS autofill
        (id INTEGER PRIMARY KEY AUTOINCREMENT, host TEXT,
         password TEXT, form_data TEXT)
        """)

    def add_entry(self, host, password, form_data):
        result = self._conn.execute("""
        SELECT id, host, password, form_data FROM autofill
        WHERE host=? AND form_data=? ORDER BY id
        """, (host, str(form_data)))
        if len(list(result))>0:
            self._conn.execute("""
            UPDATE autofill SET password=?
            WHERE host=? and form_data=?
            """, (password, host, str(form_data)))
        else:
            self._conn.execute("""
            INSERT INTO autofill (host, password, form_data)
            VALUES (?, ?, ?)
            """, (host, password, str(form_data)))
        self._conn.commit()

    def get_entries(self, host, id):
        return self._conn.execute("""
        SELECT id, host, password, form_data FROM autofill
        WHERE host=? and id>? ORDER BY id
        """, (host, id))

class ZoomSizeDb(object):
    def __init__(self, dbpath):
        self._conn = sqlite3.connect(dbpath)
        self._conn.execute("""
        CREATE TABLE IF NOT EXISTS ZoomSize
        (Host TEXT PRIMARY KEY, ZoomScale REAL)
        """)

    def add_entry(self, host, zoom_scale):
        result = self._conn.execute("""
        SELECT Host, ZoomScale FROM ZoomSize
        WHERE Host=?
        """, (host,))
        if len(list(result))>0:
            self._conn.execute("""
            UPDATE ZoomSize SET ZoomScale=?
            WHERE Host=?
            """, (zoom_scale, host))
        else:
            self._conn.execute("""
            INSERT INTO ZoomSize (Host, ZoomScale)
            VALUES (?, ?)
            """, (host, zoom_scale))
        self._conn.commit()

    def get_entry(self, host):
        return self._conn.execute("""
        SELECT ZoomScale FROM ZoomSize
        WHERE Host=?
        """, (host,))

    def delete_entry(self, host):
        self._conn.execute("""
        DELETE FROM ZoomSize
        WHERE Host=?
        """, (host,))
        self._conn.commit()
