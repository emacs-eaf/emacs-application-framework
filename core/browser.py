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
    open_url_in_background_tab = QtCore.pyqtSignal(str)
    translate_selected_text = QtCore.pyqtSignal(str)
    trigger_focus_event = QtCore.pyqtSignal(str)

    def __init__(self, buffer_id, config_dir):
        super(QWebEngineView, self).__init__()

        self.installEventFilter(self)
        self.buffer_id = buffer_id
        self.config_dir = config_dir

        self.web_page = BrowserPage()
        self.setPage(self.web_page)

        self.cookie_store = self.page().profile().cookieStore()
        self.cookie_storage = BrowserCookieStorage(config_dir)
        self.cookie_store.cookieAdded.connect(self.cookie_storage.add_cookie)

        self.selectionChanged.connect(self.select_text_change)

        self.urlChanged.connect(lambda url: self.search_quit())

        self.load_cookie()

        self.search_term = ""

        self.get_markers_raw = self.read_js_content("get_markers.js")
        self.goto_marker_raw = self.read_js_content("goto_marker.js")
        self.get_codes_raw = self.read_js_content("get_codes.js")
        self.goto_code_raw = self.read_js_content("goto_code.js")
        self.get_focus_text_js = self.read_js_content("get_focus_text.js")
        self.set_focus_text_raw = self.read_js_content("set_focus_text.js")
        self.clear_focus_js = self.read_js_content("clear_focus.js")
        self.select_input_text_js = self.read_js_content("select_input_text.js")
        self.dark_mode_js = self.read_js_content("dark_mode.js")
        self.caret_browsing_js = self.read_js_content("caret_browsing.js")
        self.get_selection_text_js = self.read_js_content("get_selection_text.js")
        self.focus_input_js = self.read_js_content("focus_input.js")

    def load_css(self, path, name):
        path = QFile(path)
        if not path.open(QFile.ReadOnly | QtCore.QFile.Text):
            return
        css = path.readAll().data().decode("utf-8")
        SCRIPT = """
        (function() {
        css = document.createElement('style');
        css.type = 'text/css';
        css.id = "%s";
        document.head.appendChild(css);
        css.innerText = `%s`;
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

    def open_download_manage_page(self):
        ''' Open aria2-webui download manage page. '''
        self.open_url_new_buffer("file://" + (os.path.join(os.path.dirname(__file__), "aria2-webui", "index.html")))

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

    @interactive()
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

    @interactive()
    def record_form_data(self):
        ''' Record form data.'''
        self.buffer.add_password_entry()
        self.buffer.message_to_emacs.emit("Successfully recorded form data!")

    @interactive()
    def toggle_autofill(self):
        ''' Toggle Autofill status and data'''
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

    @interactive()
    def search_text_forward(self):
        ''' Forward Search Text.'''
        if self.search_term == "":
            self.buffer.send_input_message("Forward Search Text: ", "search_text_forward")
        else:
            self._search_text(self.search_term)

    @interactive()
    def search_text_backward(self):
        ''' Backward Search Text.'''
        if self.search_term == "":
            self.buffer.send_input_message("Backward Search Text: ", "search_text_backward")
        else:
            self._search_text(self.search_term, True)

    @interactive(new_name = "action_quit")
    def search_quit(self):
        ''' Quit search.'''
        if self.search_term != "":
            self._search_text("")

    def select_text_change(self):
        ''' Change selected text.'''
        modifiers = QApplication.keyboardModifiers()
        if modifiers == Qt.ControlModifier:
            self.translate_selected_text.emit(self.selectedText())

    def load_cookie(self):
        ''' Load cookies.'''
        for cookie in self.cookie_storage.load_cookie():
            self.cookie_store.setCookie(cookie)

    def clear_cookies(self):
        ''' Clear cookies.'''
        self.cookie_storage.clear_cookies(self.cookie_store)

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

    @interactive(insert_or_do=True)
    def zoom_out(self):
        ''' Zoom out.'''
        self.setZoomFactor(max(0.25, self.zoomFactor() - 0.25))

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

    @interactive(insert_or_do=True)
    def scroll_up_page(self):
        ''' Scroll up a page.'''
        self.eval_js("document.scrollingElement.scrollBy({left: 0, top: window.innerHeight/2, behavior: '" + self.buffer.emacs_var_dict["eaf-browser-scroll-behavior"] + "'})")

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

    def get_selection_text(self):
        ''' Get the selected text.'''
        return self.execute_js(self.get_selection_text_js)

    @interactive(insert_or_do=True)
    def refresh_page(self):
        ''' Refresh the page.'''
        self.reload()

    def copy_text(self):
        ''' Copy selected text.'''
        self.triggerPageAction(self.web_page.Copy)

    @interactive(msg_emacs="Yank selected text.")
    def yank_text(self):
        ''' Paste selected text.'''
        self.triggerPageAction(self.web_page.Paste)

    @interactive(msg_emacs="Kill selected text.")
    def kill_text(self):
        ''' Cut selected text.'''
        self.triggerPageAction(self.web_page.Cut)

    @interactive()
    def undo_action(self):
        ''' Undo action.'''
        self.triggerPageAction(self.web_page.Undo)

    @interactive()
    def redo_action(self):
        ''' Redo action.'''
        self.triggerPageAction(self.web_page.Redo)

    @interactive()
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

    @interactive()
    def get_url(self):
        ''' Get current url.'''
        return self.execute_js("window.location.href;")

    def cleanup_links(self):
        ''' Clean up links.'''
        self.eval_js("document.querySelector('.eaf-marker-container').remove();")
        self.eval_js("document.querySelector('.eaf-style').remove();")

    def get_link_markers(self):
        ''' Get link markers.'''
        self.eval_js(self.get_markers_raw.replace("%1", self.buffer.emacs_var_dict["eaf-marker-letters"]));

    def get_marker_link(self, marker):
        ''' Get marker's link.'''
        self.goto_marker_js = self.goto_marker_raw.replace("%1", str(marker));
        link = self.execute_js(self.goto_marker_js)
        self.cleanup_links()
        return link

    def jump_to_link(self, marker):
        ''' Jump to link according to marker.'''
        link = self.get_marker_link(marker)
        if link != "":
            self.open_url(link)

    def jump_to_link_new_buffer(self, marker):
        ''' Jump to a new buffer of the link.'''
        link = self.get_marker_link(marker)
        if link != "":
            self.open_url_new_buffer(link)

    def jump_to_link_background_buffer(self, marker):
        ''' Jump to the bacground buffer of the link.'''
        link = self.get_marker_link(marker)
        if link != "":
            self.open_url_background_buffer(link)

    def copy_link(self, marker):
        ''' Copy the link.'''
        link = self.get_marker_link(marker)
        if link != "":
            clipboard = QApplication.clipboard()
            clipboard.setText(link)
            self.buffer.message_to_emacs.emit("Copy link")

    def get_code_markers(self):
        ''' Get the code markers.'''
        self.eval_js(self.get_codes_raw.replace("%1", self.buffer.emacs_var_dict["eaf-marker-letters"]));

    def get_code_content(self, marker):
        ''' Get the code content according to marker.'''
        self.goto_code_js = self.goto_code_raw.replace("%1", str(marker));
        content = self.execute_js(self.goto_code_js)
        self.cleanup_links()
        return content

    def copy_code_content(self, marker):
        ''' Copy the code content according to marker.'''
        content = self.get_code_content(marker)
        if content != "":
            clipboard = QApplication.clipboard()
            clipboard.setText(content)
            self.buffer.message_to_emacs.emit("Copy code")

    def get_focus_text(self):
        ''' Get the focus text.'''
        return self.execute_js(self.get_focus_text_js)

    @interactive()
    def set_focus_text(self, new_text):
        ''' Set the focus text.'''
        self.set_focus_text_js = self.set_focus_text_raw.replace("%1", string_to_base64(new_text));
        self.eval_js(self.set_focus_text_js)

    @interactive(insert_or_do=True)
    def focus_input(self):
        ''' input in focus.'''
        self.execute_js(self.focus_input_js)

    @interactive()
    def clear_focus(self):
        ''' Clear the focus.'''
        self.eval_js(self.clear_focus_js)

    @interactive()
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

        self.add_widget(BrowserView(buffer_id, config_dir))

        self.config_dir = config_dir
        self.page_closed = False

        self.autofill = PasswordDb(os.path.join(os.path.dirname(config_dir), "browser", "password.db"))
        self.autofill_id = 0

        self.history_list = []
        if self.emacs_var_dict["eaf-browser-remember-history"] == "true":
            self.history_log_file_path = os.path.join(self.config_dir, "browser", "history", "log.txt")

            self.history_pattern = re.compile("^(.+)ᛝ(.+)ᛡ(.+)$")
            self.noprefix_url_pattern = re.compile("^(https?|file)://(.+)")
            self.nopostfix_url_pattern = re.compile("^[^#\?]*")
            self.history_close_file_path = os.path.join(self.config_dir, "browser", "history", "close.txt")
            touch(self.history_log_file_path)
            with open(self.history_log_file_path, "r") as f:
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
        self.caret_browsing_activated = False
        self.caret_browsing_mark_activated = False
        self.caret_browsing_search_text = ""
        self.progressbar_progress = 0
        self.progressbar_color = QColor(233, 129, 35, 255)
        self.progressbar_height = 2
        self.light_mode_mask_color = QColor("#FFFFFF")
        self.dark_mode_mask_color = QColor("#242525")

        self.current_url = ""
        self.request_url = ""
        self.no_need_draw_background = False

        self.init_background_color()

        self.buffer_widget.loadStarted.connect(self.start_progress)
        self.buffer_widget.loadProgress.connect(self.update_progress)
        self.buffer_widget.urlChanged.connect(self.record_url)
        self.buffer_widget.web_page.windowCloseRequested.connect(self.close_buffer)
        self.buffer_widget.web_page.fullScreenRequested.connect(self.handle_fullscreen_request)
        self.buffer_widget.web_page.pdfPrintingFinished.connect(self.notify_print_message)
        self.profile.defaultProfile().downloadRequested.connect(self.handle_download_request)

        settings = QWebEngineSettings.globalSettings()
        try:
            settings.setAttribute(QWebEngineSettings.PluginsEnabled, self.emacs_var_dict["eaf-browser-enable-plugin"] == "true")
            settings.setAttribute(QWebEngineSettings.JavascriptEnabled, self.emacs_var_dict["eaf-browser-enable-javascript"] == "true")
            settings.setAttribute(QWebEngineSettings.FullScreenSupportEnabled, True)
            settings.setAttribute(QWebEngineSettings.PlaybackRequiresUserGesture, False)
            settings.setAttribute(QWebEngineSettings.DnsPrefetchEnabled, True)
        except Exception:
            pass

        self.build_all_methods(self.buffer_widget)
        self.build_all_methods(self)
        self.build_interactive_method(self.buffer_widget, "back", "history_backward", insert_or_do=True)
        self.build_interactive_method(self.buffer_widget, "forward", "history_forward", insert_or_do=True)

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

        if self.dark_mode_is_enable():
            current_urls = self.current_url.rsplit("/", 1)
            request_urls = self.request_url.rsplit("/", 1)
            # Emacs-china forum thread don't need draw background that avoid flash.
            if self.request_url == "https://emacs-china.org/":
                self.no_need_draw_background = False
            if self.current_url.startswith("https://emacs-china.org/t/") and self.request_url.startswith("https://emacs-china.org/t/"):
                self.no_need_draw_background = current_urls[0] == request_urls[0] or self.request_url == current_urls[0]
            elif self.current_url.startswith("https://livebook.manning.com/book/") and self.request_url.startswith("https://livebook.manning.com/book/"):
                self.no_need_draw_background = current_urls[0] == request_urls[0]
            elif self.current_url.startswith("https://web.telegram.org") and self.request_url.startswith("https://web.telegram.org"):
                self.no_need_draw_background = True

    def dark_mode_is_enable(self):
        ''' Return bool of whether dark mode is enabled.'''
        module_name = self.module_path.split(".")[1]
        return (self.emacs_var_dict["eaf-browser-dark-mode"] == "true" or \
                (self.emacs_var_dict["eaf-browser-dark-mode"] == "follow" and self.emacs_var_dict["eaf-emacs-theme-mode"] == "dark")) \
                and module_name in ["browser", "terminal", "mindmap", "js-video-player"] \
                and self.url != "devtools://devtools/bundled/devtools_app.html"

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
            self.buffer_widget.eval_js(self.buffer_widget.caret_browsing_js)
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
        else:
            self.exit_fullscreen_request.emit()

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

    @interactive(insert_or_do=True)
    def save_as_pdf(self):
        ''' Request to save as pdf.'''
        self.send_input_message("Save current webpage as PDF?", "save_as_pdf", "yes-or-no")

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
        self.close_page.emit(self.buffer_widget.url().toString())

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

    def handle_input_message(self, result_tag, result_content):
        ''' Handle input message.'''
        if result_tag == "search_text_forward":
            self.buffer_widget._search_text(str(result_content))
        elif result_tag == "search_text_backward":
            self.buffer_widget._search_text(str(result_content), True)
        elif result_tag == "caret_search_text_forward":
            self._caret_search_text(str(result_content))
        elif result_tag == "caret_search_text_backward":
            self._caret_search_text(str(result_content), True)
        elif result_tag == "jump_link":
            self.buffer_widget.jump_to_link(str(result_content).strip())
        elif result_tag == "jump_link_new_buffer":
            self.buffer_widget.jump_to_link_new_buffer(str(result_content).strip())
        elif result_tag == "jump_link_background_buffer":
            self.buffer_widget.jump_to_link_background_buffer(str(result_content).strip())
        elif result_tag == "copy_link":
            self.buffer_widget.copy_link(str(result_content).strip())
        elif result_tag == "eval_js_file":
            self.buffer_widget.eval_js_file(str(result_content))
        elif result_tag == "eval_js":
            self.buffer_widget.eval_js(str(result_content))
        elif result_tag == "save_as_pdf":
            parsed = urlparse(self.url)
            qd = parse_qs(parsed.query, keep_blank_values=True)
            pdf_path = os.path.join(os.path.expanduser(self.emacs_var_dict["eaf-browser-download-path"]), "{}.pdf".format(parsed.netloc))
            self.message_to_emacs.emit("Saving as pdf...")
            self.buffer_widget.web_page.printToPdf(pdf_path)
        elif result_tag == "save_as_single_file":
            parsed = urlparse(self.url)
            qd = parse_qs(parsed.query, keep_blank_values=True)
            file_path = os.path.join(os.path.expanduser(self.emacs_var_dict["eaf-browser-download-path"]), "{}.html".format(parsed.netloc))
            self.message_to_emacs.emit("Saving as single file...")
            args = ["monolith", self.url, "-o", file_path]
            handler = partial(self.notify_monolith_message, self.emacs_var_dict["eaf-browser-download-path"], file_path, self.title)
            call_and_check_code(args, handler)
        elif result_tag == "edit_url":
            self.buffer_widget.open_url(str(result_content))
        elif result_tag == "copy_code":
            self.buffer_widget.copy_code_content(str(result_content).strip())

    def cancel_input_message(self, result_tag):
        ''' Cancel input message.'''
        if result_tag == "jump_link" or \
           result_tag == "jump_link_new_buffer" or \
           result_tag == "jump_link_background_buffer" or \
           result_tag == "copy_link" or \
           result_tag == "edit_url":
            self.buffer_widget.cleanup_links()

    def clear_cookies(self):
        ''' Clear all cookies.'''
        self.buffer_widget.clear_cookies()
        self.message_to_emacs.emit("Cleared all cookies.")

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

    def caret_browsing(self):
        ''' Init caret browsing.'''
        if self.eval_caret_js:
            self.buffer_widget.eval_js("CaretBrowsing.setInitialCursor();")
            self.message_to_emacs.emit("Caret browsing activated.")
            self.caret_browsing_activated = True
            self.caret_browsing_search_text = ""

    def caret_exit(self):
        ''' Exit caret browsing.'''
        if self.caret_browsing_activated:
            self.buffer_widget.eval_js("CaretBrowsing.shutdown();")
            self.message_to_emacs.emit("Caret browsing deactivated.")
            self.caret_browsing_activated = False

    @interactive(insert_or_do=True)
    def caret_next_line(self):
        ''' Switch to next line in caret browsing.'''
        if self.caret_browsing_activated:
            self.buffer_widget.eval_js("CaretBrowsing.move('forward', 'line');")

    @interactive(insert_or_do=True)
    def caret_previous_line(self):
        ''' Switch to previous line in caret browsing.'''
        if self.caret_browsing_activated:
            self.buffer_widget.eval_js("CaretBrowsing.move('backward', 'line');")

    @interactive(insert_or_do=True)
    def caret_next_character(self):
        ''' Switch to next character in caret browsing.'''
        if self.caret_browsing_activated:
            self.buffer_widget.eval_js("CaretBrowsing.move('forward', 'character');")

    @interactive(insert_or_do=True)
    def caret_previous_character(self):
        ''' Switch to previous character in caret browsing.'''
        if self.caret_browsing_activated:
            self.buffer_widget.eval_js("CaretBrowsing.move('backward', 'character');")

    @interactive(insert_or_do=True)
    def caret_next_word(self):
        ''' Switch to next word in caret browsing.'''
        if self.caret_browsing_activated:
            self.buffer_widget.eval_js("CaretBrowsing.move('forward', 'word');")
            
    @interactive(insert_or_do=True)
    def caret_previous_word(self):
        ''' Switch to previous word in caret browsing.'''
        if self.caret_browsing_activated:
            self.buffer_widget.eval_js("CaretBrowsing.move('backward', 'word');")

    @interactive(insert_or_do=True)
    def caret_to_bottom(self):
        ''' Switch to next word in caret browsing.'''
        if self.caret_browsing_activated:
            self.buffer_widget.eval_js("CaretBrowsing.move('forward', 'documentboundary');")
            
    @interactive(insert_or_do=True)
    def caret_to_top(self):
        ''' Switch to previous word in caret browsing.'''
        if self.caret_browsing_activated:
            self.buffer_widget.eval_js("CaretBrowsing.move('backward', 'documentboundary');")
            
    def caret_toggle_mark(self):
        ''' Toggle mark in caret browsing.'''
        if self.caret_browsing_activated:
            self.buffer_widget.eval_js("CaretBrowsing.toggleMark();")
            if self.buffer_widget.execute_js("CaretBrowsing.markEnabled"):
                self.caret_browsing_mark_activated = True
                self.message_to_emacs.emit("Mark is on.")
            else:
                self.caret_browsing_mark_activated = False
                self.message_to_emacs.emit("Mark is off.")

    def caret_clear_search(self):
        ''' Clear search text in caret browsing.'''
        if self.caret_browsing_activated:
            if self.caret_browsing_mark_activated:
                self.caret_browsing_search_text = ""
                self.message_to_emacs.emit("Cleared caret search text.")

    @interactive(insert_or_do=True)
    def caret_search_forward(self):
        ''' Search Text forward in caret browsing.'''
        if self.caret_browsing_activated:
            if self.caret_browsing_mark_activated:
                if self.caret_browsing_search_text == "":
                    self.send_input_message("Forward Search Text and Select: ", "caret_search_text_forward")
                else:
                    self._caret_search_text(self.caret_browsing_search_text)

    @interactive(insert_or_do=True)
    def caret_search_backward(self):
        ''' Search Text backward in caret browsing.'''
        if self.caret_browsing_activated:
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

    @interactive(insert_or_do=True)
    def open_download_manage_page(self):
        ''' Open download manage page.'''
        self.try_start_aria2_daemon()
        self.buffer_widget.open_download_manage_page()

    def copy_text(self):
        ''' Copy selected text.'''
        self.buffer_widget.copy_text()
        self.message_to_emacs.emit("Copy selected text.")

    @interactive(insert_or_do=True)
    def copy_code(self):
        ''' Copy code.'''
        self.buffer_widget.get_code_markers()
        self.send_input_message("Copy code: ", "copy_code");

    @interactive(insert_or_do=True)
    def open_link(self):
        ''' Open Link.'''
        self.buffer_widget.get_link_markers()
        self.send_input_message("Open Link: ", "jump_link");

    @interactive(insert_or_do=True)
    def open_link_new_buffer(self):
        ''' Open Link in New Buffer.'''
        self.buffer_widget.get_link_markers()
        self.send_input_message("Open Link in New Buffer: ", "jump_link_new_buffer");

    @interactive(insert_or_do=True)
    def open_link_background_buffer(self):
        ''' Open Link in Background Buffer.'''
        self.buffer_widget.get_link_markers()
        self.send_input_message("Open Link in Background Buffer: ", "jump_link_background_buffer");

    @interactive(insert_or_do=True)
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
            self.buffer_widget.setZoomFactor(float(self.emacs_var_dict["eaf-browser-default-zoom"]))

    def edit_focus_text(self):
        ''' Edit the focus text.'''
        text = self.buffer_widget.get_focus_text()
        if text != None:
            self.get_focus_text.emit(self.buffer_id, text)
        else:
            self.message_to_emacs.emit("No active input element.")

    def is_focus(self):
        ''' Return bool of whether the buffer is focused.'''
        return self.buffer_widget.get_focus_text() != None or self.url == "devtools://devtools/bundled/devtools_app.html"

    def record_history(self, new_title):
        ''' Record browser history.'''
        new_url = self.buffer_widget.filter_url(self.buffer_widget.url().toString())
        if self.emacs_var_dict["eaf-browser-remember-history"] == "true" and self.buffer_widget.filter_title(new_title) != "" and \
           self.arguments != "temp_html_file" and new_title != "about:blank" and new_url != "about:blank":
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

                with open(self.history_log_file_path, "w") as f:
                    f.writelines(map(lambda history: history.title + "ᛝ" + history.url + "ᛡ" + str(history.hit) + "\n", self.history_list))
            except Exception:
                import traceback
                self.message_to_emacs.emit("Error in record_history: " + str(traceback.print_exc()))

    @interactive(insert_or_do=True)
    def new_blank_page(self):
        ''' Open new blank page.'''
        self.eval_in_emacs.emit('''(eaf-open \"{0}\" \"browser\" \"\" t)'''''.format(self.emacs_var_dict["eaf-browser-blank-page-url"]))

    def clear_history(self):
        ''' Clear browsing history.'''
        if os.path.exists(self.history_log_file_path):
            os.remove(self.history_log_file_path)
            self.message_to_emacs.emit("Cleared browsing history.")
        else:
            self.message_to_emacs.emit("There is no browsing history.")

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
    def open_browser(self):
        ''' Open browser.'''
        self.eval_in_emacs.emit('''(call-interactively 'eaf-open-browser-with-history)''')

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
        ''' Eval JS.'''
        self.send_input_message("Eval JS: ", "eval_js")

    def open_dev_tool_page(self):
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
        url = self.buffer_widget.url().toString()
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
