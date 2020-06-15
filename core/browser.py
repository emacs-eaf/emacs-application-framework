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
from PyQt5.QtCore import QUrl, Qt, QEvent, QPointF, QEventLoop, QVariant, QTimer, QRectF
from PyQt5.QtGui import QBrush, QColor
from PyQt5.QtNetwork import QNetworkCookie
from PyQt5.QtWebEngineWidgets import QWebEngineView, QWebEnginePage, QWebEngineContextMenuData, QWebEngineProfile, QWebEngineSettings
from PyQt5.QtWidgets import QApplication, QWidget
from core.utils import touch, is_port_in_use, string_to_base64, popen_and_call, call_and_check_code
from core.buffer import Buffer
from urllib.parse import urlparse, parse_qs, urlunparse, urlencode
import os
import subprocess
import re
import base64
from functools import partial


MOUSE_BACK_BUTTON = 8
MOUSE_FORWARD_BUTTON = 16

class BrowserView(QWebEngineView):

    open_url_in_new_tab = QtCore.pyqtSignal(str)
    open_url_in_background_tab = QtCore.pyqtSignal(str)
    translate_selected_text = QtCore.pyqtSignal(str)
    trigger_focus_event = QtCore.pyqtSignal(str)

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
        self.get_selection_text_js = self.read_js_content("get_selection_text.js")
        self.focus_input_js = self.read_js_content("focus_input.js")

    def open_download_manage_page(self):
        self.open_url_new_buffer("file://" + (os.path.join(os.path.dirname(__file__), "aria2-webui", "index.html")))

    def read_js_content(self, js_file):
        return open(os.path.join(os.path.dirname(__file__), "js", js_file), "r").read()

    def filter_url(self, url):
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

    def search_text_forward(self):
        if self.search_term == "":
            self.buffer.send_input_message("Forward Search Text: ", "search_text_forward")
        else:
            self._search_text(self.search_term)

    def search_text_backward(self):
        if self.search_term == "":
            self.buffer.send_input_message("Backward Search Text: ", "search_text_backward")
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
        # Focus emacs buffer when user click view.
        if event.type() in [QEvent.MouseButtonPress, QEvent.MouseButtonRelease,
                            QEvent.MouseMove, QEvent.MouseButtonDblClick, QEvent.Wheel]:
            # Send mouse event to applicatin view.
            self.trigger_focus_event.emit("{0},{1}".format(event.globalX(), event.globalY()))

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
        self.setZoomFactor(float(self.buffer.emacs_var_dict["eaf-browser-default-zoom"]))

    def eval_js(self, js):
        self.web_page.runJavaScript(js)

    def eval_js_file(self, js_file):
        self.eval_js(self.read_js_content(js_file))

    def execute_js(self, js):
        return self.web_page.execute_javascript(js)

    def scroll_left(self):
        self.eval_js("document.scrollingElement.scrollBy(-35, 0)")

    def scroll_right(self):
        self.eval_js("document.scrollingElement.scrollBy(35, 0)")

    def scroll_up(self):
        self.eval_js("document.scrollingElement.scrollBy(0, 50)")

    def scroll_down(self):
        self.eval_js("document.scrollingElement.scrollBy(0, -50)")

    def scroll_up_page(self):
        self.eval_js("document.scrollingElement.scrollBy({left: 0, top: window.innerHeight/2, behavior: '" + self.buffer.emacs_var_dict["eaf-browser-scroll-behavior"] + "'})")

    def scroll_down_page(self):
        self.eval_js("document.scrollingElement.scrollBy({left: 0, top: -window.innerHeight/2, behavior: '" + self.buffer.emacs_var_dict["eaf-browser-scroll-behavior"] + "'})")

    def scroll_to_begin(self):
        self.eval_js("document.scrollingElement.scrollTo({left: 0, top: 0, behavior: '" + self.buffer.emacs_var_dict["eaf-browser-scroll-behavior"] + "'})")

    def scroll_to_bottom(self):
        self.eval_js("document.scrollingElement.scrollTo({left: 0, top: document.body.scrollHeight, behavior: '" + self.buffer.emacs_var_dict["eaf-browser-scroll-behavior"] + "'})")

    def get_selection_text(self):
        return self.execute_js(self.get_selection_text_js)

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

    def exit_fullscreen(self):
        self.triggerPageAction(self.web_page.ExitFullScreen)

    def view_source(self):
        self.triggerPageAction(self.web_page.ViewSource)

    def select_all(self):
        # We need window focus before select all text.
        self.eval_js("window.focus()")
        self.triggerPageAction(self.web_page.SelectAll)

    def select_input_text(self):
        self.eval_js(self.select_input_text_js)

    def get_url(self):
        return self.execute_js("window.location.href;")

    def cleanup_links(self):
        self.eval_js("document.querySelector('.eaf-marker-container').remove();")
        self.eval_js("document.querySelector('.eaf-style').remove();")

    def get_link_markers(self):
        self.eval_js(self.get_markers_raw.replace("%1", self.buffer.emacs_var_dict["eaf-marker-letters"]));

    def get_marker_link(self, marker):
        self.goto_marker_js = self.goto_marker_raw.replace("%1", str(marker));
        link = self.execute_js(self.goto_marker_js)
        self.cleanup_links()
        return link

    def jump_to_link(self, marker):
        link = self.get_marker_link(marker)
        if link != "":
            self.open_url(link)

    def jump_to_link_new_buffer(self, marker):
        link = self.get_marker_link(marker)
        if link != "":
            self.open_url_new_buffer(link)

    def jump_to_link_background_buffer(self, marker):
        link = self.get_marker_link(marker)
        if link != "":
            self.open_url_background_buffer(link)

    def copy_link(self, marker):
        link = self.get_marker_link(marker)
        if link != "":
            clipboard = QApplication.clipboard()
            clipboard.setText(link)
            self.buffer.message_to_emacs.emit("Copy link")

    def get_code_markers(self):
        self.eval_js(self.get_codes_raw.replace("%1", self.buffer.emacs_var_dict["eaf-marker-letters"]));

    def get_code_content(self, marker):
        self.goto_code_js = self.goto_code_raw.replace("%1", str(marker));
        content = self.execute_js(self.goto_code_js)
        self.cleanup_links()
        return content

    def copy_code_content(self, marker):
        content = self.get_code_content(marker)
        if content != "":
            clipboard = QApplication.clipboard()
            clipboard.setText(content)
            self.buffer.message_to_emacs.emit("Copy code")

    def get_focus_text(self):
        return self.execute_js(self.get_focus_text_js)

    def set_focus_text(self, new_text):
        self.set_focus_text_js = self.set_focus_text_raw.replace("%1", string_to_base64(new_text));
        self.eval_js(self.set_focus_text_js)

    def focus_input(self):
        self.execute_js(self.focus_input_js)

    def clear_focus(self):
        self.eval_js(self.clear_focus_js)

    def dark_mode(self):
        self.eval_js(self.dark_mode_js)

class BrowserPage(QWebEnginePage):
    def __init__(self):
        QWebEnginePage.__init__(self)

    def execute_javascript(self, script_src):
        self.loop = QEventLoop()
        self.result = QVariant()
        QTimer.singleShot(250, self.loop.quit)

        self.runJavaScript(script_src, self.callback_js)
        self.loop.exec_()
        self.loop = None
        return self.result

    def callback_js(self, res):
        if self.loop is not None and self.loop.isRunning():
            self.result = res
            self.loop.quit()

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

        self.add_widget(BrowserView(config_dir))

        self.config_dir = config_dir

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

        for method_name in ["search_text_forward", "search_text_backward", "zoom_out", "zoom_in", "zoom_reset",
                            "scroll_left", "scroll_right", "scroll_up", "scroll_down",
                            "scroll_up_page", "scroll_down_page", "scroll_to_begin", "scroll_to_bottom",
                            "refresh_page", "undo_action", "redo_action", "get_url", "exit_fullscreen",
                            "set_focus_text", "clear_focus", "dark_mode", "view_source", "focus_input"]:
            self.build_widget_method(method_name)

        self.build_widget_method("history_backward", "back")
        self.build_widget_method("history_forward", "forward")
        self.build_widget_method("action_quit", "search_quit")
        self.build_widget_method("yank_text", "yank_text", "Yank text.")
        self.build_widget_method("kill_text", "kill_text", "Kill text.")

        for method_name in ["recover_prev_close_page", "scroll_up", "scroll_down", "scroll_left", "scroll_right",
                            "scroll_up_page", "scroll_down_page", "scroll_to_begin", "scroll_to_bottom",
                            "open_link", "open_link_new_buffer", "open_link_background_buffer", "copy_link",
                            "history_backward", "history_forward", "new_blank_page", "open_download_manage_page",
                            "refresh_page", "zoom_in", "zoom_out", "zoom_reset", "save_as_bookmark", "edit_url",
                            "download_youtube_video", "download_youtube_audio", "toggle_device", "close_buffer",
                            "save_as_pdf", "view_source", "save_as_single_file", "select_left_tab", "select_right_tab",
                            "copy_code", "focus_input"]:
            self.build_insert_or_do(method_name)

    def notify_print_message(self, file_path, success):
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
        self.request_url = url.toString()

        # Emacs-china forum thread don't need draw background that avoid flash.
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

    def dark_mode_is_enable(self):
        module_name = self.module_path.split(".")[1]
        return (self.emacs_var_dict["eaf-browser-dark-mode"] == "true" or \
                (self.emacs_var_dict["eaf-browser-dark-mode"] == "" and self.emacs_var_dict["eaf-emacs-theme-mode"] == "dark")) \
                and module_name in ["browser", "terminal", "mindmap", "js-video-player"] \
                and self.url != "devtools://devtools/bundled/devtools_app.html"

    def init_background_color(self):
        if self.dark_mode_is_enable():
            self.buffer_widget.web_page.setBackgroundColor(self.dark_mode_mask_color)
        else:
            self.buffer_widget.web_page.setBackgroundColor(self.light_mode_mask_color)

    def drawForeground(self, painter, rect):
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
        self.progressbar_progress = 0
        self.draw_progressbar = True
        self.update()

    @QtCore.pyqtSlot()
    def hide_progress(self):
        self.current_url = self.url
        self.no_need_draw_background = False

        self.draw_progressbar = False
        self.eval_dark_js = False
        self.update()

    @QtCore.pyqtSlot(int)
    def update_progress(self, progress):
        if progress < 100:
            # Update progres.
            self.progressbar_progress = progress
            self.update()
        elif progress == 100 and self.draw_progressbar:
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
        if request.toggleOn():
            self.enter_fullscreen_request.emit()
        else:
            self.exit_fullscreen_request.emit()

        request.accept()

    def handle_download_request(self, download_item):
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

    def save_as_pdf(self):
        self.send_input_message("Save current webpage as PDF?", "save_as_pdf", "yes-or-no")

    def save_as_single_file(self):
        import shutil
        if shutil.which("monolith") is None:
            self.message_to_emacs.emit("Executable monolith not in PATH")
        else:
            self.send_input_message("Save current webpage as single html file?", "save_as_single_file", "yes-or-no")

    def destroy_buffer(self):
        # Record close page.
        self.close_page.emit(self.buffer_widget.url().toString())

        # Load blank page to stop video playing, such as youtube.com.
        self.buffer_widget.open_url("about:blank")

        if self.buffer_widget is not None:
            # NOTE: We need delete QWebEnginePage manual, otherwise QtWebEngineProcess won't quit.
            self.buffer_widget.web_page.deleteLater()
            self.buffer_widget.deleteLater()

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

    def handle_input_message(self, result_tag, result_content):
        if result_tag == "search_text_forward":
            self.buffer_widget._search_text(str(result_content))
        elif result_tag == "search_text_backward":
            self.buffer_widget._search_text(str(result_content), True)
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
        if result_tag == "jump_link" or \
           result_tag == "jump_link_new_buffer" or \
           result_tag == "jump_link_background_buffer" or \
           result_tag == "copy_link" or \
           result_tag == "edit_url":
            self.buffer_widget.cleanup_links()

    def clear_cookies(self):
        self.buffer_widget.clear_cookies()
        self.message_to_emacs.emit("Cleared all cookies.")

    def try_start_aria2_daemon(self):
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

    def open_download_manage_page(self):
        self.try_start_aria2_daemon()
        self.buffer_widget.open_download_manage_page()

    def copy_text(self):
        self.buffer_widget.copy_text()
        self.message_to_emacs.emit("Copy selected text.")

    def copy_code(self):
        self.buffer_widget.get_code_markers()
        self.send_input_message("Copy code: ", "copy_code");

    def open_link(self):
        self.buffer_widget.get_link_markers()
        self.send_input_message("Open Link: ", "jump_link");

    def open_link_new_buffer(self):
        self.buffer_widget.get_link_markers()
        self.send_input_message("Open Link in New Buffer: ", "jump_link_new_buffer");

    def open_link_background_buffer(self):
        self.buffer_widget.get_link_markers()
        self.send_input_message("Open Link in Background Buffer: ", "jump_link_background_buffer");

    def copy_link(self):
        self.buffer_widget.get_link_markers()
        self.send_input_message("Copy link: ", "copy_link");

    def edit_url(self):
        self.send_input_message("Edit link: ", "edit_url", "string", self.url)

    def reset_default_zoom(self):
        if hasattr(self, "buffer_widget"):
            self.buffer_widget.setZoomFactor(float(self.emacs_var_dict["eaf-browser-default-zoom"]))

    def edit_focus_text(self):
        text = self.buffer_widget.get_focus_text()
        if text != None:
            self.get_focus_text.emit(self.buffer_id, text)
        else:
            self.message_to_emacs.emit("No active input element.")

    def is_focus(self):
        return self.buffer_widget.get_focus_text() != None or self.url == "devtools://devtools/bundled/devtools_app.html"

    def record_history(self, new_title):
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

    def new_blank_page(self):
        self.eval_in_emacs.emit('''(eaf-open \"{0}\" \"browser\" \"\" t)'''''.format(self.emacs_var_dict["eaf-browser-blank-page-url"]))

    def clear_history(self):
        if os.path.exists(self.history_log_file_path):
            os.remove(self.history_log_file_path)
            self.message_to_emacs.emit("Cleared browsing history.")
        else:
            self.message_to_emacs.emit("There is no browsing history.")

    def record_close_page(self, url):
        if self.emacs_var_dict["eaf-browser-remember-history"] == "true":
            touch(self.history_close_file_path)
            with open(self.history_close_file_path, "a") as f:
                f.write("{0}\n".format(url))

    def recover_prev_close_page(self):
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

    def insert_or_do(func):
        def _do(self, *args, **kwargs):
            if self.is_focus():
                self.fake_key_event(self.current_event_string)
            else:
                func(self, *args, **kwargs)
        return _do

    def build_insert_or_do(self, method_name):
        def _do ():
            if self.is_focus():
                self.fake_key_event(self.current_event_string)
            else:
                getattr(self, method_name)()
        setattr(self, "insert_or_{}".format(method_name), _do)

    @insert_or_do
    def insert_or_open_url(self):
        self.eval_in_emacs.emit('''(call-interactively 'eaf-open-browser-with-history)''')

    def select_all_or_input_text(self):
        if self.is_focus():
            self.buffer_widget.select_input_text()
        else:
            self.buffer_widget.select_all()

    def eval_js_file(self):
        self.send_input_message("Eval JS file: ", "eval_js_file", "file")

    def eval_js(self):
        self.send_input_message("Eval JS: ", "eval_js")

    def open_dev_tool_page(self):
        self.open_dev_tools_tab.emit(self.buffer_widget.web_page)

    def toggle_device(self):
        user_agent = self.profile.defaultProfile().httpUserAgent()
        if user_agent == self.pc_user_agent:
            self.profile.defaultProfile().setHttpUserAgent(self.phone_user_agent)
            self.set_aspect_ratio(2.0 / 3)
        else:
            self.profile.defaultProfile().setHttpUserAgent(self.pc_user_agent)
            self.set_aspect_ratio(0)

        self.refresh_page()

    def download_youtube_video(self):
        self.download_youtube_file()

    def download_youtube_audio(self):
        self.download_youtube_file(True)

    def download_youtube_file(self, only_audio=False):
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
