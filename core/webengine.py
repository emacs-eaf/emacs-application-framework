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
from PyQt5.QtCore import QUrl, Qt, QEvent, QEventLoop, QVariant, QTimer, QFile
from PyQt5.QtGui import QColor, QScreen
from PyQt5.QtNetwork import QNetworkCookie
from PyQt5.QtWebEngineWidgets import QWebEngineView, QWebEnginePage, QWebEngineScript, QWebEngineProfile, QWebEngineSettings
from PyQt5.QtWidgets import QApplication, QWidget
from PyQt5.QtWebChannel import QWebChannel
from core.buffer import Buffer
from core.utils import touch, string_to_base64, popen_and_call, call_and_check_code, interactive, abstract, eval_in_emacs, message_to_emacs, open_url_in_background_tab, duplicate_page_in_new_tab, open_url_in_new_tab, focus_emacs_buffer, atomic_edit, get_emacs_var
from functools import partial
from urllib.parse import urlparse, parse_qs, urlunparse, urlencode
import base64
import os
import platform
import sqlite3

MOUSE_BACK_BUTTON = 8
MOUSE_FORWARD_BUTTON = 16

class BrowserView(QWebEngineView):

    translate_selected_text = QtCore.pyqtSignal(str)

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

        self.urlChanged.connect(lambda url: self.action_quit())

        self.load_cookie()

        self.search_term = ""

        self.marker_js_raw = self.read_js_content("marker.js")
        self.get_focus_text_js = self.read_js_content("get_focus_text.js")
        self.set_focus_text_raw = self.read_js_content("set_focus_text.js")
        self.clear_focus_js = self.read_js_content("clear_focus.js")
        self.select_input_text_js = self.read_js_content("select_input_text.js")
        self.dark_mode_js = open(os.path.join(os.path.dirname(os.path.dirname(__file__)),
                                              "node_modules",
                                              "darkreader",
                                              "darkreader.js")).read()
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
        # Debug event.
        # if event.type() != 1:
        #     import time
        #     print(time.time(), event.type(), self.rect())

        # Focus emacs buffer when user click view.
        event_type = [QEvent.MouseButtonPress, QEvent.MouseButtonRelease, QEvent.MouseButtonDblClick]
        if platform.system() != "Darwin":
            event_type += [QEvent.Wheel]

        if event.type() in event_type:
            focus_emacs_buffer(self.buffer_id)

        if event.type() == QEvent.MouseButtonPress:

            if platform.system() == "Darwin":
                eval_in_emacs('eaf-activate-emacs-window', [])

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

        eval_in_emacs('eaf-activate-emacs-window', [])

    def open_url_new_buffer(self, url):
        ''' Open url in a new tab.'''
        open_url_in_new_tab(url)

        eval_in_emacs('eaf-activate-emacs-window', [])

    def open_url_background_buffer(self, url):
        ''' Open url in background tab.'''
        open_url_in_background_tab(url)

        eval_in_emacs('eaf-activate-emacs-window', [])

    @interactive(insert_or_do=True)
    def zoom_in(self):
        ''' Zoom in.'''
        self.setZoomFactor(min(5, self.zoomFactor() + 0.25))
        if get_emacs_var("eaf-browser-default-zoom") == self.zoomFactor():
            self.buffer.zoom_data.delete_entry(urlparse(self.buffer.current_url).hostname)
        else:
            self.buffer.zoom_data.add_entry(urlparse(self.buffer.current_url).hostname, self.zoomFactor())

    @interactive(insert_or_do=True)
    def zoom_out(self):
        ''' Zoom out.'''
        self.setZoomFactor(max(0.25, self.zoomFactor() - 0.25))
        if get_emacs_var("eaf-browser-default-zoom") == self.zoomFactor():
            self.buffer.zoom_data.delete_entry(urlparse(self.buffer.current_url).hostname)
        else:
            self.buffer.zoom_data.add_entry(urlparse(self.buffer.current_url).hostname, self.zoomFactor())

    @interactive(insert_or_do=True)
    def zoom_reset(self):
        ''' Reset the magnification.'''
        self.setZoomFactor(get_emacs_var("eaf-browser-default-zoom"))

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
        self.eval_js("document.scrollingElement.scrollBy({left: 0, top: window.innerHeight/2, behavior: '" + get_emacs_var("eaf-browser-scroll-behavior") + "'})")

    @interactive
    def insert_or_scroll_up_page(self):
        '''If input is focus send space key to insert space.
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
        self.eval_js("document.scrollingElement.scrollBy({left: 0, top: -window.innerHeight/2, behavior: '" + get_emacs_var("eaf-browser-scroll-behavior") + "'})")

    @interactive(insert_or_do=True)
    def scroll_to_begin(self):
        ''' Scroll to the beginning.'''
        self.eval_js("document.scrollingElement.scrollTo({left: 0, top: 0, behavior: '" + get_emacs_var("eaf-browser-scroll-behavior") + "'})")

    @interactive(insert_or_do=True)
    def scroll_to_bottom(self):
        ''' Scroll to the bottom.'''
        self.eval_js("document.scrollingElement.scrollTo({left: 0, top: document.body.scrollHeight, behavior: '" + get_emacs_var("eaf-browser-scroll-behavior") + "'})")

    @interactive
    def get_selection_text(self):
        ''' Get the selected text.'''
        return self.execute_js(self.get_selection_text_js)

    @interactive(insert_or_do=True)
    def refresh_page(self):
        ''' Refresh the page.'''
        self.reload()

        if platform.system() == "Windows":
            eval_in_emacs('eaf-activate-emacs-window', [])

    @interactive(insert_or_do=True)
    def copy_text(self):
        ''' Copy selected text.'''
        self.triggerPageAction(self.web_page.Copy)
        if self.buffer.caret_browsing_mode and self.buffer.caret_browsing_mark_activated:
            self.buffer.caret_exit()

    @interactive()
    def yank_text(self):
        ''' Paste selected text.'''
        self.triggerPageAction(self.web_page.Paste)
        message_to_emacs("Yank selected text.")

    @interactive()
    def kill_text(self):
        ''' Cut selected text.'''
        self.triggerPageAction(self.web_page.Cut)
        message_to_emacs("Kill selected text.")

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
        self.eval_js(self.marker_js_raw.replace("%1", get_emacs_var("eaf-marker-letters")))

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
            message_to_emacs("Copied " + link)

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
        eval_in_emacs('eaf--toggle-caret-browsing', ["'t" if self.buffer.caret_browsing_mode else "'nil"])
        self.buffer.caret_toggle_mark()
        self.buffer.caret_next_word()

    def copy_code_content(self, marker):
        ''' Copy the code content according to marker.'''
        content = self.get_code_content(marker)
        if content != "":
            self.buffer.set_clipboard_text(content)
            message_to_emacs("Copied code block!")

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

    @interactive
    def load_dark_mode_js(self):
        self.eval_js('''if (typeof DarkReader === 'undefined') {{ {} }} '''.format(self.dark_mode_js))

    @interactive(insert_or_do=True)
    def enable_dark_mode(self):
        ''' Dark mode support.'''
        self.eval_js("""DarkReader.setFetchMethod(window.fetch); DarkReader.enable({brightness: 100, contrast: 90, sepia: 10});""")

    @interactive(insert_or_do=True)
    def disable_dark_mode(self):
        ''' Remove dark mode support.'''
        self.eval_js("""DarkReader.disable();""")

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
    open_devtools_tab = QtCore.pyqtSignal(object)

    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path, fit_to_view):
        Buffer.__init__(self, buffer_id, url, arguments, emacs_var_dict, module_path, fit_to_view)

        self.add_widget(BrowserView(buffer_id, config_dir))

        self.config_dir = config_dir
        self.page_closed = False

        self.zoom_data = ZoomSizeDb(os.path.join(os.path.dirname(config_dir), "browser", "zoom_data.db"))

        self.pc_user_agent = get_emacs_var("eaf-browser-pc-user-agent")
        self.phone_user_agent = get_emacs_var("eaf-browser-phone-user-agent")
        self.profile = QWebEngineProfile(self.buffer_widget)
        self.profile.defaultProfile().setHttpUserAgent(self.pc_user_agent)

        self.caret_js_ready = False
        self.caret_browsing_mode = False
        self.caret_browsing_exit_flag = True
        self.caret_browsing_mark_activated = False
        self.caret_browsing_search_text = ""
        self.light_mode_mask_color = QColor("#FFFFFF")
        self.dark_mode_mask_color = QColor("#242525")
        self.is_dark_mode_enabled = self.dark_mode_is_enabled()

        self.current_url = ""
        self.request_url = ""

        self.init_background_color()

        self.buffer_widget.web_page.windowCloseRequested.connect(self.close_buffer)
        self.buffer_widget.web_page.fullScreenRequested.connect(self.handle_fullscreen_request)
        self.buffer_widget.web_page.pdfPrintingFinished.connect(self.notify_print_message)
        self.profile.defaultProfile().downloadRequested.connect(self.handle_download_request)

        self.settings = QWebEngineSettings.globalSettings()
        try:
            self.settings.setAttribute(QWebEngineSettings.PluginsEnabled, get_emacs_var("eaf-browser-enable-plugin"))
            self.settings.setAttribute(QWebEngineSettings.JavascriptEnabled, get_emacs_var("eaf-browser-enable-javascript"))
            self.settings.setAttribute(QWebEngineSettings.ShowScrollBars, get_emacs_var("eaf-browser-enable-scrollbar"))
            self.settings.setAttribute(QWebEngineSettings.FullScreenSupportEnabled, True)
            self.settings.setAttribute(QWebEngineSettings.DnsPrefetchEnabled, True)
            self.settings.setAttribute(QWebEngineSettings.FocusOnNavigationEnabled, True)
            self.settings.setAttribute(QWebEngineSettings.PlaybackRequiresUserGesture, False)

            if get_emacs_var("eaf-browser-unknown-url-scheme-policy") == "DisallowUnknownUrlSchemes":
                self.settings.setUnknownUrlSchemePolicy(self.settings.DisallowUnknownUrlSchemes)
            elif get_emacs_var("eaf-browser-unknown-url-scheme-policy") == "AllowUnknownUrlSchemesFromUserInteraction":
                self.settings.setUnknownUrlSchemePolicy(self.settings.AllowUnknownUrlSchemesFromUserInteraction)
            elif get_emacs_var("eaf-browser-unknown-url-scheme-policy") == "AllowAllUnknownUrlSchemes":
                self.settings.setUnknownUrlSchemePolicy(self.settings.AllowAllUnknownUrlSchemes)

            font_family = get_emacs_var('eaf-browser-font-family')
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

        # Reset with HiDPI.
        self.buffer_widget.zoom_reset()

        # Build webchannel object.
        self.channel = QWebChannel()
        self.channel.registerObject("pyobject", self)
        self.buffer_widget.web_page.setWebChannel(self.channel)

    def notify_print_message(self, file_path, success):
        ''' Notify the print as pdf message.'''
        if success:
            # Try to rename pdf file with title.
            # Use host name if title include invalid file char.
            title_path = os.path.join(os.path.expanduser(get_emacs_var("eaf-browser-download-path")), "{}.pdf".format(self.title))
            try:
                os.rename(file_path, title_path)
                message_to_emacs("Successfully saved current webpage as '{}'.".format(title_path))
            except Exception:
                message_to_emacs("Successfully saved current webpage as '{}'.".format(file_path))
        else:
            message_to_emacs("Failed to save current webpage as '{}'.".format(file_path))

    def notify_monolith_message(self, download_path, file_path, title, retcode):
        ''' Notify the save as html message.'''
        if retcode == 0:
            title_path = os.path.join(os.path.expanduser(download_path), "{}.html".format(title))
            try:
                os.rename(file_path, title_path)
                message_to_emacs("Successfully saved current webpage as '{}'.".format(title_path))
            except Exception:
                message_to_emacs("Successfully saved current webpage as '{}'.".format(file_path))
        else:
            message_to_emacs("Failed to save current page as single file.")

    def dark_mode_is_enabled(self):
        ''' Return bool of whether dark mode is enabled.'''
        module_name = self.module_path.split(".")[1]
        return ((get_emacs_var("eaf-browser-dark-mode") == "follow" and get_emacs_var("eaf-emacs-theme-mode") == "dark")) \
                and module_name in ["browser", "terminal", "mindmap", "js-video-player"] \
                and self.url != "devtools://devtools/bundled/devtools_app.html"

    def init_background_color(self):
        ''' Initialize the background colour.'''
        if self.dark_mode_is_enabled():
            self.buffer_widget.web_page.setBackgroundColor(self.dark_mode_mask_color)
        else:
            self.buffer_widget.web_page.setBackgroundColor(self.light_mode_mask_color)

    def handle_fullscreen_request(self, request):
        ''' Handle fullscreen request.'''
        if request.toggleOn():
            self.enter_fullscreen_request.emit()
        else:
            self.exit_fullscreen_request.emit()

        request.accept()

    def should_skip_download_item(self, download_item):
        return download_item.page() != self.buffer_widget.web_page

    def handle_download_request(self, download_item):
        ''' Handle download request.'''
        if self.should_skip_download_item(download_item):
            return

        download_data = download_item.url().toString()

        if download_data.startswith("data:image/"):
            image_path = os.path.join(os.path.expanduser(get_emacs_var("eaf-browser-download-path")), "image.png")
            touch(image_path)
            with open(image_path, "wb") as f:
                f.write(base64.decodestring(download_data.split(",")[1].encode("utf-8")))

            message_to_emacs("Save image: " + image_path)
        else:
            self.try_start_aria2_daemon()

            from core.pyaria2 import Jsonrpc

            download_url = download_item.url().toString()
            jsonrpc = Jsonrpc('localhost', 6800)
            resp = jsonrpc.addUris(download_url)

            message_to_emacs("Downloading: " + download_url)

    def _save_as_pdf(self):
        parsed = urlparse(self.url)
        qd = parse_qs(parsed.query, keep_blank_values=True)
        pdf_path = os.path.join(os.path.expanduser(get_emacs_var("eaf-browser-download-path")), "{}.pdf".format(parsed.netloc))
        message_to_emacs("Saving as pdf...")
        self.buffer_widget.web_page.printToPdf(pdf_path)

    @interactive(insert_or_do=True)
    def save_as_pdf(self):
        ''' Request to save as pdf.'''
        self.send_input_message("Save current webpage as PDF?", "save_as_pdf", "yes-or-no")

    def _save_as_single_file(self):
        parsed = urlparse(self.url)
        qd = parse_qs(parsed.query, keep_blank_values=True)
        file_path = os.path.join(os.path.expanduser(get_emacs_var("eaf-browser-download-path")), "{}.html".format(parsed.netloc))
        message_to_emacs("Saving as single file...")
        args = ["monolith", self.url, "-o", file_path]
        handler = partial(self.notify_monolith_message, get_emacs_var("eaf-browser-download-path"), file_path, self.title)
        call_and_check_code(args, handler)

    @interactive(insert_or_do=True)
    def save_as_single_file(self):
        ''' Request to save current webpage as single html file.'''
        import shutil
        if shutil.which("monolith") is None:
            message_to_emacs("Executable monolith not in PATH")
        else:
            self.send_input_message("Save current webpage as single html file?", "save_as_single_file", "yes-or-no")

    def _save_as_screenshot(self):
        screenshot_path = os.path.join(os.path.expanduser(get_emacs_var("eaf-browser-download-path")), "{}.png".format(self.title))
        message_to_emacs("Save as screenshot at {}".format(screenshot_path))
        self.buffer_widget.grab().save(screenshot_path, b'PNG')

    @interactive(insert_or_do=True)
    def save_as_screenshot(self):
        ''' Request to save current webpage as screenshot.'''
        self.send_input_message("Save current webpage as screenshot?", "save_as_screenshot", "yes-or-no")

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
        elif callback_tag == "save_as_screenshot":
            self._save_as_screenshot()
        elif callback_tag == "edit_url":
            self.buffer_widget.open_url(result_content)
        elif callback_tag == "copy_code":
            self.buffer_widget.copy_code_content(result_content.strip())
        else:
            return False
        return True

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

    def caret_toggle_browsing(self):
        ''' Init caret browsing.'''
        if self.caret_js_ready:
            if self.caret_browsing_mode:
                self.buffer_widget.eval_js("CaretBrowsing.shutdown();")
                message_to_emacs("Caret browsing deactivated.")
                self.caret_browsing_mode = False
            else:
                self.buffer_widget.eval_js("CaretBrowsing.setInitialCursor();")
                message_to_emacs("Caret browsing activated.")
                self.caret_browsing_mode = True

        eval_in_emacs('eaf--toggle-caret-browsing', ["'t" if self.caret_browsing_mode else "'nil"])

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
                message_to_emacs("Caret Mark set")
            else:
                self.caret_browsing_mark_activated = False
                message_to_emacs("Caret Mark deactivated")

    @interactive
    def caret_clear_search(self):
        ''' Clear search text in caret browsing.'''
        if self.caret_browsing_mode:
            if self.caret_browsing_mark_activated:
                self.caret_browsing_search_text = ""
                message_to_emacs("Cleared caret search text.")

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
                message_to_emacs("Unable to find more, please try forward search.")
        else:
            if not self.buffer_widget.execute_js("window.find('"+text+"')"):
                message_to_emacs("Unable to find more, please try backward search.")

    @interactive
    def caret_translate_text(self):
        self.translate_text()

    @interactive(insert_or_do=True)
    def translate_text(self):
        if self.buffer_widget.selectedText().strip() != "":
            self.buffer_widget.translate_selected_text.emit(self.buffer_widget.selectedText())

    def copy_text(self):
        ''' Copy selected text.'''
        self.buffer_widget.copy_text()
        message_to_emacs("Copied selected text.")

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
            zoom_factor = get_emacs_var("eaf-browser-default-zoom")
            for row in result:
                zoom_factor = float(row[0])

            self.buffer_widget.setZoomFactor(zoom_factor)

    def atomic_edit(self):
        ''' Edit the focus text.'''
        text = self.buffer_widget.get_focus_text()
        if text != None:
            atomic_edit(self.buffer_id, text)
        else:
            message_to_emacs("No active input element.")

    def is_focus(self):
        ''' Return bool of whether the buffer is focused.'''
        return self.buffer_widget.get_focus_text() != None or self.url == "devtools://devtools/bundled/devtools_app.html"

    @interactive(insert_or_do=True)
    def duplicate_page(self):
        duplicate_page_in_new_tab(self.current_url)

    @interactive(insert_or_do=True)
    def open_browser(self):
        ''' Open browser.'''
        eval_in_emacs('call-interactively', ['\'eaf-open-browser-with-history'])

    def select_all_or_input_text(self):
        ''' Select all or input text.'''
        if self.is_focus():
            self.buffer_widget.select_input_text()
        else:
            self.buffer_widget.select_all()

    @interactive()
    def eval_js_file(self):
        ''' Eval JS file.'''
        self.send_input_message("Eval JS file: ", "eval_js_file", "file")

    @interactive()
    def eval_js(self):
        ''' Eval JS interactively.'''
        self.send_input_message("Eval JS: ", "eval_js")

    @interactive()
    def open_devtools(self):
        ''' Open dev-tool page.'''
        self.open_devtools_tab.emit(self.buffer_widget.web_page)

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

    @interactive
    def toggle_dark_mode(self):
        self.is_dark_mode_enabled = not self.is_dark_mode_enabled
        self.buffer_widget.load_dark_mode_js()
        if self.is_dark_mode_enabled:
            self.buffer_widget.enable_dark_mode()
        else:
            self.buffer_widget.disable_dark_mode()

    @interactive(insert_or_do=True)
    def history_forward(self):
        self.buffer_widget.history().forward()

        if platform.system() == "Windows":
            eval_in_emacs('eaf-activate-emacs-window', [])

    @interactive(insert_or_do=True)
    def history_backward(self):
        self.buffer_widget.history().back()

        if platform.system() == "Windows":
            eval_in_emacs('eaf-activate-emacs-window', [])

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
                download_path = "{}/%(title)s-%(id)s.%(ext)s".format(os.path.expanduser(get_emacs_var("eaf-browser-download-path")))

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
                    popen_and_call(youtube_dl_args, lambda : message_to_emacs("Downloaded: {0}".format(url)), null_file)

                message_to_emacs("Downloading {0}: {1}".format(file_type, url))
            else:
                message_to_emacs("Please install youtube-dl to use this feature.")
        else:
            message_to_emacs("Only videos from YouTube can be downloaded for now.")

    def build_js_bridge_method(self, python_method_name, js_method_name):
        def _do():
            self.buffer_widget.execute_js('''{}()'''.format(js_method_name))

        setattr(self, python_method_name, _do)

    def convert_index_html(self, index_file_content, dist_dir):
        '''
        Convert path to absolute path and change body background.
        '''
        return index_file_content.replace(
            '''<link href=''', '''<link href=''' + dist_dir).replace(
                '''<script src=''', '''<script src=''' + dist_dir).replace(
                    '''<body>''', '''<body style="background: {}; color: {}">'''.format(
                        get_emacs_var("eaf-emacs-theme-background-color"),
                        get_emacs_var("eaf-emacs-theme-foreground-color")
                    ))

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
