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

from PyQt6 import QtCore
from PyQt6.QtCore import QUrl, Qt, QEvent, QEventLoop, QTimer, QFile, QPointF, QPoint
from PyQt6.QtWebChannel import QWebChannel
from PyQt6.QtWebEngineWidgets import QWebEngineView
from PyQt6.QtWebEngineCore import QWebEnginePage, QWebEngineScript, QWebEngineProfile, QWebEngineSettings
from PyQt6.QtWidgets import QApplication, QWidget
from core.buffer import Buffer
from core.utils import (touch, string_to_base64, popen_and_call,
                        call_and_check_code, interactive,
                        eval_in_emacs, message_to_emacs, clear_emacs_message,
                        open_url_in_background_tab, duplicate_page_in_new_tab,
                        open_url_in_new_tab, open_url_in_new_tab_other_window,
                        focus_emacs_buffer, atomic_edit, get_emacs_config_dir,
                        to_camel_case, get_emacs_vars, PostGui)
from urllib.parse import urlparse, parse_qs
import base64
import os
import platform

def webengine_init():
    global eaf_profile
    eaf_profile = QWebEngineProfile("Default", QApplication.instance())
    eaf_profile.setCachePath("")
    eaf_profile.setHttpCacheType(QWebEngineProfile.HttpCacheType.DiskHttpCache)
    eaf_profile.setPersistentStoragePath("")
    eaf_profile.setPersistentCookiesPolicy(QWebEngineProfile.PersistentCookiesPolicy.AllowPersistentCookies)

MOUSE_LEFT_BUTTON = 1
MOUSE_WHEEL_BUTTON = 4
MOUSE_BACK_BUTTON = 8
MOUSE_FORWARD_BUTTON = 16

class BrowserView(QWebEngineView):

    translate_selected_text = QtCore.pyqtSignal(str)

    def __init__(self, buffer_id):
        super(QWebEngineView, self).__init__()

        self.installEventFilter(self)
        self.buffer_id = buffer_id
        self.is_button_press = False

        self.web_page = BrowserPage(self)
        self.setPage(self.web_page)

        self.url_hovered = ""
        self.page().linkHovered.connect(self.link_hovered)

        self.selectionChanged.connect(self.select_text_change)

        # Cookie init.
        # self.cookies_manager = CookiesManager(self)

        self.search_term = ""

        self.buildin_js_dir = os.path.join(os.path.dirname(__file__), "js")

        self.marker_js_raw = None
        self.get_focus_text_js = None
        self.set_focus_text_raw = None
        self.clear_focus_js = None
        self.select_input_text_js = None
        self.get_selection_text_js = None
        self.focus_input_js = None
        self.simulated_wheel_event = False

        (self.default_zoom, self.show_hover_link,
         self.marker_letters, self.marker_fontsize,
         self.scroll_step) = get_emacs_vars(
            ["eaf-webengine-default-zoom",
             "eaf-webengine-show-hover-link",
             "eaf-marker-letters",
             "eaf-marker-fontsize",
             "eaf-webengine-scroll-step"])

    def delete_all_cookies(self):
        # self.cookies_manager.delete_all_cookies()
        eaf_profile.cookieStore().deleteAllCookies()

    def delete_cookie(self):
        # self.cookies_manager.delete_cookie()
        # eaf_profile.cookieStore().deleteCookie() # TODO: only delete cookie for this view
        pass

    def load_css(self, path, name):
        path = QFile(path)
        if not path.open(QFile.OpenModeFlag.ReadOnly | QFile.OpenModeFlag.Text):
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
        self.web_page.runJavaScript(SCRIPT, QWebEngineScript.ScriptWorldId.ApplicationWorld)
        script.setName(name)
        script.setSourceCode(SCRIPT)
        script.setInjectionPoint(QWebEngineScript.InjectionPoint.DocumentReady)
        script.setRunsOnSubFrames(True)
        script.setWorldId(QWebEngineScript.ScriptWorldId.ApplicationWorld)
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
            self.web_page.runJavaScript(SCRIPT, QWebEngineScript.ScriptWorldId.ApplicationWorld)

        script = self.web_page.scripts().findScript(name)
        self.web_page.scripts().remove(script)

    def read_js_content(self, js_file):
        ''' Read content of JavaScript(js) files.'''
        return open(os.path.join(self.buildin_js_dir, js_file), "r").read()

    def filter_url(self, url):
        ''' Filter url, avoid duplicate url for the same keyword.'''
        parsed = urlparse(url)
        qd = parse_qs(parsed.query, keep_blank_values=True)
        if parsed.netloc.startswith("www.google.com"):
            # Only save search parameters for Google, avoid duplicate url for same keyword.
            filtered = dict((k, v) for k, v in qd.items() if k.startswith("q"))
        else:
            filtered = dict((k, v) for k, v in qd.items())

        from urllib.parse import urlunparse, urlencode

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

    def callback_text_search(self, found):
        ''' Callback of text search'''
        if self.search_term != "" and (not found):
            message_to_emacs("No ocurrence of \"" + self.search_term + "\" is found.", False)

    def clean_search_and_select(self):
        self.search_term = ""
        self.web_page.findText("")
        QTimer().singleShot(0, lambda : self.triggerPageAction(self.web_page.WebAction.Unselect))

    def clean_search(self):
        self.search_term = ""
        self.web_page.findText("")

    def _search_text(self, text, is_backward = False):
        self.search_term = text

        if is_backward:
            self.web_page.findText(self.search_term, self.web_page.FindFlag.FindBackward,
                                   self.callback_text_search)
        else:
            self.web_page.findText(self.search_term, resultCallback = self.callback_text_search)

    @interactive
    def search_text_forward(self):
        ''' Forward Search Text.'''
        self.buffer.send_input_message("Forward Search Text: ", "search_text_forward", "search")

    @interactive
    def search_text_backward(self):
        ''' Backward Search Text.'''
        self.buffer.send_input_message("Backward Search Text: ", "search_text_backward", "search")

    @interactive
    def action_quit(self):
        ''' Quit action.'''
        # Clean search selections if search text is not empty.
        self.clean_search_and_select()

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
            if modifiers == Qt.KeyboardModifier.ControlModifier and self.selectedText().strip() != "":
                self.translate_selected_text.emit(self.selectedText())

    def event(self, event):
        ''' Catch event.'''
        if event.type() == QEvent.Type.ChildAdded:
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

        if event.type() in [QEvent.Type.MouseButtonPress]:
            self.is_button_press = True
        elif event.type() in [QEvent.Type.MouseButtonRelease]:
            self.is_button_press = False

        # Focus emacs buffer when user click view.
        event_type = [QEvent.Type.MouseButtonPress, QEvent.Type.MouseButtonRelease, QEvent.Type.MouseButtonDblClick]
        if platform.system() != "Darwin":
            event_type += [QEvent.Type.Wheel]

        if event.type() == QEvent.Type.MouseButtonRelease:
            self.buffer.is_focus()
            
        if event.type() in event_type:
            if self.simulated_wheel_event:
               self.simulated_wheel_event = False
            else:
                focus_emacs_buffer(self.buffer_id)

        if event.type() == QEvent.Type.MouseButtonPress:

            if platform.system() == "Darwin":
                eval_in_emacs('eaf-activate-emacs-window', [])

            if event.button() == MOUSE_FORWARD_BUTTON:
                modifiers = QApplication.keyboardModifiers()
                if modifiers == Qt.KeyboardModifier.ControlModifier:
                    self.switch_to_next_webpage_buffer()
                else:
                    self.forward()

                event.accept()
                return True

            elif event.button() == MOUSE_BACK_BUTTON:
                modifiers = QApplication.keyboardModifiers()
                if modifiers == Qt.KeyboardModifier.ControlModifier:
                    self.switch_to_previous_webpage_buffer()
                else:
                    self.back()

                event.accept()
                return True

            elif event.button() == MOUSE_LEFT_BUTTON:
                modifiers = QApplication.keyboardModifiers()
                if modifiers == Qt.KeyboardModifier.ControlModifier and self.url_hovered:
                    self.open_url_background_buffer(self.url_hovered)
                    return True

            elif event.button() == MOUSE_WHEEL_BUTTON:
                if self.url_hovered:
                    self.open_url_new_buffer_other_window(self.url_hovered)
                    return True


        if event.type() == QEvent.Type.Wheel:
            modifiers = QApplication.keyboardModifiers()
            if modifiers == Qt.KeyboardModifier.ControlModifier:
                if event.angleDelta().y() > 0:
                    self.zoom_in()
                else:
                    self.zoom_out()

        return super(QWebEngineView, self).eventFilter(obj, event)

    def link_hovered(self, url):
        self.url_hovered = url

        if self.show_hover_link:
            if url:
                message_to_emacs(url, True, False)
            else:
                clear_emacs_message()

        return True

    def open_url(self, url):
        ''' Configure current url.'''
        self.setUrl(QUrl(url))
        
        eval_in_emacs('eaf-activate-emacs-window', [])

    def open_url_new_buffer(self, url):
        ''' Open url in a new tab.'''
        open_url_in_new_tab(url)

        eval_in_emacs('eaf-activate-emacs-window', [])

    def open_url_new_buffer_other_window(self, url):
        ''' Open url in a new tab.'''
        open_url_in_new_tab_other_window(url)

        eval_in_emacs('eaf-activate-emacs-window', [])

    def open_url_background_buffer(self, url):
        ''' Open url in background tab.'''
        open_url_in_background_tab(url)

        eval_in_emacs('eaf-activate-emacs-window', [])

    def switch_to_next_webpage_buffer(self):
        ''' Switch to next web page buffer.'''
        eval_in_emacs('eaf-next-buffer-same-app', [])

        eval_in_emacs('eaf-activate-emacs-window', [])

    def switch_to_previous_webpage_buffer(self):
        ''' Switch to previous web page buffer.'''
        eval_in_emacs('eaf-previous-buffer-same-app', [])

        eval_in_emacs('eaf-activate-emacs-window', [])

    @interactive(insert_or_do=True)
    def zoom_in(self):
        ''' Zoom in.'''
        self.setZoomFactor(min(5, self.zoomFactor() + 0.25))
        if self.default_zoom == self.zoomFactor():
            self.buffer.zoom_data.delete_entry(urlparse(self.buffer.current_url).hostname)
        else:
            self.buffer.zoom_data.add_entry(urlparse(self.buffer.current_url).hostname, self.zoomFactor())

    @interactive(insert_or_do=True)
    def zoom_out(self):
        ''' Zoom out.'''
        self.setZoomFactor(max(0.25, self.zoomFactor() - 0.25))
        if self.default_zoom == self.zoomFactor():
            self.buffer.zoom_data.delete_entry(urlparse(self.buffer.current_url).hostname)
        else:
            self.buffer.zoom_data.add_entry(urlparse(self.buffer.current_url).hostname, self.zoomFactor())

    @interactive(insert_or_do=True)
    def zoom_reset(self):
        ''' Reset the magnification.'''
        self.setZoomFactor(float(self.default_zoom))

    def eval_js(self, js):
        ''' Run JavaScript.'''
        self.web_page.runJavaScript(js)

    def eval_js_file(self, js_file):
        ''' Run JavaScript from JS file.'''
        self.eval_js(self.read_js_content(js_file))

    def execute_js(self, js):
        ''' Execute JavaScript and get result.

        NOTE:
        Please use eval_js instead if JavaScript function haven't result return.
        Otherwise, execute_js will block EAF!!!
        '''
        return self.web_page.execute_javascript(js)

    def eval_js_function(self, *args):
        import json

        function_name = args[0]
        function_args = args[1:]

        format_string = ""

        for index, arg in enumerate(function_args):
            if type(arg) == str:
                format_string += '\"{}\"'.format(arg)
            else:
                format_string += '{}'.format(json.dumps(arg))

            if index != len(function_args) - 1:
                format_string += ","

        format_string = function_name + "(" + format_string + ");"

        self.web_page.runJavaScript(format_string)

    def scroll_wheel(self, x_offset, y_offset):
        from PyQt6.QtGui import QWheelEvent

        self.simulated_wheel_event = True

        pos = self.rect().center()
        global_pos = self.mapToGlobal(pos)

        event = QWheelEvent(
            QPointF(pos),
            QPointF(global_pos),
            QPoint(x_offset, y_offset),
            QPoint(x_offset, y_offset),
            Qt.MouseButton.NoButton,
            Qt.KeyboardModifier.NoModifier,
            Qt.ScrollPhase.NoScrollPhase,
            False
        )

        for widget in self.buffer.get_key_event_widgets():
            QApplication.postEvent(widget, event)

    @interactive(insert_or_do=True)
    def scroll_left(self):
        ''' Scroll to left side.'''
        self.scroll_wheel(-35, 0)

    @interactive(insert_or_do=True)
    def scroll_right(self):
        ''' Scroll to right side.'''
        self.scroll_wheel(35, 0)

    @interactive(insert_or_do=True)
    def scroll_up(self):
        ''' Scroll up.'''
        self.scroll_wheel(0, -self.scroll_step)

    @interactive(insert_or_do=True)
    def scroll_down(self):
        ''' Scroll down.'''
        self.scroll_wheel(0, self.scroll_step)

    @interactive
    def scroll_up_page(self):
        ''' Scroll page up.'''
        self.scroll_wheel(0, -self.rect().height())

    @interactive(insert_or_do=True)
    def scroll_down_page(self):
        ''' Scroll down a page.'''
        self.scroll_wheel(0, self.rect().height())

    @interactive(insert_or_do=True)
    def scroll_to_begin(self):
        ''' Scroll to the beginning.'''
        self.scroll_wheel(0, 1000000)

    @interactive(insert_or_do=True)
    def scroll_to_bottom(self):
        ''' Scroll to the bottom.'''
        self.scroll_wheel(0, -1000000)

    @interactive
    def insert_or_scroll_up_page(self):
        '''If input is focus send space key to insert space.
        If browser is fullscreen, send space key to play/pause video.

        Otherwise, scroll page up.
        '''
        if self.buffer.is_focus() or self.buffer.is_fullscreen:
            self.buffer.send_key(self.buffer.current_event_string)
        else:
            self.scroll_up_page()

    @interactive
    def get_selection_text(self):
        ''' Get the selected text.'''
        if self.get_selection_text_js == None:
            self.get_selection_text_js = self.read_js_content("get_selection_text.js")

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
        self.triggerPageAction(self.web_page.WebAction.Copy)
        if self.buffer.caret_browsing_mode and self.buffer.caret_browsing_mark_activated:
            self.buffer.caret_exit()

    @interactive()
    def yank_text(self):
        ''' Paste selected text.'''
        self.triggerPageAction(self.web_page.WebAction.Paste)
        message_to_emacs("Yank selected text.")

    @interactive()
    def kill_text(self):
        ''' Cut selected text.'''
        self.triggerPageAction(self.web_page.WebAction.Cut)
        message_to_emacs("Kill selected text.")

    @interactive
    def undo_action(self):
        ''' Undo action.'''
        self.triggerPageAction(self.web_page.WebAction.Undo)

    @interactive
    def redo_action(self):
        ''' Redo action.'''
        self.triggerPageAction(self.web_page.WebAction.Redo)

    @interactive
    def exit_fullscreen(self):
        ''' Exit full screen.'''
        self.triggerPageAction(self.web_page.WebAction.ExitFullScreen)

    @interactive(insert_or_do=True)
    def view_source(self):
        ''' View source.'''
        self.triggerPageAction(self.web_page.WebAction.ViewSource)

    def select_all(self):
        ''' Select all text.'''
        # We need window focus before select all text.
        self.eval_js("window.focus()")
        self.triggerPageAction(self.web_page.WebAction.SelectAll)

    def select_input_text(self):
        ''' Select input text.'''
        if self.select_input_text_js == None:
            self.select_input_text_js = self.read_js_content("select_input_text.js")

        self.eval_js(self.select_input_text_js)

    @interactive
    def get_url(self):
        ''' Get current url.'''
        return self.url().toString().replace(" ", "%20")

    def load_marker_file(self):
        if self.marker_js_raw == None:
            self.marker_js_raw = self.read_js_content("marker.js")

        self.eval_js(self.marker_js_raw
                     .replace("%{marker_letters}", self.marker_letters)
                     .replace("%{marker_offset_x}", str(self.buffer.marker_offset_x()))
                     .replace("%{marker_offset_y}", str(self.buffer.marker_offset_y()))
                     .replace("%{marker_fontsize}", str(self.marker_fontsize)))

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
        if link is None or type(link).__name__ == "QVariant" or link.startswith("eaf::"):
            return False
        else:
            return link
        
    def _open_link(self, marker):
        ''' Jump to link according to marker.'''
        link = self.get_marker_link(marker)
        if link: self.open_url(link)            

    def _open_link_new_buffer(self, marker):
        ''' Open the link at the marker in a new buffer.'''
        link = self.get_marker_link(marker)
        if link: self.open_url_new_buffer(link)

    def _open_link_new_buffer_other_window(self, marker):
        ''' Open the link at the marker in a new buffer in other window.'''
        link = self.get_marker_link(marker)
        if link: self.open_url_new_buffer_other_window(link)

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
        self.eval_js("Marker.gotoMarker('%s', (e) => window.getSelection().collapse(e, 0))" % str(marker))
        self.cleanup_links_dom()

        markEnabled = self.execute_js("CaretBrowsing.markEnabled")
        # reset to clear caret state so the next sentence can be marked
        if markEnabled:
            self.eval_js("CaretBrowsing.shutdown();")

        self.buffer.caret_enable_mark()
        self.buffer.caret_next_sentence()
        self.buffer.caret_browsing_mode = True

        eval_in_emacs('eaf--toggle-caret-browsing', ["'t"])

    def copy_code_content(self, marker):
        ''' Copy the code content according to marker.'''
        content = self.get_code_content(marker)
        if content != "":
            self.buffer.set_clipboard_text(content)
            message_to_emacs("Copied code block!")

    def get_focus_text(self):
        ''' Get the focus text.'''
        if self.get_focus_text_js == None:
            self.get_focus_text_js = self.read_js_content("get_focus_text.js")

        return self.execute_js(self.get_focus_text_js)

    @interactive
    def set_focus_text(self, new_text):
        ''' Set the focus text.'''
        new_text = base64.b64decode(new_text).decode("utf-8")

        if self.set_focus_text_raw == None:
            self.set_focus_text_raw = self.read_js_content("set_focus_text.js")

        self.set_focus_text_js = self.set_focus_text_raw.replace("%{new_text_base64}", string_to_base64(new_text));
        self.eval_js(self.set_focus_text_js)

    @interactive(insert_or_do=True)
    def focus_input(self):
        ''' input in focus.'''
        if self.focus_input_js == None:
            self.focus_input_js = self.read_js_content("focus_input.js")

        self.eval_js(self.focus_input_js)
        eval_in_emacs('eaf-update-focus-state', [self.buffer_id, "'t"])

    @interactive
    def clear_focus(self):
        ''' Clear the focus.'''
        if self.clear_focus_js == None:
            self.clear_focus_js = self.read_js_content("clear_focus.js")

        self.eval_js(self.clear_focus_js)
        eval_in_emacs('eaf-update-focus-state', [self.buffer_id, "'nil"])

    def init_dark_mode_js(self, module_path, selection_color="auto"):
        js_string = open(os.path.join(os.path.dirname(module_path), "node_modules", "darkreader", "darkreader.js")).read()

        if selection_color != "auto":
            js_string = js_string.replace("selectionColor: 'auto'", "selectionColor: '" + selection_color + "'")

        js_string += """DarkReader.setFetchMethod(window.fetch); DarkReader.enable({brightness: 100, contrast: 90, sepia: 10});"""

        self.dark_mode_inject_js = QWebEngineScript()
        self.dark_mode_inject_js.setInjectionPoint(QWebEngineScript.InjectionPoint.DocumentCreation)
        self.dark_mode_inject_js.setWorldId(QWebEngineScript.ScriptWorldId.MainWorld)
        self.dark_mode_inject_js.setName("dark_mode_inject.js")
        self.dark_mode_inject_js.setRunsOnSubFrames(True)
        self.dark_mode_inject_js.setSourceCode(js_string)

        if self.buffer.dark_mode_is_enabled():
            self.enable_dark_mode()

    @interactive(insert_or_do=True)
    def enable_dark_mode(self):
        ''' Dark mode support.'''
        self.page().scripts().insert(self.dark_mode_inject_js)

    @interactive(insert_or_do=True)
    def disable_dark_mode(self):
        ''' Remove dark mode support.'''
        self.page().scripts().remove(self.dark_mode_inject_js)

class BrowserPage(QWebEnginePage):
    def __init__(self, view: BrowserView):
        QWebEnginePage.__init__(self, eaf_profile, view)

    def execute_javascript(self, script_src):
        ''' Execute JavaScript.'''
        if hasattr(self, "loop") and self.loop.isRunning():
            # NOTE:
            #
            # Just return None is QEventLoop is busy, such as press 'j' key not release on webpage.
            # Otherwise will got error 'RecursionError: maximum recursion depth exceeded while calling a Python object'.
            #
            # And don't warry, API 'execute_javascript' is works well for programming purpse since we just call this interface occasionally.
            return None
        else:
            # Build event loop.
            self.loop = QEventLoop()

            # Run JavaScript code.
            self.runJavaScript(script_src, self.callback_js)

            # Execute event loop, and wait event loop quit.
            self.loop.exec()

            # Return JavaScript function result.
            return self.result

    def callback_js(self, result):
        ''' Callback of JavaScript, call loop.quit to jump code after loop.exec.'''
        self.result = result
        self.loop.quit()

class BrowserBuffer(Buffer):

    close_page = QtCore.pyqtSignal(str)
    open_devtools_tab = QtCore.pyqtSignal(object)

    def __init__(self, buffer_id, url, arguments, fit_to_view):
        Buffer.__init__(self, buffer_id, url, arguments, fit_to_view)

        self.add_widget(BrowserView(buffer_id))

        self.url = url

        self.config_dir = get_emacs_config_dir()
        self.page_closed = False

        self.zoom_data = ZoomSizeDb(os.path.join(os.path.dirname(self.config_dir), "browser", "zoom_data.db"))

        (self.pc_user_agent,
         self.phone_user_agent,
         self.font_family,
         self.fixed_font_family,
         self.serif_font_family,
         self.font_size,
         self.fixed_font_size,
         self.enable_plugin,
         self.enable_javascript,
         self.enable_javascript_access_clipboard,
         self.enable_scrollbar,
         self.unknown_url_scheme_policy,
         self.download_path,
         self.default_zoom) = get_emacs_vars(
             ["eaf-webengine-pc-user-agent",
              "eaf-webengine-phone-user-agent",
              "eaf-webengine-font-family",
              "eaf-webengine-fixed-font-family",
              "eaf-webengine-serif-font-family",
              "eaf-webengine-font-size",
              "eaf-webengine-fixed-font-size",
              "eaf-webengine-enable-plugin",
              "eaf-webengine-enable-javascript",
              "eaf-webengine-enable-javascript-access-clipboard",
              "eaf-webengine-enable-scrollbar",
              "eaf-webengine-unknown-url-scheme-policy",
              "eaf-webengine-download-path",
              "eaf-webengine-default-zoom"])

        eaf_profile.setHttpUserAgent(self.pc_user_agent)

        self.caret_js_ready = False
        self.caret_browsing_mode = False
        self.caret_browsing_exit_flag = True
        self.caret_browsing_mark_activated = False
        self.caret_browsing_search_text = ""
        self.is_dark_mode_enabled = self.dark_mode_is_enabled()

        self.current_url = ""
        self.request_url = ""

        self.buffer_widget.web_page.windowCloseRequested.connect(self.close_buffer)
        self.buffer_widget.web_page.fullScreenRequested.connect(self.handle_fullscreen_request)
        self.buffer_widget.web_page.pdfPrintingFinished.connect(self.notify_print_message)
        eaf_profile.downloadRequested.connect(self.handle_download_request)

        self.settings = self.buffer_widget.settings()
        try:

            if self.font_family:
                for ff in (
                        self.settings.FontFamily.StandardFont,
                        self.settings.FontFamily.CursiveFont,
                        self.settings.FontFamily.FantasyFont,
                        self.settings.FontFamily.PictographFont):
                    self.settings.setFontFamily(ff, self.font_family)

            if self.fixed_font_family:
                self.settings.setFontFamily(self.settings.FontFamily.FixedFont, self.fixed_font_family)

            if self.serif_font_family:
                for ff in (
                        self.settings.FontFamily.SerifFont,
                        self.settings.FontFamily.SansSerifFont):
                    self.settings.setFontFamily(ff, self.serif_font_family)

            self.settings.setFontSize(QWebEngineSettings.FontSize.DefaultFontSize, self.font_size)
            self.settings.setFontSize(QWebEngineSettings.FontSize.DefaultFixedFontSize, self.fixed_font_size)

            self.settings.setAttribute(QWebEngineSettings.WebAttribute.FullScreenSupportEnabled, True)
            self.settings.setAttribute(QWebEngineSettings.WebAttribute.DnsPrefetchEnabled, True)
            self.settings.setAttribute(QWebEngineSettings.WebAttribute.FocusOnNavigationEnabled, True)
            self.settings.setAttribute(QWebEngineSettings.WebAttribute.PlaybackRequiresUserGesture, False)
            self.settings.setAttribute(QWebEngineSettings.WebAttribute.PluginsEnabled, self.enable_plugin)
            self.settings.setAttribute(QWebEngineSettings.WebAttribute.JavascriptEnabled, self.enable_javascript)
            self.settings.setAttribute(QWebEngineSettings.WebAttribute.JavascriptCanAccessClipboard, self.enable_javascript_access_clipboard)
            self.settings.setAttribute(QWebEngineSettings.WebAttribute.ShowScrollBars, self.enable_scrollbar)

            if self.unknown_url_scheme_policy == "DisallowUnknownUrlSchemes":
                self.settings.setUnknownUrlSchemePolicy(self.settings.UnknownUrlSchemePolicy.DisallowUnknownUrlSchemes)
            elif self.unknown_url_scheme_policy == "AllowUnknownUrlSchemesFromUserInteraction":
                self.settings.setUnknownUrlSchemePolicy(self.settings.UnknownUrlSchemePolicy.AllowUnknownUrlSchemesFromUserInteraction)
            elif self.unknown_url_scheme_policy == "AllowAllUnknownUrlSchemes":
                self.settings.setUnknownUrlSchemePolicy(self.settings.UnknownUrlSchemePolicy.AllowAllUnknownUrlSchemes)

        except Exception:
            import traceback
            traceback.print_exc()

        self.build_all_methods(self.buffer_widget)
        self.build_all_methods(self)

        # Reset with HiDPI.
        self.buffer_widget.zoom_reset()

        # Build webchannel object.
        QtCore.qInstallMessageHandler(self.filter_instant_message)
        self.channel = QWebChannel()
        self.channel.registerObject("pyobject", self)
        self.buffer_widget.web_page.setWebChannel(self.channel)

    def add_widget(self, widget):
        super(BrowserBuffer, self).add_widget(widget)
        # Set web page background.
        if hasattr(self.buffer_widget, "web_page"):
            self.init_web_page_background()

    def fetch_marker_callback(self):
        try:
            return list(map(lambda x: x.lower(),
                            self.buffer_widget.execute_js("Array.from(document.getElementsByClassName(\"eaf-marker\")).map(function(e) { return e.id });")))
        except Exception:
            return None

    def filter_instant_message(self, *args):
        # Disable QWebChannel warnings.
        if not args[-1].endswith('value updates in HTML will be broken!'):
            print("".join(list(map(str, args[2:]))))

    def notify_print_message(self, file_path, success):
        ''' Notify the print as pdf message.'''
        if success:
            # Try to rename pdf file with title.
            # Use host name if title include invalid file char.
            title_path = os.path.join(os.path.expanduser(self.download_path), "{}.pdf".format(self.title))
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
        return False

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
            image_path = os.path.join(os.path.expanduser(self.download_path), "image.png")
            touch(image_path)
            with open(image_path, "wb") as f:
                b64bytes = download_data.split(",")[1].encode("utf-8")
                f.write(base64.b64decode(b64bytes))

            message_to_emacs("Save image: " + image_path)
        else:
            if hasattr(self, "try_start_aria2_daemon"):
                self.try_start_aria2_daemon()

                from core.pyaria2 import Jsonrpc

                download_url = download_item.url().toString()
                jsonrpc = Jsonrpc('localhost', 6800)
                resp = jsonrpc.addUris(download_url)

                message_to_emacs("Downloading: " + download_url)

    def _save_as_pdf(self):
        parsed = urlparse(self.url)
        qd = parse_qs(parsed.query, keep_blank_values=True)
        pdf_path = os.path.join(os.path.expanduser(self.download_path), "{}.pdf".format(parsed.netloc))
        message_to_emacs("Saving as pdf...")
        self.buffer_widget.web_page.printToPdf(pdf_path)

    @interactive(insert_or_do=True)
    def save_as_pdf(self):
        ''' Request to save as pdf.'''
        self.send_input_message("Save current webpage as PDF?", "save_as_pdf", "yes-or-no")

    def _save_as_single_file(self):
        from functools import partial

        parsed = urlparse(self.url)
        qd = parse_qs(parsed.query, keep_blank_values=True)
        file_path = os.path.join(os.path.expanduser(self.download_path), "{}.html".format(parsed.netloc))
        message_to_emacs("Saving as single file...")
        args = ["monolith", self.url, "-o", file_path]
        handler = partial(self.notify_monolith_message, self.download_path, file_path, self.title)
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
        screenshot_path = os.path.join(os.path.expanduser(self.download_path), "{}.png".format(self.title))
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
        self.buffer_widget.setUrl(QUrl("about:blank"))

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
        elif callback_tag == "open_link_new_buffer_other_window":
            self.buffer_widget._open_link_new_buffer_other_window(result_content.strip())
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
           callback_tag == "open_link_new_buffer_other_window" or \
           callback_tag == "jump_link_background_buffer" or \
           callback_tag == "select_marker_text" or \
           callback_tag == "caret_at_line" or \
           callback_tag == "copy_link" or \
           callback_tag == "copy_code" or \
           callback_tag == "edit_url":
            self.buffer_widget.cleanup_links_dom()
        elif callback_tag == "search_text_forward" or callback_tag == "search_text_backward":
            self.buffer_widget.clean_search_and_select()

    def handle_search_forward(self, callback_tag):
        if callback_tag == "search_text_forward" or callback_tag == "search_text_backward":
            self.buffer_widget._search_text(self.buffer_widget.search_term)

    def handle_search_backward(self, callback_tag):
        if callback_tag == "search_text_forward" or callback_tag == "search_text_backward":
            self.buffer_widget._search_text(self.buffer_widget.search_term, True)

    def handle_search_finish(self, callback_tag):
        if callback_tag == "search_text_forward" or callback_tag == "search_text_backward":
            self.buffer_widget.clean_search()

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
    def caret_enable_mark(self):
        ''' Toggle mark in caret browsing.'''
        if self.caret_browsing_mode:
            self.caret_browsing_mark_activated = True
            if not self.buffer_widget.execute_js("CaretBrowsing.markEnabled"):
                self.buffer_widget.eval_js("CaretBrowsing.toggleMark();")
                message_to_emacs("Caret Mark set")
        else:
            message_to_emacs("Not in Caret Browsing mode!")

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
        self.send_input_message("Copy code: ", "copy_code", "marker");

    @interactive(insert_or_do=True)
    def select_text(self):
        ''' Select Text.'''
        self.buffer_widget.get_text_markers()
        self.send_input_message("Select Text: ", "select_marker_text", "marker");

    @interactive(insert_or_do=True)
    def caret_at_line(self):
        self.buffer_widget.get_text_markers()
        self.send_input_message("Toggle Caret Browsing at Line: ", "caret_at_line", "marker");

    @interactive(insert_or_do=True)
    def open_link(self):
        ''' Open Link through a marker.'''
        self.buffer_widget.get_link_markers()
        self.send_input_message("Open Link: ", "open_link", "marker");

    @interactive(insert_or_do=True)
    def open_link_new_buffer(self):
        ''' Open Link in New Buffer.'''
        self.buffer_widget.get_link_markers()
        self.send_input_message("Open Link in New Buffer: ", "open_link_new_buffer", "marker");

    @interactive(insert_or_do=True)
    def open_link_new_buffer_other_window(self):
        ''' Open Link in New Buffer in Other Window.'''
        self.buffer_widget.get_link_markers()
        self.send_input_message("Open Link in New Buffer in Other Window: ", "open_link_new_buffer_other_window", "marker");

    @interactive(insert_or_do=True)
    def open_link_background_buffer(self):
        ''' Open Link in Background Buffer.'''
        self.buffer_widget.get_link_markers()
        self.send_input_message("Open Link in Background Buffer: ", "jump_link_background_buffer", "marker");

    @interactive
    def copy_link(self):
        ''' Copy link.'''
        self.buffer_widget.get_link_markers()
        self.send_input_message("Copy link: ", "copy_link", "marker");

    @interactive(insert_or_do=True)
    def edit_url(self):
        ''' Edit link.'''
        self.send_input_message("Edit link: ", "edit_url", "string", self.url)

    def reset_default_zoom(self):
        ''' Reset default magnification.'''
        if hasattr(self, "buffer_widget"):
            result = self.zoom_data.get_entry(urlparse(self.url).hostname)
            zoom_factor = self.default_zoom
            for row in result:
                zoom_factor = float(row[0])

            self.buffer_widget.setZoomFactor(float(zoom_factor))

    def atomic_edit(self):
        ''' Edit the focus text.'''
        text = self.buffer_widget.get_focus_text()
        if text != None:
            atomic_edit(self.buffer_id, text)
        else:
            message_to_emacs("No active input element.")

    def is_focus(self):
        ''' Return bool of whether the buffer is focused.'''
        input_focus = self.buffer_widget.get_focus_text() != None or self.url.startswith("devtools://")
        
        eval_in_emacs("eaf-update-focus-state", [self.buffer_id, input_focus])                    
        
        return input_focus

    @interactive(insert_or_do=True)
    def duplicate_page(self):
        duplicate_page_in_new_tab(self.url)

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
        user_agent = eaf_profile.httpUserAgent()
        if user_agent == self.pc_user_agent:
            eaf_profile.setHttpUserAgent(self.phone_user_agent)
            self.set_aspect_ratio(2.0 / 3)
        else:
            eaf_profile.setHttpUserAgent(self.pc_user_agent)
            self.set_aspect_ratio(0)

        self.refresh_page()

    @interactive
    def toggle_dark_mode(self):
        self.is_dark_mode_enabled = not self.is_dark_mode_enabled

        if self.is_dark_mode_enabled:
            self.buffer_widget.enable_dark_mode()
        else:
            self.buffer_widget.disable_dark_mode()

        self.buffer_widget.reload()

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
                download_path = "{}/%(title)s-%(id)s.%(ext)s".format(os.path.expanduser(self.download_path))

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

    def eval_js_function(self, function_name, function_arguments):
        ''' Eval JavaScript function.'''
        if function_arguments == "":
            self.buffer_widget.eval_js('''{}()'''.format(to_camel_case(function_name)))
        else:
            self.buffer_widget.eval_js('''{}({})'''.format(to_camel_case(function_name), function_arguments))

    def execute_js_function(self, function_name, function_arguments):
        ''' Execute JavaScript function and return result.'''
        if function_arguments == "":
            return self.buffer_widget.execute_js('''{}()'''.format(to_camel_case(function_name)))
        else:
            return self.buffer_widget.execute_js('''{}({})'''.format(to_camel_case(function_name), function_arguments))

    def eval_js_code(self, js_code):
        ''' Eval JavaScript code.'''
        self.buffer_widget.eval_js(js_code)

    def execute_js_code(self, js_code):
        ''' Execute JavaScript code and return result.'''
        return self.buffer_widget.execute_js(js_code)

    def init_app(self):
        pass

    def load_index_html(self, app_file):
        self.buffer_widget.loadFinished.connect(self.init_app)

        self.index_file_dir = os.path.join(os.path.dirname(app_file), "dist")
        self.index_file = os.path.join(self.index_file_dir, "index.html")

        with open(self.index_file, "r") as f:
            html = self.convert_index_html(f.read(), self.index_file_dir)
            self.buffer_widget.setHtml(html, QUrl("file://"))

    def convert_index_html(self, index_file_content, dist_dir):
        '''
        Convert path to absolute path and change body background.
        '''
        import lxml.html as LH

        root = LH.fromstring(index_file_content)
        for el in root.iter('link'):
            el.attrib['href'] = "{}{}".format(dist_dir, el.attrib['href'])
        for el in root.iter('script'):
            el.attrib['src'] = "{}{}".format(dist_dir, el.attrib['src'])
            el.attrib['type'] = "text/javascript"
        for el in root.iter('body'):
            el.attrib['style'] = "background: {}; color: {}".format(self.theme_background_color, self.theme_foreground_color)

        return LH.tostring(root, encoding=str)

    @QtCore.pyqtSlot(str, list)
    def eval_emacs_function(self, function_name, function_arguments):
        eval_in_emacs(function_name, function_arguments)

    def init_web_page_background(self):
        # Web page background follow Emacs's background.
        self.buffer_widget.web_page.setBackgroundColor(self.background_color)

    @interactive(insert_or_do=True)
    def change_url(self, url):
        # _change_url use PostGui make sure change url in main thread.
        self._change_url(url)

    @PostGui()
    def _change_url(self, url):
        self.buffer_widget.stop()
        self.buffer_widget.setUrl(QUrl(url))

    def marker_offset_x(self):
        return 0

    def marker_offset_y(self):
        return 0

class ZoomSizeDb(object):
    def __init__(self, dbpath):
        import sqlite3

        if not os.path.exists(os.path.dirname(dbpath)):
            os.makedirs(os.path.dirname(dbpath))

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

class CookiesManager(object):
    def __init__(self, browser_view):
        self.browser_view = browser_view

        self.cookies_dir = os.path.join(get_emacs_config_dir(), "browser", "cookies")

        # Both session and persistent cookies are stored in memory
        self.browser_view.page().profile().setPersistentCookiesPolicy(QWebEngineProfile.PersistentCookiesPolicy.NoPersistentCookies)

        self.cookie_store = self.browser_view.page().profile().cookieStore()

        self.cookie_store.cookieAdded.connect(self.add_cookie)      # save cookie to disk when captured cookieAdded signal
        self.cookie_store.cookieRemoved.connect(self.remove_cookie) # remove cookie stored on disk when captured cookieRemoved signal
        self.browser_view.loadStarted.connect(self.load_cookie)     # load disk cookie to QWebEngineView instance when page start load


    def add_cookie(self, cookie):
        '''Store cookie on disk.'''
        cookie_domain = cookie.domain()
        if not cookie.isSessionCookie():
            cookie_file = os.path.join(self.cookies_dir, cookie_domain, self._generate_cookie_filename(cookie))
            touch(cookie_file)

            # Save newest cookie to disk.
            with open(cookie_file, "wb") as f:
                f.write(cookie.toRawForm())

    def load_cookie(self):
        ''' Load cookie file from disk.'''
        if not os.path.exists(self.cookies_dir):
            return

        all_cookies_domain = os.listdir(self.cookies_dir)

        for domain in filter(self.domain_matching, all_cookies_domain):
            from PyQt6.QtNetwork import QNetworkCookie

            domain_dir = os.path.join(self.cookies_dir, domain)

            for cookie_file in os.listdir(domain_dir):
                with open(os.path.join(domain_dir, cookie_file), "rb") as f:
                    for cookie in QNetworkCookie.parseCookies(f.read()):
                        if not domain.startswith('.'):
                            if self.browser_view.url().host() == domain:
                                # restore host-only cookie
                                cookie.setDomain('')
                                self.cookie_store.setCookie(cookie, self.browser_view.url())
                        else:
                            self.cookie_store.setCookie(cookie)

    def remove_cookie(self, cookie):
        ''' Delete cookie file.'''
        if not cookie.isSessionCookie():
            cookie_file = os.path.join(self.cookies_dir, cookie.domain(), self._generate_cookie_filename(cookie))

            if os.path.exists(cookie_file):
                os.remove(cookie_file)

    def delete_all_cookies(self):
        ''' Simply delete all cookies stored on memory and disk.'''
        self.cookie_store.deleteAllCookies()
        if os.path.exists(self.cookies_dir):
            import shutil
            shutil.rmtree(self.cookies_dir)

    def delete_cookie(self):
        ''' Delete all cookie used by current site except session cookies.'''
        from PyQt6.QtNetwork import QNetworkCookie
        import shutil

        cookies_domain = os.listdir(self.cookies_dir)

        for domain in filter(self.get_relate_domains, cookies_domain):
            domain_dir = os.path.join(self.cookies_dir, domain)

            for cookie_file in os.listdir(domain_dir):
                with open(os.path.join(domain_dir, cookie_file), "rb") as f:
                    for cookie in QNetworkCookie.parseCookies(f.read()):
                        self.cookie_store.deleteCookie(cookie)
            shutil.rmtree(domain_dir)

    def domain_matching(self, cookie_domain):
        ''' Check if a given cookie's domain is matching for host string.'''

        cookie_is_hostOnly = True
        if cookie_domain.startswith('.'):
            # get rid of prefixing dot when matching domains
            cookie_domain = cookie_domain[1:]
            cookie_is_hostOnly = False

        host_string = self.browser_view.url().host()

        if cookie_domain == host_string:
            # The domain string and the host string are identical
            return True

        if len(host_string) < len(cookie_domain):
            # For obvious reasons, the host string cannot be a suffix if the domain
            # is shorter than the domain string
            return False

        if host_string.endswith(cookie_domain) and host_string[:-len(cookie_domain)][-1] == '.' and not cookie_is_hostOnly:
            # The domain string should be a suffix of the host string,
            # The last character of the host string that is not included in the
            # domain string should be a %x2E (".") character.
            # and cookie domain not have prefixing dot (host-only cookie is not for subdomains)
            return True

        return False

    def get_relate_domains(self, cookie_domain):
        ''' Check whether the cookie domain is located under the same root host as the current URL host.'''
        import tld, re

        host_string = self.browser_view.url().host()

        if cookie_domain.startswith('.'):
            cookie_domain = cookie_domain[1:]

        base_domain = tld.get_fld(host_string, fix_protocol=True, fail_silently=True)

        if not base_domain:
            # check whether host string is an IP address
            if re.compile('^((25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(25[0-5]|2[0-4]\d|[01]?\d\d?)$').match(host_string) and host_string == cookie_domain:
                return True
            return  False

        if cookie_domain == base_domain:
            return True

        if cookie_domain.endswith(base_domain) and cookie_domain[:-len(base_domain)][-1] == '.':
            return True

        return False

    def _generate_cookie_filename(self, cookie):
        ''' Gets the name of the cookie file stored on the hard disk.'''
        name = cookie.name().data().decode("utf-8")
        domain = cookie.domain()
        encode_path = cookie.path().replace("/", "|")

        return name + "+" + domain + "+" + encode_path
