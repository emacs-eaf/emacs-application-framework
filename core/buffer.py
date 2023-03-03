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

from PyQt6.QtCore import Qt, QEvent, QThread, pyqtSignal
from PyQt6.QtGui import QKeyEvent, QCursor, QFocusEvent, QColor
from PyQt6.QtWidgets import QGraphicsScene, QApplication
from core.utils import (interactive, abstract, get_clipboard_text,
                        set_clipboard_text, eval_in_emacs, message_to_emacs,
                        input_message, get_emacs_var, get_emacs_func_result,
                        get_emacs_theme_mode, get_emacs_theme_foreground, get_emacs_theme_background)
import abc
import string

qt_key_dict = {}

# Build char event.
for char in string.ascii_lowercase:
    upper_char = char.upper()
    qt_key_dict[char] = eval("Qt.Key.Key_{}".format(upper_char))
    qt_key_dict[upper_char] = eval("Qt.Key.Key_{}".format(upper_char))

# Build number event.
for number in range(0, 10):
    qt_key_dict[str(number)] = eval("Qt.Key.Key_{}".format(number))

qt_key_dict.update({
    ''':''': Qt.Key.Key_Colon,
    ''';''': Qt.Key.Key_Semicolon,
    '''.''': Qt.Key.Key_Period,
    ''',''': Qt.Key.Key_Comma,
    '''+''': Qt.Key.Key_Plus,
    '''-''': Qt.Key.Key_Minus,
    '''=''': Qt.Key.Key_Equal,
    '''_''': Qt.Key.Key_Underscore,
    '''[''': Qt.Key.Key_BracketLeft,
    ''']''': Qt.Key.Key_BracketRight,
    '''(''': Qt.Key.Key_BraceLeft,
    ''')''': Qt.Key.Key_BraceRight,
    '''{''': Qt.Key.Key_ParenLeft,
    '''}''': Qt.Key.Key_ParenRight,
    '''<''': Qt.Key.Key_Less,
    '''>''': Qt.Key.Key_Greater,
    '''@''': Qt.Key.Key_At,
    '''\\''': Qt.Key.Key_Backslash,
    '''|''': Qt.Key.Key_Bar,
    '''/''': Qt.Key.Key_Slash,
    '''#''': Qt.Key.Key_NumberSign,
    '''$''': Qt.Key.Key_Dollar,
    '''?''': Qt.Key.Key_Question,
    '''"''': Qt.Key.Key_QuoteDbl,
    '''`''': Qt.Key.Key_QuoteLeft,
    '''%''': Qt.Key.Key_Percent,
    '''^''': Qt.Key.Key_AsciiCircum,
    '''&''': Qt.Key.Key_Ampersand,
    '''*''': Qt.Key.Key_Asterisk,
    '''~''': Qt.Key.Key_AsciiTilde,
    '''!''': Qt.Key.Key_Exclam,
    '''\'''': Qt.Key.Key_Apostrophe,
    '''SPC''': Qt.Key.Key_Space,
    '''RET''': Qt.Key.Key_Return,
    '''DEL''': Qt.Key.Key_Backspace,
    '''TAB''': Qt.Key.Key_Tab,
    '''<backtab>''': Qt.Key.Key_Backtab,
    '''<home>''': Qt.Key.Key_Home,
    '''<end>''': Qt.Key.Key_End,
    '''<left>''': Qt.Key.Key_Left,
    '''<right>''': Qt.Key.Key_Right,
    '''<up>''': Qt.Key.Key_Up,
    '''<down>''': Qt.Key.Key_Down,
    '''<prior>''': Qt.Key.Key_PageUp,
    '''<next>''': Qt.Key.Key_PageDown,
    '''<delete>''': Qt.Key.Key_Delete,
    '''<backspace>''': Qt.Key.Key_Backspace,
    '''<return>''': Qt.Key.Key_Return,
    '''<escape>''': Qt.Key.Key_Escape
})

# NOTE:
# We need convert return or backspace to correct text,
# otherwise EAF browser will crash when user type return/backspace key.
qt_text_dict = {
    "SPC": " ",
    "<return>": "RET",
    "<backtab>": "",
    "<home>": "",
    "<end>": "",
    "<left>": "",
    "<right>": "",
    "<up>": "",
    "<down>": "",
    "<prior>": "",
    "<next>": "",
    "<delete>": "",
    "<backspace>": "",
    "<escape>": ""
}

class Buffer(QGraphicsScene):
    __metaclass__ = abc.ABCMeta

    aspect_ratio_change = pyqtSignal()
    enter_fullscreen_request = pyqtSignal()
    exit_fullscreen_request = pyqtSignal()

    def __init__(self, buffer_id, url, arguments, fit_to_view):
        super(QGraphicsScene, self).__init__()

        self.buffer_id = buffer_id
        self.url = url
        self.arguments = arguments
        self.fit_to_view = fit_to_view
        self.title = ""
        self.current_event_string = ""

        self.buffer_widget = None
        self.is_fullscreen = False

        self.aspect_ratio = 0
        self.vertical_padding_ratio = 1.0 / 8

        self.fetch_marker_input_thread = None
        self.fetch_search_input_thread = None

        self.theme_mode = get_emacs_theme_mode()
        self.theme_foreground_color = get_emacs_theme_foreground()
        self.theme_background_color = get_emacs_theme_background()

        self.enter_fullscreen_request.connect(self.enable_fullscreen)
        self.exit_fullscreen_request.connect(self.disable_fullscreen)

    def base_class_name(self):
        return self.__class__.__bases__[0].__name__

    def build_all_methods(self, origin_class):
        ''' Build all methods.'''
        method_list = [func for func in dir(origin_class) if callable(getattr(origin_class, func)) and not func.startswith("__")]
        for func_name in method_list:
            func_attr = getattr(origin_class, func_name)
            if hasattr(func_attr, "interactive"):
                self.build_interactive_method(
                    origin_class,
                    func_name,
                    getattr(func_attr, "new_name"),
                    getattr(func_attr, "insert_or_do"))

    def build_interactive_method(self, origin_class, class_method_name, new_method_name=None, insert_or_do=False):
        ''' Build interactive methods.'''
        new_name = class_method_name if new_method_name is None else new_method_name
        if (not hasattr(self, class_method_name)) or hasattr(getattr(self, class_method_name), "abstract"):
            self.__dict__.update({new_name: getattr(origin_class, class_method_name)})
        if insert_or_do:
            self.build_insert_or_do(new_name)

    def build_insert_or_do(self, method_name):
        ''' Build insert or do.'''
        def _do ():
            if self.is_focus():    # type: ignore
                self.send_key(self.current_event_string)
            else:
                getattr(self, method_name)()
        setattr(self, "insert_or_{}".format(method_name), _do)

    def toggle_fullscreen(self):
        ''' Toggle full screen.'''
        if self.is_fullscreen:
            self.exit_fullscreen_request.emit()
        else:
            self.enter_fullscreen_request.emit()

    def enable_fullscreen(self):
        ''' Enable full screen.'''
        self.is_fullscreen = True
        eval_in_emacs('eaf--enter-fullscreen-request', [])

    def disable_fullscreen(self):
        ''' Disable full screen.'''
        self.is_fullscreen = False
        eval_in_emacs('eaf--exit_fullscreen_request', [])

    def move_cursor_to_corner(self):
        '''
        Move cursor to bottom right corner of screen.
        '''
        screen = QApplication.instance().primaryScreen()    # type: ignore
        try:
            QCursor().setPos(screen, screen.size().width(), screen.size().height())
        except:
            # Moves the cursor the primary screen to the global screen position (x, y).
            # Sometimes, setPos(QScreen, Int, Int) API don't exists.
            QCursor().setPos(screen.size().width(), screen.size().height())

    def set_aspect_ratio(self, aspect_ratio):
        ''' Set aspect ratio.'''
        self.aspect_ratio = aspect_ratio
        self.aspect_ratio_change.emit()

    def add_widget(self, widget):
        ''' Add widget.'''
        # Init background color before addWidget.
        if not hasattr(self, "background_color"):
            self.background_color = QColor(self.theme_background_color)

        self.buffer_widget = widget
        self.addWidget(self.buffer_widget)

        self.buffer_widget.installEventFilter(self)

        self.buffer_widget.buffer = self

    def destroy_buffer(self):
        ''' Destroy buffer.'''
        if self.buffer_widget is not None:
            self.buffer_widget.deleteLater()

    def change_title(self, new_title):
        ''' Change title.'''
        if new_title != "about:blank":
            self.title = new_title
            eval_in_emacs('eaf--update-buffer-details', [self.buffer_id, new_title, self.url])

    @interactive(insert_or_do=True)
    def close_buffer(self):
        ''' Close buffer.'''
        eval_in_emacs('eaf-request-kill-buffer', [self.buffer_id])

    @abstract
    def all_views_hide(self):
        pass

    @abstract
    def some_view_show(self):
        pass

    @abstract
    def resize_view(self):
        pass

    def get_key_event_widgets(self):
        ''' Get key event widgets.'''
        return [self.buffer_widget]

    def send_input_message(self, message, callback_tag, input_type="string", initial_content="", completion_list=[]):
        ''' Send an input message to Emacs side for the user to respond.

        MESSAGE is a message string that would be sent to the user.

        CALLBACK_TAG is the reference tag when handle_input_message is invoked.

        INPUT_TYPE must be one of "string", "file", "yes-or-no", "marker" or "search".

        INITIAL_CONTENT is the intial content of the user response, it is only useful when INPUT_TYPE is "string".
        '''
        input_message(self.buffer_id, message, callback_tag, input_type, initial_content, completion_list)

        if input_type == "marker" and (not hasattr(getattr(self, "fetch_marker_callback"), "abstract")):
            self.start_marker_input_monitor_thread(callback_tag)
        elif input_type == "search":
            self.start_search_input_monitor_thread(callback_tag)

    @abstract
    def fetch_marker_callback(self):
        pass

    def start_marker_input_monitor_thread(self, callback_tag):
        self.fetch_marker_input_thread = FetchMarkerInputThread(callback_tag, self.fetch_marker_callback)
        self.fetch_marker_input_thread.match_marker.connect(self.handle_input_response)
        self.fetch_marker_input_thread.start()

    def stop_marker_input_monitor_thread(self):
        if self.fetch_marker_input_thread != None and self.fetch_marker_input_thread.isRunning():
            self.fetch_marker_input_thread.running_flag = False
            self.fetch_marker_input_thread = None

    def start_search_input_monitor_thread(self, callback_tag):
        self.fetch_search_input_thread = FetchSearchInputThread(callback_tag)
        self.fetch_search_input_thread.search_changed.connect(self.handle_input_response)
        self.fetch_search_input_thread.search_finish.connect(self.handle_search_finish)
        self.fetch_search_input_thread.start()

    def stop_search_input_monitor_thread(self):
        if self.fetch_search_input_thread != None and self.fetch_search_input_thread.isRunning():
            self.fetch_search_input_thread.stop()
            self.fetch_search_input_thread = None

    @abstract
    def handle_input_response(self, callback_tag, result_content):
        pass

    @abstract
    def action_quit(self):
        pass

    @abstract
    def cancel_input_response(self, callback_tag):
        pass

    @abstract
    def handle_search_forward(self, callback_tag):
        pass

    @abstract
    def handle_search_backward(self, callback_tag):
        pass

    @abstract
    def handle_search_finish(self, callback_tag):
        pass

    @abstract
    def scroll_other_buffer(self, scroll_direction, scroll_type):
        pass

    def save_session_data(self):
        return ""

    @abstract
    def restore_session_data(self, session_data):
        pass

    @abstract
    def update_with_data(self, update_data):
        pass

    @abstract
    def eval_js_function(self, function_name, function_arguments):
        ''' Eval JavaScript function.'''
        pass

    @abstract
    def execute_js_function(self, function_name, function_arguments):
        ''' Execute JavaScript function and return result.'''
        return None

    @abstract
    def eval_js_code(self, function_name, function_arguments):
        ''' Eval JavaScript function.'''
        pass

    @abstract
    def execute_js_code(self, function_name, function_arguments):
        ''' Execute JavaScript function and return result.'''
        return None

    def execute_function(self, function_name):
        ''' Call function.'''
        return getattr(self, function_name)()

    def execute_function_with_args(self, function_name, *args, **kwargs):
        ''' Call function with arguments.'''
        return getattr(self, function_name)(*args, **kwargs)

    @abstract
    def send_key_filter(self, event_string):
        pass

    def send_key(self, event_string):
        ''' Fake key event.'''
        # Init.
        text = event_string
        modifier = Qt.KeyboardModifier.NoModifier

        # Get key text.
        if event_string in qt_text_dict:
            text = qt_text_dict[event_string]

        if event_string == "<backtab>":
            modifier = Qt.KeyboardModifier.ShiftModifier
        elif len(event_string) == 1 and event_string.isupper():
            modifier = Qt.KeyboardModifier.ShiftModifier

        # print("Press: ", event_string, modifier, text)

        # NOTE: don't ignore text argument, otherwise QWebEngineView not respond key event.
        try:
            key_press = QKeyEvent(QEvent.Type.KeyPress, qt_key_dict[event_string], modifier, text)
        except:
            key_press = QKeyEvent(QEvent.Type.KeyPress, Qt.Key.Key_unknown, modifier, text)

        for widget in self.get_key_event_widgets():
            QApplication.postEvent(widget, key_press)

        self.send_key_filter(event_string)

    def send_key_sequence(self, event_string):
        ''' Fake key sequence.'''
        event_list = event_string.split("-")

        if len(event_list) > 1:
            for widget in [self.buffer_widget.focusProxy()]:    # type: ignore
                last_char = event_list[-1]
                last_key = last_char
                if len(last_char) == 1:
                    last_key = last_char.lower()

                modifiers = Qt.KeyboardModifier.NoModifier

                for modifier in event_list[0:-1]:
                    if modifier == "C":
                        modifiers |= Qt.KeyboardModifier.ControlModifier
                    elif modifier == "M":
                        modifiers |= Qt.KeyboardModifier.AltModifier
                    elif modifier == "S":
                        modifiers |= Qt.KeyboardModifier.ShiftModifier
                    elif modifier == "s":
                        modifiers |= Qt.KeyboardModifier.MetaModifier

                if last_key in qt_text_dict:
                    last_key = qt_text_dict[last_key]

                QApplication.postEvent(widget, QKeyEvent(QEvent.Type.KeyPress, qt_key_dict[last_key], modifiers, last_key))

    def get_url(self):
        ''' Get url.'''
        return self.url

    def get_clipboard_text(self):
        ''' Get text from system clipboard.'''
        return get_clipboard_text()

    def set_clipboard_text(self, text):
        ''' Set text to system clipboard.'''
        set_clipboard_text(text)

    @interactive(insert_or_do=True)
    def save_as_bookmark(self):
        ''' Save as bookmark.'''
        eval_in_emacs('bookmark-set', [])

    @interactive(insert_or_do=True)
    def select_left_tab(self):
        ''' Select left tab.'''
        eval_in_emacs('eaf-goto-left-tab', [])

    @interactive(insert_or_do=True)
    def select_right_tab(self):
        ''' Select right tab.'''
        eval_in_emacs('eaf-goto-right-tab', [])

    @interactive
    def update_theme(self):
        pass

    def focus_widget(self, event=None):
        '''Focus buffer widget.'''
        if event is None:
            event = QFocusEvent(QEvent.Type.FocusIn, Qt.FocusReason.MouseFocusReason)
        QApplication.postEvent(self.buffer_widget.focusProxy(), event)    # type: ignore

        # Activate emacs window when call focus widget, avoid first char is not
        eval_in_emacs('eaf-activate-emacs-window', [])

class FetchMarkerInputThread(QThread):

    match_marker = pyqtSignal(str, str)

    def __init__(self, callback_tag, fetch_marker_callback):
        QThread.__init__(self)

        self.callback_tag = callback_tag
        self.running_flag = True

        self.fetch_marker_callback = fetch_marker_callback
        self.marker_quit_keys = get_emacs_var("eaf-marker-quit-keys") or ""
        self.markers = self.fetch_marker_callback()

    def run(self):
        while self.running_flag:
            ## In some cases, the markers may not be ready when fetch_marker_callback is first called,
            ## so we need to call fetch_marker_callback multiple times.
            if self.markers is None or len(self.markers) == 0:
                self.markers = self.fetch_marker_callback()
                
            if self.markers != None:
                minibuffer_input = get_emacs_func_result("minibuffer-contents-no-properties", [])
                
                marker_input_quit = minibuffer_input and len(minibuffer_input) > 0 and minibuffer_input[-1] in self.marker_quit_keys
                marker_input_finish = minibuffer_input in self.markers
                
                if marker_input_quit:
                    self.running_flag = False
                    eval_in_emacs('exit-minibuffer', [])
                    message_to_emacs("Quit marker selection.")
                elif marker_input_finish:
                    self.running_flag = False
                    eval_in_emacs('exit-minibuffer', [])
                    message_to_emacs("Marker selected.")

            import time
            time.sleep(0.1)

class FetchSearchInputThread(QThread):

    search_changed = pyqtSignal(str, str)
    search_finish = pyqtSignal(str)

    def __init__(self, callback_tag):
        QThread.__init__(self)

        self.search_string = ""
        self.callback_tag = callback_tag
        self.running_flag = True

    def run(self):
        while self.running_flag:
            in_minibuffer = get_emacs_func_result("minibufferp", [])

            if in_minibuffer:
                minibuffer_input = get_emacs_func_result("minibuffer-contents-no-properties", [])

                if minibuffer_input != self.search_string:
                    self.search_changed.emit(self.callback_tag, minibuffer_input)
                    self.search_string = minibuffer_input
            else:
                self.stop()

            import time
            time.sleep(0.1)

    def stop(self):
        self.search_finish.emit(self.callback_tag)
        self.running_flag = False
