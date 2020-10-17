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
from PyQt5.QtGui import QBrush, QColor
from PyQt5.QtWidgets import QGraphicsScene
from PyQt5.QtCore import Qt, QEvent
from PyQt5.QtGui import QKeyEvent
from PyQt5.QtWidgets import QApplication
from core.utils import interactive
import abc
import string

qt_key_dict = {}

# Build char event.
for char in string.ascii_lowercase:
    upper_char = char.upper()
    qt_key_dict[char] = eval("Qt.Key_{}".format(upper_char))
    qt_key_dict[upper_char] = eval("Qt.Key_{}".format(upper_char))

# Build number event.
for number in range(0, 10):
    qt_key_dict[str(number)] = eval("Qt.Key_{}".format(number))

qt_key_dict.update({
    ''':''': Qt.Key_Colon,
    ''';''': Qt.Key_Semicolon,
    '''.''': Qt.Key_Period,
    ''',''': Qt.Key_Comma,
    '''+''': Qt.Key_Plus,
    '''-''': Qt.Key_Minus,
    '''=''': Qt.Key_Equal,
    '''_''': Qt.Key_Underscore,
    '''[''': Qt.Key_BracketLeft,
    ''']''': Qt.Key_BracketRight,
    '''(''': Qt.Key_BraceLeft,
    ''')''': Qt.Key_BraceRight,
    '''{''': Qt.Key_ParenLeft,
    '''}''': Qt.Key_ParenRight,
    '''<''': Qt.Key_Less,
    '''>''': Qt.Key_Greater,
    '''@''': Qt.Key_At,
    '''\\''': Qt.Key_Backslash,
    '''|''': Qt.Key_Bar,
    '''/''': Qt.Key_Slash,
    '''#''': Qt.Key_NumberSign,
    '''$''': Qt.Key_Dollar,
    '''?''': Qt.Key_Question,
    '''"''': Qt.Key_QuoteDbl,
    '''`''': Qt.Key_QuoteLeft,
    '''%''': Qt.Key_Percent,
    '''^''': Qt.Key_AsciiCircum,
    '''&''': Qt.Key_Ampersand,
    '''*''': Qt.Key_Asterisk,
    '''~''': Qt.Key_AsciiTilde,
    '''!''': Qt.Key_Exclam,
    '''\'''': Qt.Key_Apostrophe,
    '''SPC''': Qt.Key_Space,
    '''RET''': Qt.Key_Return,
    '''DEL''': Qt.Key_Backspace,
    '''TAB''': Qt.Key_Tab,
    '''<backtab>''': Qt.Key_Backtab,
    '''<home>''': Qt.Key_Home,
    '''<end>''': Qt.Key_End,
    '''<left>''': Qt.Key_Left,
    '''<right>''': Qt.Key_Right,
    '''<up>''': Qt.Key_Up,
    '''<down>''': Qt.Key_Down,
    '''<prior>''': Qt.Key_PageUp,
    '''<next>''': Qt.Key_PageDown,
    '''<delete>''': Qt.Key_Delete,
    '''<backspace>''': Qt.Key_Backspace,
    '''<return>''': Qt.Key_Return
})

qt_text_dict = {
    "SPC": " "
}

class Buffer(QGraphicsScene):
    __metaclass__ = abc.ABCMeta

    update_buffer_details = QtCore.pyqtSignal(str, str, str)
    open_url_in_new_tab = QtCore.pyqtSignal(str)
    duplicate_page_in_new_tab = QtCore.pyqtSignal(str)
    open_url_in_background_tab = QtCore.pyqtSignal(str)
    translate_text = QtCore.pyqtSignal(str)
    input_message = QtCore.pyqtSignal(str, str, str, str, str)
    request_close_buffer = QtCore.pyqtSignal(str)
    message_to_emacs = QtCore.pyqtSignal(str)
    set_emacs_var = QtCore.pyqtSignal(str, str, str)
    eval_in_emacs = QtCore.pyqtSignal(str)
    goto_left_tab = QtCore.pyqtSignal()
    goto_right_tab = QtCore.pyqtSignal()
    aspect_ratio_change = QtCore.pyqtSignal()
    enter_fullscreen_request = QtCore.pyqtSignal()
    exit_fullscreen_request = QtCore.pyqtSignal()

    def __init__(self, buffer_id, url, arguments, emacs_var_dict, module_path, fit_to_view):
        super(QGraphicsScene, self).__init__()

        self.buffer_id = buffer_id
        self.url = url
        self.arguments = arguments
        self.emacs_var_dict = emacs_var_dict
        self.module_path = module_path
        self.fit_to_view = fit_to_view
        if emacs_var_dict["eaf-emacs-theme-mode"] == "dark":
            self.background_color = QColor(0, 0, 0, 255)
        else:
            self.background_color = QColor(255, 255, 255, 255)
        self.setBackgroundBrush(QBrush(self.background_color))
        self.title = ""

        self.buffer_widget = None

        self.is_fullscreen = False

        self.current_event_string = ""

        self.aspect_ratio = 0
        self.vertical_padding_ratio = 1.0 / 8

        self.enter_fullscreen_request.connect(self.enable_fullscreen)
        self.exit_fullscreen_request.connect(self.disable_fullscreen)

    def build_all_methods(self, origin_class):
        ''' Build all methods.'''
        method_list = [func for func in dir(origin_class) if callable(getattr(origin_class, func)) and not func.startswith("__")]
        for func_name in method_list:
            func_attr = getattr(origin_class, func_name)
            if hasattr(func_attr, "interactive"):
                self.build_interactive_method(origin_class, func_name, getattr(func_attr, "new_name"), getattr(func_attr, "msg_emacs"), getattr(func_attr, "insert_or_do"))

    def build_interactive_method(self, origin_class, class_method_name, new_method_name=None, msg_emacs=None, insert_or_do=False):
        ''' Build interactive methods.'''
        new_name = class_method_name if new_method_name is None else new_method_name
        self.__dict__.update({new_name: getattr(origin_class, class_method_name)})
        if msg_emacs is not None:
            self.message_to_emacs.emit(msg_emacs)
        if insert_or_do:
            self.build_insert_or_do(new_name)

    def build_insert_or_do(self, method_name):
        ''' Build insert or do.'''
        def _do ():
            if self.is_focus():
                self.fake_key_event(self.current_event_string)
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

    def disable_fullscreen(self):
        ''' Disable full screen.'''
        self.is_fullscreen = False

    def set_aspect_ratio(self, aspect_ratio):
        ''' Set aspect ratio.'''
        self.aspect_ratio = aspect_ratio
        self.aspect_ratio_change.emit()

    def add_widget(self, widget):
        ''' Add widget.'''
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
            self.update_buffer_details.emit(self.buffer_id, new_title, self.url)

    @interactive(insert_or_do=True)
    def close_buffer(self):
        ''' Close buffer.'''
        self.request_close_buffer.emit(self.buffer_id)

    def all_views_hide(self):
        pass

    def some_view_show(self):
        pass

    def resize_view(self):
        pass

    def get_key_event_widgets(self):
        ''' Get key event widgets.'''
        return [self.buffer_widget]

    def send_input_message(self, message, callback_tag, input_type="string", initial_content=""):
        ''' Send an input message to Emacs side for the user to respond.

        MESSAGE is a message string that would be sent to the user.

        CALLBACK_TAG is the reference tag when handle_input_message is invoked.

        INPUT_TYPE must be one of "string", "file", or "yes-or-no".

        INITIAL_CONTENT is the intial content of the user response, it is only useful when INPUT_TYPE is "string".
        '''
        self.input_message.emit(self.buffer_id, message, callback_tag, input_type, initial_content)

    def handle_input_response(self, callback_tag, result_content):
        pass

    def action_quit(self):
        pass

    def cancel_input_response(self, callback_tag):
        pass

    def scroll_other_buffer(self, scroll_direction, scroll_type):
        pass

    def save_session_data(self):
        return ""

    def restore_session_data(self, session_data):
        pass

    def update_with_data(self, update_data):
        pass

    def execute_function(self, function_name):
        ''' Execute function.'''
        getattr(self, function_name)()

    def call_function(self, function_name):
        ''' Call function.'''
        return getattr(self, function_name)()

    def call_function_with_args(self, function_name, args_string):
        ''' Call function with arguments.'''
        return getattr(self, function_name)(args_string)

    def fake_key_event_filter(self, event_string):
        pass

    def fake_key_event(self, event_string):
        ''' Fake key event.'''
        # Init.
        text = event_string
        modifier = Qt.NoModifier

        # Get key text.
        if event_string in qt_text_dict:
            text = qt_text_dict[event_string]

        if event_string in ["TAB", "<backtab>"]:
            text = ""
            if event_string == "<backtab>":
                modifier = Qt.ShiftModifier
        elif event_string.isupper():
            modifier = Qt.ShiftModifier

        # print("Press: ", event_string)

        # NOTE: don't ignore text argument, otherwise QWebEngineView not respond key event.
        try:
            key_press = QKeyEvent(QEvent.KeyPress, qt_key_dict[event_string], modifier, text)
        except:
            key_press = QKeyEvent(QEvent.KeyPress, Qt.Key_unknown, modifier, text)

        for widget in self.get_key_event_widgets():
            QApplication.sendEvent(widget, key_press)

        self.fake_key_event_filter(event_string)

    def fake_key_sequence(self, event_string):
        ''' Fake key sequence.'''
        event_list = event_string.split("-")

        if len(event_list) > 1:
            for widget in [self.buffer_widget.focusProxy()]:
                last_char = event_list[-1]
                last_key = last_char
                if len(last_char) == 1:
                    last_key = last_char.lower()

                modifiers = Qt.NoModifier

                for modifier in event_list[0:-1]:
                    if modifier == "C":
                        modifiers |= Qt.ControlModifier
                    elif modifier == "M":
                        modifiers |= Qt.AltModifier
                    elif modifier == "S":
                        modifiers |= Qt.ShiftModifier
                    elif modifier == "s":
                        modifiers |= Qt.MetaModifier

                QApplication.sendEvent(widget, QKeyEvent(QEvent.KeyPress, qt_key_dict[last_key], modifiers, last_key))

    def get_url(self):
        ''' Get url.'''
        return self.url

    @interactive(insert_or_do=True)
    def save_as_bookmark(self):
        ''' Save as bookmark.'''
        self.eval_in_emacs.emit('''(bookmark-set)''')

    @interactive(insert_or_do=True)
    def select_left_tab(self):
        ''' Select left tab.'''
        self.goto_left_tab.emit()

    @interactive(insert_or_do=True)
    def select_right_tab(self):
        ''' Select right tab.'''
        self.goto_right_tab.emit()
