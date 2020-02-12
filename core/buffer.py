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
import abc

qt_key_dict = {
    '''a''': Qt.Key_A,
    '''b''': Qt.Key_B,
    '''c''': Qt.Key_C,
    '''d''': Qt.Key_D,
    '''e''': Qt.Key_E,
    '''f''': Qt.Key_F,
    '''g''': Qt.Key_G,
    '''h''': Qt.Key_H,
    '''i''': Qt.Key_I,
    '''j''': Qt.Key_J,
    '''k''': Qt.Key_K,
    '''l''': Qt.Key_L,
    '''m''': Qt.Key_M,
    '''n''': Qt.Key_N,
    '''o''': Qt.Key_O,
    '''p''': Qt.Key_P,
    '''q''': Qt.Key_Q,
    '''r''': Qt.Key_R,
    '''s''': Qt.Key_S,
    '''t''': Qt.Key_T,
    '''u''': Qt.Key_U,
    '''v''': Qt.Key_V,
    '''w''': Qt.Key_W,
    '''x''': Qt.Key_X,
    '''y''': Qt.Key_Y,
    '''z''': Qt.Key_Z,

    '''A''': Qt.Key_A,
    '''B''': Qt.Key_B,
    '''C''': Qt.Key_C,
    '''D''': Qt.Key_D,
    '''E''': Qt.Key_E,
    '''F''': Qt.Key_F,
    '''G''': Qt.Key_G,
    '''H''': Qt.Key_H,
    '''I''': Qt.Key_I,
    '''J''': Qt.Key_J,
    '''K''': Qt.Key_K,
    '''L''': Qt.Key_L,
    '''M''': Qt.Key_M,
    '''N''': Qt.Key_N,
    '''O''': Qt.Key_O,
    '''P''': Qt.Key_P,
    '''Q''': Qt.Key_Q,
    '''R''': Qt.Key_R,
    '''S''': Qt.Key_S,
    '''T''': Qt.Key_T,
    '''U''': Qt.Key_U,
    '''V''': Qt.Key_V,
    '''W''': Qt.Key_W,
    '''X''': Qt.Key_X,
    '''Y''': Qt.Key_Y,
    '''Z''': Qt.Key_Z,

    '''0''': Qt.Key_0,
    '''1''': Qt.Key_1,
    '''2''': Qt.Key_2,
    '''3''': Qt.Key_3,
    '''4''': Qt.Key_4,
    '''5''': Qt.Key_5,
    '''6''': Qt.Key_6,
    '''7''': Qt.Key_7,
    '''8''': Qt.Key_8,
    '''9''': Qt.Key_9,
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
    '''<delete>''': Qt.Key_Delete
}

qt_text_dict = {
    "SPC": " "
}

class Buffer(QGraphicsScene):
    __metaclass__ = abc.ABCMeta

    update_title = QtCore.pyqtSignal(str, str)
    open_url_in_new_tab = QtCore.pyqtSignal(str)
    open_url_in_background_tab = QtCore.pyqtSignal(str)
    translate_text = QtCore.pyqtSignal(str)
    before_destroy_hook = QtCore.pyqtSignal()
    input_message = QtCore.pyqtSignal(str, str, str, str)
    close_buffer = QtCore.pyqtSignal(str)
    message_to_emacs = QtCore.pyqtSignal(str)
    set_emacs_var = QtCore.pyqtSignal(str, str)
    eval_in_emacs = QtCore.pyqtSignal(str)
    goto_left_tab = QtCore.pyqtSignal()
    goto_right_tab = QtCore.pyqtSignal()

    def __init__(self, buffer_id, url, arguments, emacs_var_dict, fit_to_view, background_color):
        super(QGraphicsScene, self).__init__()

        self.buffer_id = buffer_id
        self.url = url
        self.arguments = arguments
        self.emacs_var_dict = emacs_var_dict
        self.fit_to_view = fit_to_view
        self.background_color = background_color
        self.setBackgroundBrush(QBrush(self.background_color))

        self.buffer_widget = None

        self.draw_progressbar = False
        self.progressbar_progress = 0
        self.progressbar_color = QColor(233, 129, 35, 255)
        self.progressbar_height = 2

        self.current_event_string = ""

    def drawForeground(self, painter, rect):
        if self.draw_progressbar:
            painter.setBrush(self.progressbar_color)
            painter.setPen(self.progressbar_color)
            painter.drawRect(0, 0, rect.width() * self.progressbar_progress * 1.0 / 100, self.progressbar_height)

    @QtCore.pyqtSlot()
    def start_progress(self):
        self.progressbar_progress = 0
        self.draw_progressbar = True
        self.update()

    @QtCore.pyqtSlot()
    def stop_progress(self):
        self.draw_progressbar = False
        self.update()

    @QtCore.pyqtSlot(int)
    def update_progress(self, progress):
        self.progressbar_progress = progress
        self.draw_progressbar = True
        self.update()

        if progress == 100:
            QtCore.QTimer.singleShot(500, self.stop_progress)

    def add_widget(self, widget):
        self.buffer_widget = widget
        self.addWidget(self.buffer_widget)

        self.buffer_widget.installEventFilter(self)

        self.buffer_widget.message_to_emacs = self.message_to_emacs
        self.buffer_widget.set_emacs_var = self.set_emacs_var
        self.buffer_widget.eval_in_emacs = self.eval_in_emacs
        self.buffer_widget.send_input_message = self.send_input_message
        self.buffer_widget.buffer = self

    def handle_destroy(self):
        self.before_destroy_hook.emit()

        if self.buffer_widget is not None:
            self.buffer_widget.destroy()

    def change_title(self, title):
        self.update_title.emit(self.buffer_id, title)

    def request_close_buffer(self):
        self.close_buffer.emit(self.buffer_id)

    def all_views_hide(self):
        pass

    def some_view_show(self):
        pass

    def get_key_event_widgets(self):
        return [self.buffer_widget]

    def send_input_message(self, message, callback_type, input_type="string"):
        self.input_message.emit(self.buffer_id, message, callback_type, input_type)

    def handle_input_message(self, result_type, result_content):
        pass

    def action_quit(self):
        pass

    def cancel_input_message(self, result_type):
        pass

    def scroll(self, scroll_direction, scroll_type):
        pass

    def save_session_data(self):
        return ""

    def restore_session_data(self, session_data):
        pass

    def update_with_data(self, update_data):
        pass

    def execute_function(self, function_name):
        getattr(self, function_name)()

    def call_function(self, function_name):
        return getattr(self, function_name)()

    def fake_key_event_filter(self, event_string):
        pass

    def fake_key_event(self, event_string):
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

    def get_url(self):
        return self.url
