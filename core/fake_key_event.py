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

from PyQt5.QtCore import Qt, QEvent
from PyQt5.QtGui import QKeyEvent
from PyQt5.QtWidgets import QApplication

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
    ''';''': Qt.Key_Semicolon,
    '''.''': Qt.Key_Period,
    ''',''': Qt.Key_Comma,
    '''+''': Qt.Key_Plus,
    '''-''': Qt.Key_Minus,
    '''=''': Qt.Key_Equal,
    '''[''': Qt.Key_BracketLeft,
    ''']''': Qt.Key_BracketRight,
    '''@''': Qt.Key_At,
    '''"''': Qt.Key_QuoteDbl,
    '''$''': Qt.Key_Dollar,
    '''%''': Qt.Key_Percent,
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
}

qt_text_dict = {
    "SPC": " "
}

def fake_key_event(event_string, app_buffer):
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

    if event_string.isupper()    :
        modifier = Qt.ShiftModifier

    print("Press: ", event_string)    

    # NOTE: don't ignore text argument, otherwise QWebEngineView not respond key event.
    key_press = QKeyEvent(QEvent.KeyPress, qt_key_dict[event_string], modifier, text)

    for widget in app_buffer.get_key_event_widgets():
        QApplication.sendEvent(widget, key_press)
