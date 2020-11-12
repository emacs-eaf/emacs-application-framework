#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2018 Andy Stewart
#
# Author:     MacKong <mackonghp@gmail.com>
# Maintainer: MacKong <mackonghp@gmail.com>
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

from core.utils import get_clipboard_text, set_clipboard_text

try:
    from qtconsole.kill_ring import QtKillRing
except ImportError:
    """ A generic Emacs-style kill ring, as well as a Qt-specific version.

    Stolen from qtconsole.
    """
    from PyQt5 import QtCore, QtWidgets, QtGui


    class KillRing(object):
        """ A generic Emacs-style kill ring.
        """

        def __init__(self):
            self.clear()

        def clear(self):
            """ Clears the kill ring.
            """
            self._index = -1
            self._ring = []

        def kill(self, text):
            """ Adds some killed text to the ring.
            """
            self._ring.append(text)

        def yank(self):
            """ Yank back the most recently killed text.

            Returns
            -------
            A text string or None.
            """
            self._index = len(self._ring)
            return self.rotate()

        def rotate(self):
            """ Rotate the kill ring, then yank back the new top.

            Returns
            -------
            A text string or None.
            """
            self._index -= 1
            if self._index >= 0:
                return self._ring[self._index]
            return None


    class QtKillRing(QtCore.QObject):
        """ A kill ring attached to Q[Plain]TextEdit.
        """

        #--------------------------------------------------------------------------
        # QtKillRing interface
        #--------------------------------------------------------------------------

        def __init__(self, text_edit):
            """ Create a kill ring attached to the specified Qt text edit.
            """
            assert isinstance(text_edit, (QtWidgets.QTextEdit, QtWidgets.QPlainTextEdit))
            super(QtKillRing, self).__init__()

            self._ring = KillRing()
            self._prev_yank = None
            self._skip_cursor = False
            self._text_edit = text_edit

            text_edit.cursorPositionChanged.connect(self._cursor_position_changed)

        def clear(self):
            """ Clears the kill ring.
            """
            self._ring.clear()
            self._prev_yank = None

        def kill(self, text):
            """ Adds some killed text to the ring.
            """
            self._ring.kill(text)

        def kill_cursor(self, cursor):
            """ Kills the text selected by the give cursor.
            """
            text = cursor.selectedText()
            if text:
                cursor.removeSelectedText()
                self.kill(text)

        def yank(self):
            """ Yank back the most recently killed text.
            """
            text = self._ring.yank()
            if text:
                self._skip_cursor = True
                cursor = self._text_edit.textCursor()
                cursor.insertText(text)
                self._prev_yank = text

        def rotate(self):
            """ Rotate the kill ring, then yank back the new top.
            """
            if self._prev_yank:
                text = self._ring.rotate()
                if text:
                    self._skip_cursor = True
                    cursor = self._text_edit.textCursor()
                    cursor.movePosition(QtGui.QTextCursor.Left,
                                        QtGui.QTextCursor.KeepAnchor,
                                        n = len(self._prev_yank))
                    cursor.insertText(text)
                    self._prev_yank = text

        #--------------------------------------------------------------------------
        # Protected interface
        #--------------------------------------------------------------------------

        #------ Signal handlers ----------------------------------------------------

        def _cursor_position_changed(self):
            if self._skip_cursor:
                self._skip_cursor = False
            else:
                self._prev_yank = None


class EafKillRing(QtKillRing):
    """ Kill ring suppport co-communication with system clipboard.
    """
    def __init__(self, text_edit):
        """ Create a kill ring attached to the specified Qt text edit.
        """
        super(EafKillRing, self).__init__(text_edit)

    def kill(self, text):
        super(EafKillRing, self).kill(text)

        set_clipboard_text(text)

    def yank(self):
        text = get_clipboard_text()
        self.kill(text)

        super(EafKillRing, self).yank()
