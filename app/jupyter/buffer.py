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

import json
from PyQt5.QtCore import QTimer, Qt
from PyQt5.QtWidgets import QApplication
from qtconsole import styles
from qtconsole.rich_jupyter_widget import RichJupyterWidget
from qtconsole.manager import QtKernelManager
from core.buffer import Buffer
from core.utils import interactive
from core.kill_ring import EafKillRing

class AppBuffer(Buffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path):
        Buffer.__init__(self, buffer_id, url, arguments, emacs_var_dict, module_path, False)

        arguments_dict = json.loads(arguments)
        self.kernel = arguments_dict["kernel"]

        font_size = int(self.emacs_var_dict["eaf-jupyter-font-size"])
        font_family = self.emacs_var_dict["eaf-jupyter-font-family"]

        self.add_widget(EafJupyterWidget(emacs_var_dict, self.kernel, font_size=font_size, font_family=font_family))

        QTimer.singleShot(500, self.focus_widget)

        self.build_all_methods(self.buffer_widget)

    def get_key_event_widgets(self):
        ''' Send key event to RichJupyterWidget's focusProxy widget.'''
        # We need send key event to RichJupyterWidget's focusProxy widget, not RichJupyterWidget.
        widget = self.buffer_widget.focusProxy()
        return [widget] if widget else []

    def destroy_buffer(self):
        print('Shutdown jupyter kernel {}'.format(self.kernel))
        self.buffer_widget.destroy()

        super().destroy_buffer()


class EafJupyterWidget(RichJupyterWidget):
    def __init__(self, emacs_var_dict, kernel, *args, **kwargs):
        bg_color = emacs_var_dict["eaf-emacs-theme-background-color"]
        fg_color = emacs_var_dict["eaf-emacs-theme-foreground-color"]
        dark_mode = emacs_var_dict["eaf-jupyter-dark-mode"] == "true" or \
           (emacs_var_dict["eaf-jupyter-dark-mode"] == "follow" and emacs_var_dict["eaf-emacs-theme-mode"] == "dark")
        self._init_style(bg_color, fg_color, dark_mode)

        self.scrollbar_visibility = False

        super(EafJupyterWidget, self).__init__(*args, **kwargs)

        kernel_manager = QtKernelManager(kernel_name=kernel)
        kernel_manager.start_kernel()

        kernel_client = kernel_manager.client()
        kernel_client.start_channels()

        self.kernel_manager = kernel_manager
        self.kernel_client = kernel_client

        self._control.setStyleSheet("border: none;")
        self._page_control.setStyleSheet("border: none;")

        self._kill_ring = EafKillRing(self._control)

    @interactive()
    def zoom_in(self):
        self.change_font_size(1)

    @interactive()
    def zoom_out(self):
        self.change_font_size(-1)

    @interactive()
    def zoom_reset(self):
        self.reset_font()

    def focusProxy(self):
        if self._control.isVisible():
            return self._control
        elif self._page_control.isVisible():
            return self._page_control
        else:
            return None

    def _init_style(self, bg_color, fg_color, dark_mode):
        if dark_mode:
            self.style_sheet = styles.default_dark_style_template % dict(bgcolor=bg_color, fgcolor=fg_color, select="#555")
            self.syntax_style = styles.default_dark_syntax_style
        else:
            self.style_sheet = styles.default_light_style_template % dict(bgcolor=bg_color, fgcolor=fg_color, select="#ccc")
            self.syntax_style = styles.default_light_syntax_style
        self._syntax_style_changed()
        self._style_sheet_changed()

    def destroy(self):
        self.kernel_client.stop_channels()
        self.kernel_manager.shutdown_kernel()
