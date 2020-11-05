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
from qtconsole import styles
from qtconsole.rich_jupyter_widget import RichJupyterWidget
from qtconsole.manager import QtKernelManager
from core.buffer import Buffer

class AppBuffer(Buffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path):
        Buffer.__init__(self, buffer_id, url, arguments, emacs_var_dict, module_path, False)

        arguments_dict = json.loads(arguments)
        self.kernel = arguments_dict["kernel"]

        font_size = int(self.emacs_var_dict["eaf-jupyter-font-size"])
        font_family = self.emacs_var_dict["eaf-jupyter-font-family"]
        style = self.emacs_var_dict["eaf-jupyter-syntax-style"]

        self.add_widget(EafJupyterWidget(style, self.kernel, font_size=font_size, font_family=font_family))

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
    def __init__(self, style, kernel, *args, **kwargs):
        self._init_style(style)

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

    def focusProxy(self):
        if self._control.isVisible():
            return self._control
        elif self._page_control.isVisible():
            return self._page_control
        else:
            return None

    def _init_style(self, style):
        if not style:
            self.set_default_style()
            return

        if style == "bw":
            colors = "nocolor"
        elif styles.dark_style(style):
            colors = "linux"
        else:
            colors = "lightbg"
        self.style_sheet = styles.sheet_from_template(style, colors)
        self.syntax_style = style
        self._syntax_style_changed()
        self._style_sheet_changed()

    def destroy(self):
        self.kernel_client.stop_channels()
        self.kernel_manager.shutdown_kernel()
