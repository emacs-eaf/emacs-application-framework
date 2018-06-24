#! /usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2011 ~ 2014 Andy Stewart
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

xlib_display = None

def get_xlib_display():
    global xlib_display

    if xlib_display == None:
        from Xlib import display
        xlib_display = display.Display()

    return xlib_display

def grab_focus(window_id):
    global xlib_display

    from Xlib import X
    xwindow = xlib_display.create_resource_object("window", window_id)

    xwindow.set_input_focus(X.RevertToNone, X.CurrentTime)
    xwindow.configure(stack_mode=X.Above)

    xlib_display.sync()

def get_parent_window_id(window_id):
    xlib_display = get_xlib_display()

    return xlib_display.create_resource_object("window", window_id).query_tree().parent.__window__()

