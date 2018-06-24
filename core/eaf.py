#! /usr/bin/env python
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

from PyQt5.QtCore import QEvent, QPointF
from PyQt5.QtGui import QMouseEvent
from PyQt5.QtWidgets import QApplication
from dbus.mainloop.glib import DBusGMainLoop
from fake_key_event import fake_key_event
from view import View
import dbus
import dbus.service
import threading
import time

import os,sys,inspect
current_dir = os.path.dirname(os.path.abspath(inspect.getfile(inspect.currentframe())))
parent_dir = os.path.dirname(current_dir)
sys.path.insert(0, parent_dir) 
from app.browser.buffer import BrowserBuffer
from app.imageviewer.buffer import ImageViewerBuffer
from app.videoplayer.buffer import VideoPlayerBuffer

EAF_DBUS_NAME = "com.lazycat.eaf"
EAF_OBJECT_NAME = "/com/lazycat/eaf"

class EAF(dbus.service.Object):
    def __init__(self, args):
        global emacs_xid, emacs_width, emacs_height
        
        dbus.service.Object.__init__(
            self,
            dbus.service.BusName(EAF_DBUS_NAME, bus=dbus.SessionBus()),
            EAF_OBJECT_NAME)
        
        (emacs_xid, emacs_width, emacs_height) = (map(lambda x: int(x), args))
        self.buffer_dict = {}
        self.view_dict = {}
        
        self.start_finish()
        
    @dbus.service.method(EAF_DBUS_NAME, in_signature="ss", out_signature="s")
    def new_buffer(self, buffer_id, url):
        global emacs_width, emacs_height

        if url.startswith("/"):
            if url.endswith(".jpg") or url.endswith(".png"):
                self.buffer_dict[buffer_id] = ImageViewerBuffer(buffer_id, url, emacs_width, emacs_height)
            elif url.endswith(".ogg"):
                self.buffer_dict[buffer_id] = VideoPlayerBuffer(buffer_id, url, emacs_width, emacs_height)
            else:
                return "Don't know how to open {0}".format(url)
        else:
            from urllib.parse import urlparse 
            result = urlparse(url)
            if len(result.scheme) != 0:
                self.buffer_dict[buffer_id] = BrowserBuffer(buffer_id, result.geturl(), emacs_width, emacs_height)
            else:
                result = urlparse("{0}:{1}".format("http", url))
                if result.scheme != "":
                    self.buffer_dict[buffer_id] = BrowserBuffer(buffer_id, result.geturl(), emacs_width, emacs_height)
                else:
                    return "{0} is not valid url".format(url)
                
        return ""
            
    @dbus.service.method(EAF_DBUS_NAME, in_signature="s", out_signature="")
    def update_views(self, args):
        global emacs_xid
        
        view_infos = args.split(",")
        
        # Remove old key from view dict and destroy old view.
        for key in list(self.view_dict):
            if key not in view_infos:
                self.view_dict[key].handle_destroy()
                self.view_dict.pop(key, None)
        
        # Create new view and udpate in view dict.
        if view_infos != ['']:
            for view_info in view_infos:
                if view_info not in self.view_dict:
                    view = View(emacs_xid, view_info)
                    self.view_dict[view_info] = view
                    
                    view.trigger_mouse_event.connect(self.send_mouse_event_to_buffer)
                    view.trigger_focus_event.connect(self.focus_emacs_buffer)
                    
    @dbus.service.method(EAF_DBUS_NAME, in_signature="s", out_signature="")
    def kill_buffer(self, buffer_id):
        # Kill all view base on buffer_id.
        for key in list(self.view_dict):
            if buffer_id == self.view_dict[key].buffer_id:
                self.view_dict[key].handle_destroy()
                self.view_dict.pop(key, None)
        
        # Clean buffer from buffer dict.
        if buffer_id in self.buffer_dict:
            self.buffer_dict[buffer_id].handle_destroy()
            self.buffer_dict.pop(buffer_id, None)
    
            
    @dbus.service.method(EAF_DBUS_NAME, in_signature="s", out_signature="")
    def send_key(self, args):
        print("Send key: %s" % args)
        (buffer_id, event_string) = args.split(":")
        
        if buffer_id in self.buffer_dict:
            QApplication.sendEvent(self.buffer_dict[buffer_id].buffer_widget, fake_key_event(event_string))
            
    @dbus.service.signal("com.lazycat.eaf")        
    def focus_emacs_buffer(self, message):
        print("************* %s" % message)
        
    @dbus.service.signal("com.lazycat.eaf")    
    def start_finish(self):
        pass
        
    def send_mouse_event_to_buffer(self, buffer_id, view_width, view_height, view_image_width, view_image_height, event):
        print("Send mouse: %s %s" % (buffer_id, event))
        
        global emacs_xid
        
        if buffer_id in self.buffer_dict:
            if event.type() in [QEvent.MouseButtonPress, QEvent.MouseButtonRelease,
                                QEvent.MouseMove, QEvent.MouseButtonDblClick]:
                # Get view render coordinate.
                view_render_x = (view_width - view_image_width) / 2
                view_render_y = (view_height - view_image_height) / 2
                
                # Just send event if response in view image area.
                if (event.x() >= view_render_x) and (event.x() <= view_render_x + view_image_width) and (event.y() >= view_render_y) and (event.y() <= view_render_y + view_image_height):
                    view_sizes = list(map(lambda v: (v.width, v.height), self.view_dict.values()))
                    
                    buffer_width = emacs_width
                    buffer_height = emacs_height
                    
                    if len(view_sizes) > 0:
                        buffer_width, buffer_height = max(view_sizes, key=lambda size: size[0] * size[1])
                                        
                    width_scale = view_width * 1.0 / buffer_width
                    height_scale = view_height * 1.0 / buffer_height
                    image_scale = 1.0
                    if width_scale < height_scale:
                        image_scale = width_scale
                    else:
                        image_scale = height_scale

                    new_event_x = (event.x() - view_render_x) / image_scale
                    new_event_y = (event.y() - view_render_y) / image_scale
                    new_event_pos = QPointF(new_event_x, new_event_y)
                    
                    new_event = QMouseEvent(event.type(),
                                            new_event_pos,
                                            event.button(),
                                            event.buttons(),
                                            event.modifiers())
                    
                    QApplication.sendEvent(self.buffer_dict[buffer_id].buffer_widget, new_event)
                else:
                    print("Do not send event, because event out of view's response area")
            else:
                QApplication.sendEvent(self.buffer_dict[buffer_id].buffer_widget, event)
                        
    def update_buffers(self):
        global emacs_width, emacs_height
        
        while True:
            for buffer in list(self.buffer_dict.values()):
                # Get size list of buffer's views.
                view_sizes = list(map(lambda v: (v.width, v.height), self.view_dict.values()))
                
                # Init buffer size with emacs' size.
                buffer_width = emacs_width
                buffer_height = emacs_height
                
                # Update buffer size with max area view's size,
                # to make each view has the same rendering area after user do split operation in emacs.
                if len(view_sizes) > 0:
                    buffer_width, buffer_height = max(view_sizes, key=lambda size: size[0] * size[1])
                                        
                # Resize buffer.
                buffer.resize_buffer(buffer_width, buffer_height)        
                
                # Update buffer image.
                buffer.update_content()
                
                if buffer.qimage != None:
                    # Render views.
                    for view in list(self.view_dict.values()):
                        if view.buffer_id == buffer.buffer_id:
                            # Scale image to view size.
                            width_scale = view.width * 1.0 / buffer_width
                            height_scale = view.height * 1.0 / buffer_height
                            image_scale = 1.0
                            if width_scale < height_scale:
                                image_scale = width_scale
                            else:
                                image_scale = height_scale

                            view.qimage = buffer.qimage.scaled(buffer_width * image_scale, buffer_height * image_scale)
                            view.background_color = buffer.background_color

                            # Update view.
                            view.update()
                
            time.sleep(0.04)
                            
if __name__ == "__main__":
    import sys
    import signal
    
    DBusGMainLoop(set_as_default=True) # WARING: only use once in one process
    
    bus = dbus.SessionBus()
    if bus.request_name(EAF_DBUS_NAME) != dbus.bus.REQUEST_NAME_REPLY_PRIMARY_OWNER:
        print("EAF process has startup.")
    else:
        emacs_xid = 0
        emacs_width = emacs_height = 0
        
        app = QApplication(sys.argv)
        
        eaf = EAF(sys.argv[1:])
        
        threading.Thread(target=eaf.update_buffers).start()
        
        print("EAF process start.")
        
        signal.signal(signal.SIGINT, signal.SIG_DFL)
        sys.exit(app.exec_())
