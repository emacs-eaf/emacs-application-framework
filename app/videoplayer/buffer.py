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

from PyQt5.QtGui import QColor
from PyQt5.QtWebKitWidgets import QWebView
from buffer import Buffer

html_file = '''
<!DOCTYPE html>
<html>
  <head>
  </head>
  <body>
    <style>
     html {
	 margin: 0px;
	 padding: 0px;
     }
     
     body {
	 margin: 0px;
	 padding: 0px;
     }
     
     video {
	 width: 100%;
	 height: 100%;
	 margin: 0px;
	 padding: 0px;
     }
     
     ::-webkit-scrollbar {
	 display: none;
     }
    </style>
      <video controls autoplay src="file://**********"></video>
  </body>
</html>
'''

class VideoPlayerBuffer(Buffer):
    def __init__(self, buffer_id, url, width, height):
        Buffer.__init__(self, buffer_id, url, width, height, QColor(255, 255, 255, 255))
        
        self.buffer_widget = QWebView()
        self.buffer_widget.resize(self.width, self.height)
        self.buffer_widget.setHtml(html_file.replace("**********", url))
        
        print("Create buffer: %s" % buffer_id)
        
    def resize_buffer(self, width, height):
        self.width = width
        self.height = height
        self.buffer_widget.resize(self.width, self.height)
