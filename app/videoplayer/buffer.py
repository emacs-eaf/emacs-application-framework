#!/usr/bin/env python
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

from PyQt5.QtCore import QSizeF, Qt, QUrl
from PyQt5.QtGui import QBrush, QColor
from PyQt5.QtMultimedia import QMediaContent, QMediaPlayer
from PyQt5.QtMultimediaWidgets import QGraphicsVideoItem
from PyQt5.QtWidgets import QWidget, QGraphicsScene, QGraphicsView, QVBoxLayout
from buffer import Buffer

class VideoPlayerBuffer(Buffer):
    def __init__(self, buffer_id, url, width, height):
        Buffer.__init__(self, buffer_id, url, width, height, QColor(0, 0, 0, 255))

        self.add_widget(VideoPlayer())
        self.buffer_widget.resize(self.width, self.height)
        self.buffer_widget.play(url)

        self.fit_to_view = True

    def resize_buffer(self, width, height):
        self.width = width
        self.height = height
        self.buffer_widget.resize(self.width, self.height)

    def set_video_size(self, width, height):
        self.buffer_widget.video_item.setSize(QSizeF(width, height))

class VideoPlayer(QWidget):

    def __init__(self, parent=None):
        super(VideoPlayer, self).__init__(parent)

        self.media_player = QMediaPlayer(None, QMediaPlayer.VideoSurface)

        self.video_item = QGraphicsVideoItem()

        self.scene = QGraphicsScene(self)
        self.scene.setBackgroundBrush(QBrush(QColor(0, 0, 0, 255)))
        self.graphics_view = QGraphicsView(self.scene)
        self.graphics_view.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.graphics_view.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.graphics_view.setFrameStyle(0)
        self.graphics_view.setStyleSheet("QGraphicsView {background: transparent; border: 3px; outline: none;}")
        self.scene.addItem(self.video_item)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self.graphics_view)

        self.media_player.setVideoOutput(self.video_item)

    def play(self, url):
        self.media_player.setMedia(QMediaContent(QUrl.fromLocalFile(url)))
        self.media_player.play()
