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
from core.buffer import Buffer

class AppBuffer(Buffer):
    def __init__(self, buffer_id, url):
        Buffer.__init__(self, buffer_id, url, True, QColor(0, 0, 0, 255))

        self.add_widget(VideoPlayerWidget())
        self.buffer_widget.play(url)

    def all_views_hide(self):
        # Pause video before all views hdie, otherwise will got error "Internal data stream error".
        if self.buffer_widget.media_player.state() == QMediaPlayer.PlayingState:
            self.buffer_widget.media_player.pause()
            self.buffer_widget.video_need_replay = True

    def some_view_show(self):
        if self.buffer_widget.video_need_replay == True:
            self.buffer_widget.media_player.play()

class VideoPlayerWidget(QWidget):

    def __init__(self, parent=None):
        super(VideoPlayerWidget, self).__init__(parent)

        self.scene = QGraphicsScene(self)
        self.scene.setBackgroundBrush(QBrush(QColor(0, 0, 0, 255)))
        self.graphics_view = QGraphicsView(self.scene)
        self.graphics_view.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.graphics_view.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.graphics_view.setFrameStyle(0)
        self.graphics_view.setStyleSheet("QGraphicsView {background: transparent; border: 3px; outline: none;}")
        self.video_item = QGraphicsVideoItem()
        self.scene.addItem(self.video_item)

        self.layout = QVBoxLayout(self)
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.layout.addWidget(self.graphics_view)

        self.media_player = QMediaPlayer(None, QMediaPlayer.VideoSurface)
        self.media_player.setVideoOutput(self.video_item)

        self.video_need_replay = False
        self.video_seek_durcation = 3000 # in milliseconds

    def resizeEvent(self, event):
        self.video_item.setSize(QSizeF(event.size().width(), event.size().height()))
        QWidget.resizeEvent(self, event)

    def play(self, url):
        self.media_player.setMedia(QMediaContent(QUrl.fromLocalFile(url)))
        self.media_player.play()

    def seek_forward(self):
        video_position = self.media_player.position()
        self.media_player.setPosition(video_position + self.video_seek_durcation)

    def seek_backward(self):
        video_position = self.media_player.position()
        self.media_player.setPosition(max(video_position - self.video_seek_durcation, 0))

    def keyPressEvent(self, event):
        if event.key() == Qt.Key_Space:
            if self.media_player.state() == QMediaPlayer.PlayingState:
                self.media_player.pause()
                self.video_need_replay = False
            else:
                self.media_player.play()
                self.video_need_replay = True
        elif event.key() == Qt.Key_H:
            self.seek_backward()
        elif event.key() == Qt.Key_L:
            self.seek_forward()
