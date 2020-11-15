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

from PyQt5.QtGui import QColor, QFont
from PyQt5.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QLabel, QListView, QStackedWidget, QPushButton
from PyQt5.QtCore import QStringListModel

from core.buffer import Buffer
from core.utils import interactive

from pygit2 import Repository, discover_repository
from pygit2 import GIT_SORT_TOPOLOGICAL, GIT_SORT_REVERSE

from datetime import datetime

import json
import os

class AppBuffer(Buffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path):
        Buffer.__init__(self, buffer_id, url, arguments, emacs_var_dict, module_path, False)

        self.background_color = QColor(0, 0, 0)

        arguments_dict = json.loads(arguments)

        self.add_widget(GitViewerWidget(buffer_id, config_dir, arguments_dict["directory"]))

    def show_status_info(self):
        self.buffer_widget.info_stacked_widget.setCurrentIndex(0)

    def show_commit_info(self):
        self.buffer_widget.info_stacked_widget.setCurrentIndex(1)

    def show_branch_info(self):
        self.buffer_widget.info_stacked_widget.setCurrentIndex(2)

    def show_submodule_info(self):
        self.buffer_widget.info_stacked_widget.setCurrentIndex(3)

class GitViewerWidget(QWidget):

    def __init__(self, buffer_id, config_dir, directory):
        super(GitViewerWidget, self).__init__()

        repo_path = discover_repository(directory)
        if repo_path:
            repo = Repository(repo_path)
            commit_size = 0
            lastest_commit = None
            for commit in repo.walk(repo.head.target, GIT_SORT_TOPOLOGICAL):
                commit_size += 1

                if not lastest_commit:
                    lastest_commit = commit

            remote_branch_size = 0
            for branch in repo.branches.remote:
                if branch != "origin/HEAD" and branch != "origin/master":
                    remote_branch_size += 1

            # Change background.
            self.setStyleSheet("background-color: #000000");

            # Add main box.
            main_layout = QVBoxLayout()
            main_layout.setSpacing(0)
            main_layout.setContentsMargins(0, 0, 0, 0)

            self.setLayout(main_layout)

            # Add repo top area.
            self.repo_top_font_size = 16
            self.lastest_commit_font_size = 16
            self.repo_top_area = QWidget()
            self.repo_top_layout = QVBoxLayout()
            self.repo_top_layout.setSpacing(0)
            self.repo_top_layout.setContentsMargins(30, 30, 30, 30)
            self.repo_top_area.setLayout(self.repo_top_layout)
            main_layout.addWidget(self.repo_top_area)

            # Add repo title.
            self.repo_title = QLabel(repo.path)
            self.repo_title.setStyleSheet("QLabel {color: #E98123;}")
            self.repo_title.setFont(QFont('Arial', 20))
            self.repo_top_layout.addWidget(self.repo_title)

            # Add summary info.
            self.repo_summary_area = QWidget()
            self.repo_summary_layout = QHBoxLayout()
            self.repo_summary_layout.setSpacing(30)
            self.repo_summary_layout.setContentsMargins(0, 0, 0, 0)
            self.repo_summary_area.setLayout(self.repo_summary_layout)
            self.repo_top_layout.addWidget(self.repo_summary_area)

            # Add head info.
            self.head_info = QLabel("{}    {}    {}    {}".format(
                repo.head.shorthand,
                str(remote_branch_size) + " branches",
                str(commit_size) + " commits",
                format_bytes(get_dir_size(repo_path))
            ))
            self.head_info.setStyleSheet("QLabel {color: #C46C1D;}")
            self.head_info.setFont(QFont('Arial', self.repo_top_font_size))
            self.repo_summary_layout.addWidget(self.head_info)

            self.repo_summary_layout.addStretch(1)

            # Add lastest commit info.
            self.lastest_commit_area = QWidget()
            self.lastest_commit_layout = QHBoxLayout()
            self.lastest_commit_layout.setSpacing(30)
            self.lastest_commit_layout.setContentsMargins(0, 30, 0, 0)
            self.lastest_commit_area.setLayout(self.lastest_commit_layout)
            self.repo_top_layout.addWidget(self.lastest_commit_area)

            self.lastest_commit_info = QLabel("Lastest commit:\n{}    {}    {}    \n{}".format(
                lastest_commit.author.name,
                lastest_commit.hex,
                datetime.utcfromtimestamp(lastest_commit.author.time).strftime('%Y-%m-%d %H:%M:%S'),
                lastest_commit.message))
            self.lastest_commit_info.setStyleSheet("QLabel {color: #6C6C6C;}")
            self.lastest_commit_info.setFont(QFont('Arial', self.lastest_commit_font_size))
            self.lastest_commit_layout.addWidget(self.lastest_commit_info)

            self.lastest_commit_layout.addStretch(1)

            # Add info box.
            info_box = QWidget()
            info_area_layout = QHBoxLayout()
            info_area_layout.setSpacing(30)
            info_area_layout.setContentsMargins(30, 0, 30, 30)
            info_box.setLayout(info_area_layout)

            # Add category panel.
            category_panel_listview = QListView()
            category_panel_listview.setSpacing(10)
            category_panel_listview.setStyleSheet("QListView {font-size: 40px;}")
            category_panel_model = QStringListModel()
            category_panel_list = ["1: Status", "2: Commit", "3: Branch", "4: Submodule"]
            category_panel_model.setStringList(category_panel_list)
            category_panel_listview.setModel(category_panel_model)

            info_area_layout.addWidget(category_panel_listview)
            info_area_layout.setStretchFactor(category_panel_listview, 1)

            # Add content widget.
            self.info_stacked_widget = QStackedWidget()

            self.git_status_widget = QPushButton("Git status")
            self.git_commit_widget = QPushButton("Git commit")
            self.git_branch_widget = QPushButton("Git branch")
            self.git_submodule_widget = QPushButton("Git submodule")

            self.info_stacked_widget.addWidget(self.git_status_widget)
            self.info_stacked_widget.addWidget(self.git_commit_widget)
            self.info_stacked_widget.addWidget(self.git_branch_widget)
            self.info_stacked_widget.addWidget(self.git_submodule_widget)

            self.info_stacked_widget.setCurrentIndex(0)

            info_area_layout.addWidget(self.info_stacked_widget)
            info_area_layout.setStretchFactor(self.info_stacked_widget, 4)

            # Add help panel.
            help_panel_listview = QListView()
            help_panel_listview.setSpacing(10)
            help_panel_listview.setStyleSheet("QListView {font-size: 40px;}")
            help_panel_model = QStringListModel()
            help_panel_list = ["Press l: Git pull",
                               "Press u: Git push",
                               "Press a: Commit all",
                               "Press s: Commit file",
                               "Press k: Cancel file"]
            help_panel_model.setStringList(help_panel_list)
            help_panel_listview.setModel(help_panel_model)

            info_area_layout.addWidget(help_panel_listview)
            info_area_layout.setStretchFactor(help_panel_listview, 1)

            main_layout.addWidget(info_box)

def get_dir_size(start_path = '.'):
    total_size = 0
    for dirpath, dirnames, filenames in os.walk(start_path):
        for f in filenames:
            fp = os.path.join(dirpath, f)
            # skip if it is symbolic link
            if not os.path.islink(fp):
                total_size += os.path.getsize(fp)

    return total_size

def format_bytes(size):
    # 2**10 = 1024
    power = 2**10
    n = 0
    power_labels = {0 : '', 1: 'K', 2: 'M', 3: 'G', 4: 'T'}
    while size > power:
        size /= power
        n += 1
    return str(round(size, 2)) + power_labels[n]+'B'
