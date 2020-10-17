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
from PyQt5.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QLabel
from core.buffer import Buffer

from pygit2 import Repository, discover_repository
from pygit2 import GIT_SORT_TOPOLOGICAL, GIT_SORT_REVERSE

from datetime import datetime

import json
import os

class AppBuffer(Buffer):
    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, module_path):
        Buffer.__init__(self, buffer_id, url, arguments, emacs_var_dict, module_path, True)

        arguments_dict = json.loads(arguments)

        self.add_widget(GitViewerWidget(buffer_id, config_dir, arguments_dict["directory"]))

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
            main_box = QVBoxLayout()
            main_box.setSpacing(0)
            main_box.setContentsMargins(0, 0, 0, 0)

            self.setLayout(main_box)

            # Add repo top area.
            self.repo_top_font_size = 18
            self.repo_top_area = QWidget()
            self.repo_top_layout = QVBoxLayout()
            self.repo_top_layout.setSpacing(0)
            self.repo_top_layout.setContentsMargins(30, 30, 30, 30)
            self.repo_top_area.setLayout(self.repo_top_layout)
            main_box.addWidget(self.repo_top_area)

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
            self.lastest_commit_layout.setContentsMargins(0, 30, 0, 30)
            self.lastest_commit_area.setLayout(self.lastest_commit_layout)
            self.repo_top_layout.addWidget(self.lastest_commit_area)

            self.lastest_commit_info = QLabel("Lastest: {}    {}...    {}    {}".format(
                lastest_commit.author.name,
                lastest_commit.message.split("\n")[0][:40],
                lastest_commit.hex[:7],
                datetime.utcfromtimestamp(lastest_commit.author.time).strftime('%Y-%m-%d %H:%M:%S')))
            self.lastest_commit_info.setStyleSheet("QLabel {color: #6C6C6C;}")
            self.lastest_commit_info.setFont(QFont('Arial', self.repo_top_font_size))
            self.lastest_commit_layout.addWidget(self.lastest_commit_info)

            self.lastest_commit_layout.addStretch(1)

            # Add commit status.

            # Add commit list.

            main_box.addStretch(1)

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
