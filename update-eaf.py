#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import subprocess

subprocess.Popen(["git", "pull"], stdin = subprocess.PIPE, text=True).wait()
subprocess.Popen(["python", "install-eaf.py"], stdin = subprocess.PIPE, text=True).wait()
