#!/bin/sh

set -e

echo "[EAF] Installing python dependencies..."
brew install python3 taglib
pip3 install epc PyQt5 PyQt5-sip PyQtWebEngine PyMuPDF qrcode qtconsole retrying mac-app-frontmost pytaglib psutil

echo "[EAF] Installing npm dependencies..."
brew install node
npm install

echo "[EAF] Installing brew dependencies..."
brew install aria2
brew tap filebrowser/tap
brew install filebrowser
