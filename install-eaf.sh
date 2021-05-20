#!/usr/bin/env sh

set -eu

ARCH_PACKAGES="git nodejs npm \
python-pyqt5 python-pyqt5-sip python-pyqtwebengine wmctrl \
python-qrcode aria2 python-qtconsole taglib"

# System dependencies
if [ "$(command -v apt)" ]; then
    # Missing in Ubuntu: filebrowser-bin
    # shellcheck disable=SC2015
    DEPS=("git" "aria2" "wmctrl")

    if ! command -v node &> /dev/null; then
        DEPS+=("nodejs")
    fi

    if ! command -v npm &> /dev/null; then
        DEPS+=("npm")
    fi

    sudo apt -y install "${DEPS[@]}" &&
        sudo apt -y install libglib2.0-dev &&
        sudo apt -y install python3-pyqt5 python3-sip python3-pyqt5.qtwebengine \
             python3-qrcode python3-feedparser \
             python3-markdown python3-qtconsole python3-pygit2 libtag1-dev ||
            { echo "Failed to install dependency with apt."; exit 1;}

elif [ "$(command -v dnf)" ]; then
    # TODO: please add filebrowser-bin if it exists in Fedora repo.
    # shellcheck disable=SC2015
    sudo dnf -y install git nodejs npm aria2 wmctrl &&
        sudo dnf -y install glib2-devel &&
        sudo dnf -y install python3-qt5 python3-pyqt5-sip pyqtwebengine-devel \
             python3-qrcode python3-feedparser python3-markdown \
             python3-qtconsole python3-pygit2 taglib-devel ||
            { echo "Failed to install dependencies with dnf."; exit 1; }

elif [ "$(command -v pacman)" ]; then
    # shellcheck disable=SC2086
    sudo pacman -Sy --noconfirm --needed $ARCH_PACKAGES ||
        { echo "Failed to install dependency with pacman."; exit 1;}
    [ "$(command -v yay)" ] && yay -S --noconfirm filebrowser-bin
else
    echo "Unsupported distribution/package manager. Here are the packages that needs to be installed:"
    for PCK in $ARCH_PACKAGES; do
        # shellcheck disable=SC2039
        echo "- ${PCK}"
    done
    echo "Please test their installation and submit an issue/PR to \
https://github.com/manateelazycat/emacs-application-framework for the script to be updated."
    exit 1
fi

echo "Installing npm dependencies..."
npm install || { echo "Failed to install dependency with npm."; exit 1;}

# Python dependencies
if [ "$(command -v pip3)" ]; then
    pip3 install --user pymupdf epc retrying pytaglib psutil || { echo "Failed to install dependency with pip3."; exit 1;}
elif [ "$(command -v pip)" ]; then
    pip install --user pymupdf epc retrying pytaglib psutil || { echo "Failed to install dependency with pip."; exit 1;}
else
    echo "Cannot find pip. Please install it before launching the script again."
    exit 1
fi

echo "eaf-install.sh finished."
