#!/usr/bin/env sh

set -eu

IGNORE_SYS_DEPS=""
IGNORE_PY_DEPS=""
IGNORE_NPM_DEPS=""

while [ $# -gt 0 ]; do
  key="$1"

  case $key in
    --ignore-sys-deps)
        IGNORE_SYS_DEPS=YES
        shift
        ;;
    --ignore-py-deps)
        IGNORE_PY_DEPS=YES
        shift
        ;;
    --ignore-npm-deps)
        IGNORE_NPM_DEPS=YES
        shift
        ;;
  esac
done

ARCH_PACKAGES="git nodejs npm \
python-pyqt5 python-pyqt5-sip python-pyqtwebengine wmctrl \
python-qrcode aria2 python-qtconsole taglib"

# System dependencies
if [ $IGNORE_SYS_DEPS ]; then
    :
elif [ "$(command -v apt)" ]; then
    # Missing in Ubuntu: filebrowser-bin
    # shellcheck disable=SC2015
    DEPS="git aria2 wmctrl"

    if [ ! "$(command -v node)" ]; then
        DEPS="${DEPS} nodejs"
    fi

    if [ ! "$(command -v npm)" ]; then
        DEPS="${DEPS} npm"
    fi

    sudo apt install -y ${DEPS} &&
        sudo apt install -y libglib2.0-dev &&
        sudo apt install -y python3-pyqt5 python3-sip python3-pyqt5.qtwebengine \
             python3-qrcode python3-feedparser \
             python3-markdown python3-qtconsole python3-pygit2 libtag1-dev ||
            { echo "[EAF] Failed to install dependency with apt."; exit 1;}

elif [ "$(command -v dnf)" ]; then
    # TODO: please add filebrowser-bin if it exists in Fedora repo.
    # shellcheck disable=SC2015
    sudo dnf -y install git nodejs npm aria2 wmctrl &&
        sudo dnf -y install glib2-devel &&
        sudo dnf -y install python3-qt5 python3-pyqt5-sip pyqtwebengine-devel \
             python3-qrcode python3-feedparser python3-markdown \
             python3-qtconsole python3-pygit2 taglib-devel ||
            { echo "[EAF] Failed to install dependencies with dnf."; exit 1; }

elif [ "$(command -v pacman)" ]; then
    # shellcheck disable=SC2086
    sudo pacman -Sy --noconfirm --needed $ARCH_PACKAGES ||
        { echo "[EAF] Failed to install dependency with pacman."; exit 1;}
    [ "$(command -v yay)" ] && yay -S --noconfirm filebrowser-bin
else
    echo "[EAF] Unsupported distribution/package manager. Here are the packages that needs to be installed:"
    for PCK in $ARCH_PACKAGES; do
        # shellcheck disable=SC2039
        echo "- ${PCK}"
    done
    echo "Please test their installation and submit an issue/PR to \
https://github.com/manateelazycat/emacs-application-framework for the script to be updated."
    exit 1
fi

# Python dependencies
if [ $IGNORE_PY_DEPS ]; then
    :
elif [ "$(command -v pip3)" ]; then
    pip3 install --user pymupdf epc retrying pytaglib psutil || { echo "[EAF] Failed to install dependency with pip3."; exit 1;}
elif [ "$(command -v pip)" ]; then
    pip install --user pymupdf epc retrying pytaglib psutil || { echo "[EAF] Failed to install dependency with pip."; exit 1;}
else
    echo "[EAF] Cannot find pip. Please install it before launching the script again."
    exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

if [ $IGNORE_NPM_DEPS ]; then
    :
else
    echo "[EAF] Installing npm dependencies..."
    (cd $SCRIPT_DIR && npm install) || { echo "[EAF] Failed to install dependency with npm."; exit 1;}
fi

echo "[EAF] install-eaf.sh finished."
