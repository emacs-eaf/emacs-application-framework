# What is Emacs Application Framework?
Emacs Application Framework is a development framework that developers can develop any PyQt program and integrate into Emacs.

This framework mainly implements three functions:
1. Integrate PyQt program window into Emacs Frame using Xlib Reparent technology
2. Listening to EAF buffer's keyboard event flow and controlling the keyboard input of PyQt program via DBus IPC
3. Created a window compositer to make the PyQt program window adapt Emacs's Window/Buffer design

Using this framework, you can use PyQt develop powerful graphics programs to extend Emacs

## Some screenshots

### Browser
![img](./screenshot/browser.gif)

### Image Viewer
![img](./screenshot/image_viewer.gif)

### Video Player
![img](./screenshot/video_player.gif)

### PDF Player
![img](./screenshot/video_player.gif)

## Installation

1. Install python libraries:
```Bash
sudo pacman -S python-xlib python-pyqt5 python-pymediainfo
sudo pip install git+https://github.com/wbsoft/python-poppler-qt5.git
```

2. Clone this repository and add below code in your ~/.emacs
```Elisp
(require 'eaf)
```

## Usage

```
M-x eaf-open
```

Such as,
* type www.google.com to open browser, Ctrl + LeftButton open link in new tab
* type /path/image_file to open image viewer, and press key j or k to select other image in same directory
* type /path/video_file to open video player, press space to toggle play/pause status, press H or L to seek video position.
* type /path/pdf_file to open pdf viewer.

## How to develop new plugins?
[Developer manual](HACKING.md)

## Join Us
Do you want to make Emacs a real operating system?

Do you want to live in emacs more comfortably?

Want to create unparalleled plugins to extend emacs?

Join us, happy hacking!
