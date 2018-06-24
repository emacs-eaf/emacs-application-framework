# What is Emacs Application Framework?
Emacs Application Framework is a development framework that developers can develop any PyQt program and integrate into Emacs.

This framework mainly implements three functions:
1. Integrate PyQt program window into Emacs Frame using Xlib Reparent technology
2. Listening to EAF buffer's keyboard event flow and controlling the keyboard input of PyQt program via DBus IPC
3. Created a window compositer to make the PyQt program window adapt Emacs's Window/Buffer design

Using this framework, you can use Python language to quickly develop powerful graphics programs to extend Emacs

I have develop below plugins:

### Browser
![img](./screenshot/browser.png)

### Image Viewer
![img](./screenshot/image_viewer.png)

### Video Player
![img](./screenshot/video_player.png)

## Installation

1. Install PyQt5 and Python-Xlib (below commands use for archlinux)
```
sudo pacman -S python-xlib python-pyqt5
```

2. Clone this repository and add below code in your ~/.emacs
```
(require 'eaf)
```

## Usage

```
M-x eaf-open
```

Such as,
* type www.google.com to open browser
* type /path/image.jpg to open image viewer, and press key j or k to select other image in same directory
* type /path/video.ogg to open video player, video player just support ogg file because it implement by HTML5 video tag

## Join Us
Do you want to make Emacs a real operating system?
Do you want to live in emacs more comfortably?
Want to create unparalleled plugins to extend emacs?

Join us!

Any suggestions and patches are welcome, happy hacking!
