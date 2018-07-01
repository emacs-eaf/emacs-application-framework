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
![img](./screenshot/pdf_viewer.gif)

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
| App          | Key                | Event                                    |
| --------     | :-----:            | :----                                    |
| Browser      | Left Button        | Open link current tab                    |
|              | Ctrl + Left Button | Open link in new tab                     |
| Image Viewer | j                  | Load next image in current directory     |
|              | k                  | Load previous image in current directory |
| Video Player | Space              | Play or Pause                            |
|              | h                  | Seek backward                            |
|              | l                  | Seek forward                             |
| Pdf Viewer   | j                  | Scroll up                                |
|              | k                  | Scroll down                              |
|              | Space              | Scroll up page                           |
|              | b                  | Scroll down page                         |
|              | ,                  | Scroll to end                            |
|              | .                  | Scroll to home                           |
|              | t                  | Switch scale mode                        |
|              | -                  | Zoom out                                 |
|              | =                  | Zoom in                                  |
|              | 0                  | Zoomn reset                              |
|              | g                  | Goto page                                |

### Why this awesome framework can't works with MacOS?
There are mainly three obstacles:
1. I can't make dbus/python-dbus works on MacOS High Sierra
2. This framework need use X11 reparent to stick Qt5 window to emacs frame, but i don't know how to make X11 works on MacOS.
3. Qt5 QGraphicsView/QGraphicsScene can't work MacOS, specify QGraphicsVideoItem can't work.

## How to develop new plugins?
[Developer manual](HACKING.md)

## Join Us
Do you want to make Emacs a real operating system?

Do you want to live in emacs more comfortably?

Want to create unparalleled plugins to extend emacs?

Join us, happy hacking!
