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

### Markdown Previewer
![img](./screenshot/markdown_previewer.gif)

### Image Viewer
![img](./screenshot/image_viewer.gif)

### Video Player
![img](./screenshot/video_player.gif)

### PDF Player
![img](./screenshot/pdf_viewer.gif)

### Camera
![img](./screenshot/camera.gif)

### File Transfer
![img](./screenshot/file_transfer.png)

### Air Share
![img](./screenshot/air_share.png)

## Installation

1. Install python libraries:
```Bash
sudo pacman -S python-xlib python-pyqt5
sudo pip install PyMuPDF grip qrcode
```

| Package      | Use for                                          |
| :--------    | :----                                            |
| python-xlib  | Stick app window into emacs frame                |
| python-pyqt5 | GUI library required for application development |
| PyMuPDF      | Render engine required for PDF Viewer            |
| grip         | Markdown render server for Markdown Previewer    |
| qrcode       | Render local file QR code                        |

2. Clone this repository and add below code in your ~/.emacs
```Elisp
(require 'eaf)
```

## Usage

```
M-x eaf-open
```
| App                | Way to open                                   | Key                | Event                                    |
| :--------          | :----                                         | :-----:            | :----                                    |
| Browser            | URL                                           | Left Button        | Open link current tab                    |
|                    |                                               | Ctrl + Left Button | Open link in new tab                     |
| Markdown previewer | Markdown file path                            |                    |                                          |
| Image Viewer       | Image file path                               | j                  | Load next image in current directory     |
|                    |                                               | k                  | Load previous image in current directory |
| Video Player       | Video file path                               | Space              | Play or Pause                            |
|                    |                                               | h                  | Seek backward                            |
|                    |                                               | l                  | Seek forward                             |
| Pdf Viewer         | Pdf file path                                 | j                  | Scroll up                                |
|                    |                                               | k                  | Scroll down                              |
|                    |                                               | Space              | Scroll up page                           |
|                    |                                               | b                  | Scroll down page                         |
|                    |                                               | ,                  | Scroll to end                            |
|                    |                                               | .                  | Scroll to home                           |
|                    |                                               | t                  | Switch scale mode                        |
|                    |                                               | -                  | Zoom out                                 |
|                    |                                               | =                  | Zoom in                                  |
|                    |                                               | 0                  | Zoomn reset                              |
|                    |                                               | g                  | Goto page                                |
| Camera             | Type 'eaf-camera'                             |                    |                                          |
| Demo               | Type 'eaf-demo'                               |                    |                                          |
| File Transfer      | Type 'eaf-show-file-qrcode'                   |                    |                                          |
|                    | Or use `dired-show-file-qrcode' in dired mode |                    |                                          |
| Air Share          | Type 'eaf-air-share'                          |                    |                                          |

## Report bug
If you have any problem with eaf, please use command "emacs -Q" start Emacs first.

Then test again, if "emacs -Q" works fine, it's must be something wrong with your emacs config file.

If "emacs -Q" still have problem, please [report bug](https://github.com/manateelazycat/emacs-application-framework/issues/new)

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

## 打赏
如果我的作品让你的生活充满快乐, 欢迎请我喝瓶啤酒, 哈哈哈哈

<p float="left">
    <img src="./screenshot/alipay.jpg" width="200">
    <img src="./screenshot/wechat.jpg" width="200">
</p>
