# What is Emacs Application Framework?
Emacs Application Framework is a development framework that developers can develop any PyQt program and integrate into Emacs.

This framework mainly implements three functions:
1. Integrate PyQt program window into Emacs Frame using Xlib Reparent technology
2. Listening to EAF buffer's keyboard event flow and controlling the keyboard input of PyQt program via DBus IPC
3. Created a window compositer to make the PyQt program window adapt Emacs's Window/Buffer design

Using this framework, you can use PyQt develop powerful graphics programs to extend Emacs

## Screenshots of EAF

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

### File Uploader
![img](./screenshot/file_uploader.png)

### Air Share
![img](./screenshot/air_share.png)

## Installation

1. Install python libraries:

### ArchLinux

```Bash
sudo pacman -S python-xlib python-pyqt5
sudo pip install PyMuPDF grip qrcode
```

### Debian

```Bash
sudo apt-get update
sudo apt-get install python3-xlib python3-pyqt5
sudo pip3 instlal PyMuPDF grip qrcode
```

### Package description.

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

### Why this awesome framework can't works with MacOS?
There are mainly three obstacles:
1. I can't make dbus/python-dbus works on MacOS High Sierra
2. This framework need use X11 reparent to stick Qt5 window to emacs frame, but i don't know how to make X11 works on MacOS.
3. Qt5 QGraphicsView/QGraphicsScene can't work MacOS, specify QGraphicsVideoItem can't work.

## Usage
You will got all operations of EAF at [Manual](USAGE.md).

## Report bug
If you have any problem with eaf, please use command "emacs -Q" start Emacs first.

Then test again, if "emacs -Q" works fine, it's must be something wrong with your emacs config file.

If "emacs -Q" still have problem, please [report bug](https://github.com/manateelazycat/emacs-application-framework/issues/new)

## Join Us
Do you want to make Emacs a real operating system?

Do you want to live in emacs more comfortably?

Want to create unparalleled plugins to extend emacs?

[Let's hacking together!](HACKING.md)

## 打赏
如果我的作品让你的生活充满快乐, 欢迎请我喝瓶啤酒, 哈哈哈哈

<p float="left">
    <img src="./screenshot/alipay.jpg" width="188">
    <img src="./screenshot/wechat.jpg" width="200">
</p>
