# What is Emacs Application Framework?
Emacs Application Framework is a development framework that developers can develop any PyQt program and integrate into Emacs.

This framework mainly implements three functions:
1. Integrate PyQt program window into Emacs Frame using Xlib Reparent technology
2. Listening to EAF buffer's keyboard event flow and controlling the keyboard input of PyQt program via DBus IPC
3. Created a window compositer to make the PyQt program window adapt Emacs's Window/Buffer design

Using this framework, you can use PyQt develop powerful graphics programs to extend Emacs

## Screenshots of EAF

| Browser                                          | Markdown Previewer                                          |
| :--------:                                       | :----:                                                      |
| <img src="./screenshot/browser.gif" width="400"> | <img src="./screenshot/markdown_previewer.gif" width="400"> |

| Image Viewer                                          | Video Player                                          |
| :--------:                                            | :----:                                                |
| <img src="./screenshot/image_viewer.gif" width="400"> | <img src="./screenshot/video_player.gif" width="400"> |
|                                                       |                                                       |

| PDF Viewer                                          | Camera                                          |
| :--------:                                          | :----:                                          |
| <img src="./screenshot/pdf_viewer.gif" width="400"> | <img src="./screenshot/camera.gif" width="400"> |
|                                                     |                                                 |

| File Transfer                                          | File Uploader                                          |
| :--------:                                             | :----:                                                 |
| <img src="./screenshot/file_transfer.png" width="400"> | <img src="./screenshot/file_uploader.png" width="400"> |
|                                                        |                                                        |


| Air Share                                          | Org Previewer                                          |
| :--------:                                         | :--------:                                             |
| <img src="./screenshot/air_share.png" width="400"> | <img src="./screenshot/org_previewer.gif" width="400"> |
|                                                    |                                                        |

## Installation

1. Install python dependences:

    Make sure python3 and pip3 has install in your operating system, then execute below command:

```Bash
    sudo pip3 install dbus-python PyMuPDF grip qrcode pyqt5 python-xlib
```

2. Clone this repository and add below code in your ~/.emacs
```Elisp
    (require 'eaf)
```

### Package description.

| Package      | Use for                                          |
| :--------    | :----                                            |
| python-xlib  | Stick app window into emacs frame                |
| python-pyqt5 | GUI library required for application development |
| dbus-python  | DBus IPC for python and elisp                    |
| PyMuPDF      | Render engine required for PDF Viewer            |
| grip         | Markdown render server for Markdown Previewer    |
| qrcode       | Render local file QR code                        |

### Why this awesome framework can't works with MacOS?
There are mainly three obstacles:
1. I can't make dbus/python-dbus works on MacOS High Sierra
2. This framework need use X11 reparent to stick Qt5 window to emacs frame, but i don't know how to make X11 works on MacOS.
3. Qt5 QGraphicsView/QGraphicsScene can't work MacOS, specify QGraphicsVideoItem can't work.

## Usage
```
NOTE:
EAF use DBus' session bus, it must running in general user.
Please don't run EAF with root user, root user just can access DBus's system bus.
```

| App                | Way to open                                   | Key                | Event                                    |
| :--------          | :----                                         | :-----:            | :----                                    |
| Browser            | URL                                           | Left Button        | Open link current tab                    |
|                    |                                               | Ctrl + Left Button | Open link in new tab                     |
| Markdown previewer | Type 'eaf-open' RET markdown filepath         |                    |                                          |
| Org file previewer | Type 'eaf-open' RET org filepath              |                    |                                          |
| Image Viewer       | Type 'eaf-open' RET IMAGE filepath            | j                  | Load next image in current directory     |
|                    |                                               | k                  | Load previous image in current directory |
| Video Player       | Type 'eaf-open' RET video filepath            | Space              | Play or Pause                            |
|                    |                                               | h                  | Seek backward                            |
|                    |                                               | l                  | Seek forward                             |
| Pdf Viewer         | Type 'eaf-open' RET PDF filepath              | j                  | Scroll up                                |
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
| File Uploader      | Type 'eaf-upload-file'                        |                    |                                          |
| Air Share          | Type 'eaf-air-share'                          |                    |                                          |

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
