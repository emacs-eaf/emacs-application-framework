# What is Emacs Application Framework?
Emacs Application Framework is a development framework, where developers can develop any PyQt program, and integrate it into Emacs.

This framework mainly implements three functions:
1. Integrate PyQt program window into Emacs Frame using Xlib Reparent technology
2. Listening to EAF buffer's keyboard event flow and controlling the keyboard input of PyQt program via DBus IPC
3. Created a window compositer to make the PyQt program window adapt Emacs's Window/Buffer design

Using this framework, you can use PyQt to develop powerful GUI programs to extend Emacs.

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

```Bash
    sudo pip3 install dbus-python pymupdf grip qrcode python-xlib pyqt5 pyqtwebengine
```

2. Clone this repository and add below code in your ~/.emacs

```Elisp
(require 'eaf)
```

### Package description.

| Debian Package | Use for                                          |
| :--------      | :----                                            |
| dbus-python    | DBus IPC for python and elisp                    |
| pymupdf        | Render engine required for PDF Viewer            |
| grip           | Markdown render server for Markdown Previewer    |
| qrcode         | Render local file QR code                        |
| python-xlib    | Stick app window into emacs frame                |
| pyqt5          | GUI library required for application development |
| pyqtwebengine  | QtWebEngine for browser application              |

### Or run EAF with docker

If you prefer to run linux in a docker, you can read [Run EAF with docker](./docker/README.md)

### Why this awesome framework doesn't works with MacOS?
There are mainly three obstacles:
1. I can't make dbus/python-dbus works on MacOS High Sierra
2. This framework need use X11 reparent to stick Qt5 window to emacs frame, but i don't know how to make X11 works on MacOS.
3. Qt5 QGraphicsView/QGraphicsScene can't work MacOS, specify QGraphicsVideoItem can't work.

## Usage

| Application Name   | Launch                                        |
| :--------          | :----                                         |
| Browser            | Type 'eaf-browser' RET https://www.google.com |
| PDF Viewer         | Type 'eaf-open' RET pdf filepath             |
| Video Player       | Type 'eaf-open' RET video filepath          |
| Image Viewer       | Type 'eaf-open' RET image filepath    |
| Markdown previewer | Type 'eaf-open' RET markdown filepath         |
| Org file previewer | Type 'eaf-open' RET org filepath              |
| Camera             | Type 'eaf-camera'                             |
| Demo               | Type 'eaf-demo'                               |
| File Transfer      | Type 'eaf-show-file-qrcode'                   |
|                    | Or use `dired-show-file-qrcode' in dired mode |
| File Uploader      | Type 'eaf-upload-file'                        |
| Air Share          | Type 'eaf-air-share'                          |

Please check [Key binding](./docs/KEYBINDING.md) to check keybinding of application.

```
NOTE:
EAF use DBus' session bus, it must running in general user.
Please don't run EAF with root user, root user just can access DBus's system bus.
```

## Settings

### Proxy
If you can't access most awesome internet services like me, you perhaps need proxy settings like below:

```Elisp
(setq eaf-http-proxy-host "127.0.0.1")
(setq eaf-http-proxy-port "1080")
```

Then EAF browser is free! ;)

### Markdown Previewer
If you use markdown previewer, you need access to a [Personal access token](https://github.com/settings/tokens/new?scopes=), fill something in "Token description" and click button "Generate token" to get your personal token, then set token with code:

```Elisp
(setq eaf-grip-token "yourtokencode")
```

Otherwise, github will popup "times limit" error because so many peope use grip. ;)

## Report bug
If you have any problem with EAF, please use command "emacs -Q" to start Emacs without any customizations.

Then re-test your workflow. If "emacs -Q" works fine, it's must be something wrong with your emacs config file.

If the problem persists, please [report bug here](https://github.com/manateelazycat/emacs-application-framework/issues/new).

## Join Us
Do you want to make Emacs a real operating system?

Do you want to live in emacs more comfortably?

Want to create unparalleled plugins to extend emacs?

[Let's hack together!](./docs/HACKING.md)

## 打赏
如果我的作品让你的生活充满快乐, 欢迎请我喝瓶啤酒, 哈哈哈哈

<p float="left">
    <img src="./screenshot/alipay.jpg" width="188">
    <img src="./screenshot/wechat.jpg" width="200">
</p>
