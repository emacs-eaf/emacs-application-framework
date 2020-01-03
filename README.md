# What is Emacs Application Framework?
Emacs Application Framework (EAF) is an application framework that extend GNU Emacs to an entire new universe of powerful GUI PyQt applications.

EAF is also super extensible, developers can develop any PyQt program, and integrate it into Emacs through EAF.

This framework mainly implements three functions:
1. Integrate PyQt program window into Emacs Frame using Xlib Reparent technology
2. Listening to EAF buffer's keyboard event flow and controlling the keyboard input of PyQt program via DBus IPC
3. Created a window compositer to make the PyQt program window adapt Emacs's Window/Buffer design

## Screenshots

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

| File Sender                                            | File Receiver                                          |
| :--------:                                             | :----:                                                 |
| <img src="./screenshot/file_transfer.png" width="400"> | <img src="./screenshot/file_uploader.png" width="400"> |
|                                                        |                                                        |


| Air Share                                          | Org Previewer                                          |
| :--------:                                         | :--------:                                             |
| <img src="./screenshot/air_share.png" width="400"> | <img src="./screenshot/org_previewer.gif" width="400"> |
|                                                    |                                                        |

| Terminal Emulator                                 | RSS Reader                                          |
| :--------:                                        | :------:                                            |
| <img src="./screenshot/terminal.png" width="400"> | <img src="./screenshot/rss_reader.gif" width="400"> |
|                                                   |                                                     |

## Install EAF
1. Clone this repository, add to ```load-path```, and add the following to ```.emacs```

```Elisp
(require 'eaf)
```

If you use [use-package](https://github.com/jwiegley/use-package), a sample configuration has been provided.

```Elisp
(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :custom
  (eaf-find-alternate-file-in-dired t)
  :config
  (eaf-bind-key scroll_up "RET" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "DEL" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding))
```

2. Make sure to have ```python3``` installed, and use ```pip3``` to install EAF dependencies:

```Elisp
sudo pip3 install dbus-python python-xlib pyqt5 pyqtwebengine pymupdf grip qrcode feedparser
```

3. Compile ```qtermwidget-git``` use the following command:
```Elisp
git clone https://github.com/lxqt/qtermwidget.git --depth=1
mkdir build && cd build
cmake .. -DQTERMWIDGET_BUILD_PYTHON_BINDING=ON -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_INSTALL_LIBDIR=/usr
make
sudo make install
```

Package info:

| Package         | Package Repo        | Classification | Package Description                                |
| :--------       | :----               | :------        | :------                                            |
| dbus-python     | pip3                | Core           | DBus IPC to communicate python with elisp          |
| python-xlib     | pip3                | Core           | Stick app window into Emacs frame                  |
| pyqt5           | pip3                | Core           | GUI library required for applications              |
| pyqtwebengine   | pip3                | Core           | Browser: QtWebEngine for browser application       |
| pymupdf         | pip3                | Application    | PDF Viewer: Rendering engine                       |
| grip            | pip3                | Application    | Markdown Previewer: Markdown render server         |
| qrcode          | pip3                | Application    | File Transfer: Render QR code pointing local files |
| feedparser      | pip3                | Application    | RSS Reader: feed parser                            |
| qtermwidget-git | compile from source | Application    | Terminal: QTermWidget, PyQt5 terminal emulator     |

Pymupdf install note:

1. Before install pymupdf, user should install mupdf and libmupdf, for example:
   ```
   sudo apt-get install mupdf libmupdf-dev
   ```
2. Find the version of mupdf installed, for example: 1.14,

3. Try to install pymupdf with the below command:

   ```
   pip3 install pymupdf==1.14
   ```

   you will get error:

   ```
   Looking in indexes: http://mirrors.aliyun.com/pypi/simple/
   Collecting pymupdf==1.14
   Could not find a version that satisfies the requirement pymupdf==1.14 (from versions: 1.11.2, 1.12.5, 1.13.20, 1.14.19.post2, 1.14.19.2, 1.14.20, 1.14.21, 1.16.0, 1.16.1, 1.16.2, 1.16.3, 1.16.4, 1.16.5, 1.16.6, 1.16.7, 1.16.8, 1.16.9, 1.16.10)
   No matching distribution found for pymupdf==1.14
   ```
   
3. Finally run command:
   ```
   pip3 install pymupdf==1.14.21
   ```


## Launch EAF Applications
| Application Name   | Launch                                                                  |
| :--------          | :----                                                                   |
| Browser            | Type 'eaf-browser' RET https://www.google.com                           |
| PDF Viewer         | Type 'eaf-open' RET pdf filepath                                        |
| Video Player       | Type 'eaf-open' RET video filepath                                      |
| Image Viewer       | Type 'eaf-open' RET image filepath                                      |
| Markdown previewer | Type 'eaf-open' RET markdown filepath                                   |
| Org file previewer | Type 'eaf-open' RET org filepath                                        |
| Camera             | Type 'eaf-open-camera'                                                  |
| Terminal           | Type 'eaf-open-terminal'                                                |
| File Sender        | Type 'eaf-file-sender-qrcode', or use 'eaf-file-sender-qrcode-in-dired' |
| File Receiver      | Type 'eaf-file-receiver-qrcode'                                         |
| Airshare           | Type 'eaf-file-transfer-airshare'                                       |
| RSS Reader         | Type 'eaf-open-rss-reader'                                              |
| Demo               | Type 'eaf-open-demo'                                                    |

To run `eaf-open` on the current file under the cursor in `dired`, call `eaf-open-this-from-dired`.

You can also use the command `eaf-google-it` as a shortcut to quickly google `symbol-at-point` or an active region using EAF Browser. Similarly, you can create your own functions if you prefer to use DuckDuckGo or other search engines.

```
NOTE:
EAF use DBus' session bus, it must run in general user.
Please don't run EAF with root user, root user just can access DBus's system bus.
```

## FAQ and Support

### How about EXWM? What makes EAF special?
1. EAF gives you control over your program, while satisfying Emacs window design model. [EXWM](https://github.com/ch11ng/exwm) is only a tiling WM, that combines different applications together in an Emacs-like fashion. However, EXWM is unable to split the same application into two different windows while displaying different same application at the same time. For example, EAF is able to display same PDF on two different windows.
2. EAF essentially provides Emacs a secondary scripting language ([this topic had been brought up again in EmacsConf2019](https://media.emacsconf.org/2019/26.html) and [reddit](https://www.reddit.com/r/emacs/comments/e1wfoe/emacs_the_editor_for_the_next_40_years/)). Emacs Lisp doesn't render graphics very well, especially it doesn't play nicely with the browser. This is (an example of) where PyQt5 can come in handy.
3. With DBus IPC, EAF can use Python to control Emacs Lisp, conversely also true that Emacs Lisp can control Qt rendering and Python code.
4. EXWM, as a Windows Manager, does its job very well. Therefore, it doesn't have control and doesn't care at all how other program functions. For example, EXWM cannot control keyboard events of other programs. On the other hand, you can configure them in EAF either using existing features (see above) or write code to contribute to this repository.
5. From a higher point of view, EAF is using Emacs' design principles to extend GUI programs. You have the ability to control good GUI programs using Emacs keybindings. To achieve the ultimate goal: live in Emacs ;)

### Why this awesome framework doesn't works with MacOS or Windows?
There are mainly three obstacles:
1. We don't use MacOS or Windows
2. This framework need use X11 reparent to stick Qt5 window to emacs frame, but had trouble making X11 to work on MacOS.
3. Had trouble making dbus/python-dbus work on MacOS High Sierra
4. Had trouble making Qt5 QGraphicsView/QGraphicsScene work on MacOS, specifically QGraphicsVideoItem cannot work.
5. If you figure them out, PRs always welcome

### Why not support Wayland?
EAF use X11 XReparent technology to stick Qt5 window on Emacs buffer area, Wayland doesn't not support XReparent.

We recommend our users to use KDE, it's stable enough and supports X11 XReparent technology.

### Github Personal Access Tokens?
If you use EAF Markdown Previewer, you need the access to a [Personal access token](https://github.com/settings/tokens/new?scopes=), fill something in "Token description" and click button "Generate token" to get your personal token, then set token with code:

```Elisp
(setq eaf-grip-token "yourtokencode")
```

Otherwise, github will popup "times limit" error because so many people use grip. ;)

### Proxy
If you can't access most awesome internet services like me, you can configure the proxy settings.

```Elisp
(setq eaf-http-proxy-host "127.0.0.1")
(setq eaf-http-proxy-port "1080")
```

### "undefined symbol" error
If you got "undefined symbol" error after start EAF, and you use Arch Linux, yes, it's a bug of Arch.

You need use pip install all dependences after you upgrade your Arch system, then undefine symbol error will fix.

```Elisp
sudo pip3 install dbus-python python-xlib pyqt5 pyqtwebengine pymupdf grip qrcode feedparser --force-reinstall
```

## Report bug

For any installation and configuration assistance, please read the [Wiki](https://github.com/manateelazycat/emacs-application-framework/wiki) first!

If you have any problem with EAF, please use command "emacs -Q" to start Emacs without any customizations. Then re-test your workflow. If "emacs -Q" works fine, it's must be something wrong with your emacs config file.

If the problem persists, please [report bug here](https://github.com/manateelazycat/emacs-application-framework/issues/new).

## Join Us
Do you want to make Emacs a real operating system?

Do you want to live in emacs more comfortably?

Want to create unparalleled plugins to extend emacs?

[Let's hack together!](https://github.com/manateelazycat/emacs-application-framework/wiki/Hacking)

## 打赏
如果我的作品让你的生活充满快乐, 欢迎请我喝瓶啤酒, 哈哈哈哈

<p float="left">
    <img src="./screenshot/alipay.jpg" width="188">
    <img src="./screenshot/wechat.jpg" width="200">
</p>
