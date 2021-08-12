English | [简体中文](./README.zh-CN.md)

<p align="center">
  <img width="250" height="250" src="./EAF_Logo.png">
</p>

# Emacs Application Framework
Emacs Application Framework (EAF) is a GUI application framework for Emacs that revolutionizes its multimedia and graphical capabilities to ultimately *Live in Emacs*.

EAF supports Linux, Windows, MacOS, and FreeBSD.

## EAF Vision
Emacs, the extensible *text editor*, is more than 45 years old, older than most operating systems people use today, as old as the first UNIX system. During the 45 years of development, the brightest hackers from all over the world have been contributing their intelligence and imagination, together constructed the most comprehensive and rich ecosystem that you can find in any software to date.

If you are a hacker who works with numerous programming languages, who desires maximum freedom, extensibility and introspectivity into your tools, and into keyboard/text centrism, Emacs will be your best bet.

However, one of the greatest disadvantages of Emacs is how *old* it is. Emacs lacks performance as it doesn't have proper multithreading and modern graphical support that you can expect on many GUI applications today (such as IDEA or VSCode), resulting in the extensibility of Emacs to be limited when it shouldn't have to be.

The vision of the EAF project is that, while retaining the culture and the ecosystem of Emacs, use Python, Qt5, and JavaScript to extend Emacs' graphical capabilities, with the hope to ultimately *Live in Emacs*.


## Applications
EAF is an extensible framework. It comes with a number of useful applications, and you can develop any Qt5 application and integrate it into Emacs.

- [Browser](https://github.com/emacs-eaf/eaf-browser): Full-featured browser
- [PDF Viewer](https://github.com/emacs-eaf/eaf-pdf-viewer): The fastest PDF reader
- [Terminal](https://github.com/emacs-eaf/eaf-terminal): Full-featured terminal
- [JS Video Player](https://github.com/emacs-eaf/eaf-js-video-player): Video player based on plyr.js
- [Video Player](https://github.com/emacs-eaf/eaf-video-player): Video player base on Qt
- [Markdown Previewer](https://github.com/emacs-eaf/eaf-markdown-previewer): Markdown previewer in Emacs
- [Org Previewer](https://github.com/emacs-eaf/eaf-org-previewer): Org previewer in Emacs
- [Music Player](https://github.com/emacs-eaf/eaf-music-player): Music player, support playlist and audio visual
- [File Manager](https://github.com/emacs-eaf/eaf-file-manager): File manager, support file real-time preview
- [Mindmap](https://github.com/emacs-eaf/eaf-mindmap): Mindmap with full-featured keyboard operation
- [Jupyter](https://github.com/emacs-eaf/eaf-jupyter): Jupyter in Emacs
- [Image Viewer](https://github.com/emacs-eaf/eaf-image-viewer): Picture viewer supporting real-time zoom rotation
- [Camera](https://github.com/emacs-eaf/eaf-camera): Camera in Emacs
- [System Monitor](https://github.com/emacs-eaf/eaf-system-monitor): System monitor base on Vue.js
- [Netease Cloud Music](https://github.com/emacs-eaf/eaf-netease-cloud-music): EAF frontend for netease cloud music
- [File Browser](https://github.com/emacs-eaf/eaf-file-browser): Browse file in mobile phone
- [File Share](https://github.com/emacs-eaf/eaf-file-sender): Share file between Emacs and mobile phone
- [Airshare](https://github.com/emacs-eaf/eaf-airshare): Share text between Emacs and mobile phone
- [Demo](https://github.com/emacs-eaf/eaf-demo): Application demo base on Qt
- [Vue Demo](https://github.com/emacs-eaf/eaf-vue-demo): Application demo base on Vue.js

## EmacsConf2020 - Extend Emacs to Modern GUI Applications with EAF
- EAF talk & demo at EmacsConf2020, presented to you by [MatthewZMD](https://github.com/MatthewZMD)
- Homepage (Q&A included): https://emacsconf.org/2020/talks/34/
- Youtube link:

[![EmacsConf2020 - Extend Emacs to Modern GUI Applications with EAF](https://img.youtube.com/vi/HK_f8KTuR0s/0.jpg)](https://www.youtube.com/watch?v=HK_f8KTuR0s)

## Install
#### 1. Download EAF

```Bash
git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git ~/.emacs.d/site-lisp/emacs-application-framework/
```

- Alternatively, you can use a [Quelpa recipe](https://github.com/quelpa/quelpa)

    ```Emacs-lisp
    (quelpa '(eaf :fetcher github
                  :repo  "manateelazycat/emacs-application-framework"
                  :files ("*")))
    ```

#### 2. Install EAF dependencies

##### If you use GNU/Linux,

```Bash
cd emacs-application-framework
chmod +x ./install-eaf.py
python ./install-eaf.py
```

##### If you use Windows,

```Bash
cd emacs-application-framework
node ./install-eaf-win32.js
```

##### If you use macOS,

```Bash
cd emacs-application-framework
chmod +x ./install-eaf-mac.sh
./install-eaf-mac.sh
```

There are many useful flags available for `install-eaf.py`, check it yourself using `--help`.

#### 3. Install Elisp packages:
- [s.el](https://github.com/magnars/s.el)

#### 4. Load EAF:

From here on, you can either add the full path to the EAF installation directory to your Emacs ```load-path```, then add the following to `init.el`:

```Elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)
```

- Alternatively, if you use [use-package](https://github.com/jwiegley/use-package), you can use the following *sample* configuration for your convenience.

    ```Elisp
    (use-package eaf
      :load-path "~/.emacs.d/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
      :init
      (use-package epc :defer t :ensure t)
      (use-package ctable :defer t :ensure t)
      (use-package deferred :defer t :ensure t)
      (use-package s :defer t :ensure t)
      :custom
      (eaf-browser-continue-where-left-off t)
      :config
      (setq eaf-browser-enable-adblocker t")
      (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
      (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
      (eaf-bind-key take_photo "p" eaf-camera-keybinding)
      (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki
    ```

### Dependency List

Packages listed as **Core** are mandatory for EAF to work, whereas other packages are optional - install if you want to use corresponding EAF Application.

| Package                        | Description                                           |
| :--------                      | :------                                               |
| python-epc                     | EPC for Python                                        |
| python-pyqt5, python-pyqt5-sip | Essential GUI library                                 |
| python-pyqtwebengine           | Chromium based web rendering engine                   |
| wmctrl                         | Activate Emacs window input focus                     |
| pygetwindow                    | Activate Emacs window input focus on WSL from Windows |
| mac-app-frontmost              | Monitor frontmost app on macOS                        |
| nodejs                         | Installs dependencies, and for app communications     |

You can check `app/name/package.json` and `app/name/dependencies.json` for EAF application dependencies.

## Launch EAF Applications
| Application Name    | Launch                                                                 |
| :--------           | :----                                                                  |
| Browser             | `M-x eaf-open-browser` Search or Goto URL                              |
|                     | `M-x eaf-open-browser-with-history` Search or Goto URL or Goto History |
| HTML Email Renderer | `M-x eaf-open-mail-as-html` in `gnus`, `mu4e`, `notmuch` HTMl Mail     |
| PDF Viewer          | `M-x eaf-open` PDF File                                                |
| Video Player        | `M-x eaf-open` Video File                                              |
| Image Viewer        | `M-x eaf-open` Image File                                              |
| Markdown Previewer  | `M-x eaf-open` Markdown File                                           |
| Org Previewer       | `M-x eaf-open` Org File                                                |
| Camera              | `M-x eaf-open-camera`                                                  |
| Terminal            | `M-x eaf-open-terminal`                                                |
| File Manager   | `M-x eaf-open-file-manager`                                    |
| File Sender         | `M-x eaf-file-sender-qrcode` or `eaf-file-sender-qrcode-in-dired`      |
| File Browser        | `M-x eaf-file-browser-qrcode`                                          |
| Airshare            | `M-x eaf-open-airshare`                                                |
| Mindmap             | `M-x eaf-create-mindmap` or `M-x eaf-open-mindmap`                     |
| MS Office Viewer    | `M-x eaf-open-office`                                                  |
| Jupyter             | `M-x eaf-open-jupyter`                                                 |
| Music Player        | `M-x eaf-open-music-player`                                            |
| System Monitor      | `M-x eaf-open-system-monitor`                                          |
| Demo                | `M-x eaf-open-demo` to verify basic functionality                      |
| Vue Demo            | `M-x eaf-open-vue-demo` to verify vue.js functionality                 |

- EAF Browser and PDF Viewer support Emacs built-in bookmark operation, with `M-x bookmark-set` (defaulted to `C-x r m`) and `M-x bookmark-bmenu-list` (defaulted to `C-x r l`).

## Wiki

It is **highly** suggested to read the [Wiki](https://github.com/emacs-eaf/emacs-application-framework/wiki) first before using EAF.

Wiki consists of documentations on:
* Keybindings
* Customization
* Design
* TODOLIST

There also are some helpful tips to make EAF work with Docker, Helm, etc.

## FAQ

### How does EAF work?
EAF implements three major functionalities:
1. Integrate PyQt program window into Emacs frame using QWindow Reparent technology.
2. Listen to EAF buffer's keyboard event flow and control the keyboard input of PyQt program via Python EPC.
3. Create a window compositer to make a PyQt program window adapt Emacs' Window/Buffer design.

Learn more from the [Wiki](https://github.com/emacs-eaf/emacs-application-framework/wiki/Hacking)!

### EAF vs EXWM?
1. EAF and EXWM share a common goal: enhance collaboration between the standard GNU Emacs with other GUI tools.
2. EXWM is an X11 Window Manager, it manages and controls other software using a keyboard, but it cannot modify, customize and extend the behavior of other software.
3. EAF is *not* a Window Manager, it utilizes the windows managing capabilities of Emacs to display its applications.
4. The intention of EAF is to provide a framework to write PyQt5 applications that extends the multimedia experience of Emacs. From the perspective of Emacs' buffer/mode design, EAF is not different from any other package, with the former uses Qt for drawing contents while the latter uses Emacs' built-in text libraries instead.
5. Through EAF's design, one can use Elisp to control Python and vice versa, and even able to use Elisp to control JavaScript in EAF Browser. EAF enables Emacs to the world of **multi-language scripting**.

Both projects are similar in terms of interface, but they are two completely different projects with different goals in mind. Sometimes one may find EAF is more suitable than EXWM, sometimes it's the other way around. Please do not meaninglessly compare them.

### macOS
Currently, macOS is only partially supported and needs a lot of testing, not all functions can work normally, please check the [Wiki](https://github.com/emacs-eaf/emacs-application-framework/wiki/macOS) for details.

### Browser crashes
Please DO NOT install PyQt5 through pip, pip version has bug, please install PyQt5 from the OS software repository.

### EAF js-video-player can't play some video on Windows and Mac. Why?
`js-video-player` requires that qtwebengine built against ffmpeg to support proprietary codecs like `h264/aac`.

### Why doesn't EAF receive input events on Linux Window Manager?
EAF confirms that the desktop environment or window manager you can work includes: KDE, Gnome2, Gnome3, Mate, Xfce, LXDE, Sway, i3, QTile, Xpra, EXWM.

We suspect there are some issues with how all the Window Managers implement their x11 protocols.

One workaround is to name of command `wmctrl -m` to the elisp list `eaf-wm-focus-fix-wms`. Fill an issue if it still doesn't work.

### Proxy
If you need to use proxy to access internet, one can configure the proxy settings.

```Elisp
(setq eaf-proxy-type "http")
(setq eaf-proxy-host "127.0.0.1")
(setq eaf-proxy-port "1080")
```

If you use Socks5 as local proxy, one can set proxy type with:

```Elisp
(setq eaf-proxy-type "socks5")
```

## EAF in the community

A list of other community packages that use EAF to enhance their graphical experiences!

If we missed your package, please make a PR to add it to the list.

* ***[obr-viz](https://github.com/swhalemwo/obr-viz)***: visualizing [org-brain](https://github.com/Kungsgeten/org-brain) relationships using EAF

## Report bug

### For any installation and configuration assistance, please read the [Wiki](https://github.com/emacs-eaf/emacs-application-framework/wiki) first!

If you encounter a problem with EAF, and it occured after pulling the latest commit, please check the [Discussions](https://github.com/emacs-eaf/emacs-application-framework/discussions/527) page first.

For any other problems, please use `emacs -q` and load a minimal setup with only EAF to verify that the bug is reproducible. If `emacs -q` works fine, probably something is wrong with your Emacs config.

If the problem persists, please report it [here](https://github.com/emacs-eaf/emacs-application-framework/issues/new) with `*eaf*` buffer content, it contains many clues that can help us locate the problem faster.

If you got segfault error, please use the following way to collect crash information:
1. Install gdb and turn on option `eaf-enable-debug`
2. Use command `eaf-stop-process` stop current process
3. Restart eaf, send issue with `*eaf*` buffer content when next crash

## Join Us
Do you want to make Emacs a real "operating system"?

Do you want to live in Emacs more comfortably?

Do you want to revolutionize the capabilities of Emacs?

[Let's hack together!](https://github.com/emacs-eaf/emacs-application-framework/wiki/Hacking)

## 打赏
如果我的作品让你的生活充满快乐, 欢迎请我喝瓶啤酒, 哈哈哈哈

### ManateeLazyCat
<p float="left">
    <img src="./img/alipay.jpg" width="188">
    <img src="./img/wechat.jpg" width="200">
</p>
