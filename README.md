English | [简体中文](./README.zh-CN.md)

# Emacs Application Framework (EAF)
EAF is a GUI application framework that revolutionizes Emacs graphical capabilities to ultimately *Live in Emacs*.

## EAF Application Overview
EAF is an extensible framework, one can develop any Qt5 application and integrate it into Emacs.

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
| <img src="./screenshot/file_transfer.png" width="400"> | <img src="./screenshot/file_browser.png" width="400"> |
|                                                        |                                                        |


| Air Share                                          | Org Previewer                                          |
| :--------:                                         | :--------:                                             |
| <img src="./screenshot/air_share.png" width="400"> | <img src="./screenshot/org_previewer.gif" width="400"> |
|                                                    |                                                        |

| Terminal Emulator                                 | RSS Reader (Integrated with Elfeed)                                           |
| :--------:                                        | :------:                                            |
| <img src="./screenshot/terminal.gif" width="400"> | <img src="./screenshot/rss_reader.gif" width="400"> |
|                                                   |                                                     |

| Aria2 Download Manager                         | Mind Map  |
| :--------:                                     | :-------: |
| <img src="./screenshot/aria2.gif" width="400"> | <img src="./screenshot/mindmap.gif" width="400"> |
|                                                |           |


| Mermaid                                          | EAF Interleave                                          |
| :--------:                                       | :----------:                                            |
| <img src="./screenshot/mermaid.gif" width="400"> | <img src="./screenshot/eaf-interleave.gif" width="400"> |
|                                                  |                                                         |

| Jupyter                                          |                                                         |
| :--------:                                       | :----------:                                            |
| <img src="./screenshot/jupyter.png" width="400"> |                                                         |
|                                                  |                                                         |

## EmacsConf2020 - Extend Emacs to Modern GUI Applications with EAF
- EAF talk & demo at EmacsConf2020, presented to you by [MatthewZMD](https://github.com/MatthewZMD)
- Homepage (Q&A included): https://emacsconf.org/2020/talks/34/
- Youtube link:

[![EmacsConf2020 - Extend Emacs to Modern GUI Applications with EAF](https://img.youtube.com/vi/HK_f8KTuR0s/0.jpg)](https://www.youtube.com/watch?v=HK_f8KTuR0s)

## Install
1. Download EAF
```Bash
git clone --depth=1 -b master https://github.com/manateelazycat/emacs-application-framework.git ~/.emacs.d/site-lisp/emacs-application-framework/
```

Alternatively, you can use a [Quelpa recipe](https://github.com/quelpa/quelpa)
```Emacs-lisp
(quelpa '(eaf (:fetcher github
               :repo  "manateelazycat/emacs-application-framework"
               :files ("*"))))
```

2. Install EAF dependencies using `M-x install-eaf-dependencies`.

If you prefer to manually call the installation script in the terminal,

- If you use GNU/Linux,

```Bash
cd emacs-application-framework
chmod +x ./install-eaf.sh
./install-eaf.sh
```

- If you use Windows,

```shell
cd emacs-application-framework
node ./install-eaf-win32.js
```

Feel free to inspect the install script yourself. An explanation of each dependency can be found at [Dependency List](#dependency-list).

3. Install Elisp packages:
- [emacs-ctable](https://github.com/kiwanami/emacs-ctable)
- [emacs-ctable](https://github.com/kiwanami/emacs-ctable)
- [emacs-deferred](https://github.com/kiwanami/emacs-deferred)
- [emacs-epc](https://github.com/kiwanami/emacs-epc)
- [s.el](https://github.com/magnars/s.el)

4. From here on, you can either add the full path to the EAF installation directory to your Emacs ```load-path```, then add the following to `init.el`:

```Elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)
```
or, if you use [use-package](https://github.com/jwiegley/use-package), you can use the following *sample* configuration for your convenience.

```Elisp
(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :init
  (use-package epc :defer t)
  (use-package ctable :defer t)
  (use-package deferred :defer t)
  (use-package s :defer t :ensure t)
  :custom
  (eaf-browser-continue-where-left-off t)
  :config
  (eaf-setq eaf-browser-enable-adblocker "true")
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki
```

### Dependency List

Packages listed as **Core** are mandatory for EAF to work, whereas other packages are optional - install if you want to use corresponding EAF Application.

| Package                        | Dependent                            | Description                                   |
| :--------                      | :------                              | :------                                       |
| python-pyqt5, python-pyqt5-sip | Core                                 | Essential GUI library                         |
| python-pyqtwebengine           | Core                                 | Chromium based web rendering engine           |
| wmctrl                         | Core                                 | Activate Emacs window input focus             |
| python-pymupdf                 | PDF Viewer                           | PDF rendering engine                          |
| python-qrcode                  | File Sender, File Receiver, Airshare | Render QR code pointing to local files        |
| python-markdown                | Mermaid                              | Covert markdown format to mermaid html format |
| nodejs                         | Terminal                             | Communicate between browser and local TTY     |
| aria2                          | Browser                              | Download files from the web                   |
| libreoffice                    | Doc Viewer                           | Convert doc file to pdf                       |
| filebrowser-bin                | File Browser                         | Share files between computer and smartphone   |
| qtconsole                      | Jupyter                              | Provide RichJupyterWidget                     |

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
| File Sender         | `M-x eaf-file-sender-qrcode` or `eaf-file-sender-qrcode-in-dired`      |
| File Browser        | `M-x eaf-file-browser-qrcode`                                          |
| Airshare            | `M-x eaf-open-airshare`                                                |
| Mindmap             | `M-x eaf-create-mindmap` or `M-x eaf-open-mindmap`                     |
| MS Office Viewer    | `M-x eaf-open-office`                                                  |
| Mermaid             | `M-x eaf-open` Mermaid file (*.mmd)                                    |
| Jupyter             | `M-x eaf-open-jupyter`                                                 |
| Demo                | `M-x eaf-open-demo` to verify basic functionality                      |

- EAF Browser and PDF Viewer support Emacs built-in bookmark operation, with `M-x bookmark-set` (defaulted to `C-x r m`) and `M-x bookmark-bmenu-list` (defaulted to `C-x r l`).

## Wiki

It is **highly** suggested to read the [Wiki](https://github.com/manateelazycat/emacs-application-framework/wiki) first before using EAF.

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
3. Create a window compositer to make a PyQt program window adapt Emacs's Window/Buffer design.

Learn more from the [Wiki](https://github.com/manateelazycat/emacs-application-framework/wiki/Hacking)!

### EAF vs EXWM?
1. EAF and EXWM share a common goal: enhance collaboration between the standard GNU Emacs with other GUI tools.
2. EXWM is an X11 Window Manager, it manages and controls other software using a keyboard, but it cannot modify, customize and extend the behavior of other software.
3. EAF is *not* a Window Manager, it utilizes the windows managing capabilities of Emacs to display its applications.
4. The intention of EAF is to provide a framework to write PyQt5 applications that extends the multimedia experience of Emacs. From the perspective of Emacs' buffer/mode design, EAF is not different from any other package, with the former uses Qt for drawing contents while the latter uses Emacs' built-in text libraries instead.
5. Through EAF's design, one can use Elisp to control Python and vice versa, and even able to use Elisp to control JavaScript in EAF Browser. EAF enables Emacs to the world of **multi-language scripting**.

Both projects are similar in terms of interface, but they are two completely different projects with different goals in mind. Sometimes one may find EAF is more suitable than EXWM, sometimes it's the other way around. Please do not meaninglessly compare them.

### EAF can't works on MacOS. Why?
1. Qt5's QGraphicsScene technology does not work on MacOS.
2. QWindow Reparent technology need use the original API of Mac platform for replacement.

If you've figure them out, PRs are always welcome!

### EAF js-video-player can't play some video on Windows. Why?
`js-video-player` requires that qtwebengine built against ffmpeg to support proprietary codecs like `h264/aac`.

### Why doesn't EAF receive input events on WM?
EAF confirms that the desktop environment or window manager you can work includes: KDE, GNOME2, GNOME3, Mate, Xfce, LXDE, I3, QTILE, Xpra.

We suspect there are some issues with how all the Window Managers implement their x11 protocols.

One workaround is to name of command `wmctrl -m` to the elisp list `eaf-wm-focus-fix-wms`. Fill an issue if it still doesn't work.

### What is Github Personal Access Tokens?
If you use EAF Markdown Previewer, to get consistent previewing, you need to access [Github Personal access token site](https://github.com/settings/tokens/new?scopes=), fill something in "Token description" and click button "Generate token" to get your personal token. Then set the token:

```Elisp
(setq eaf-grip-token "yourtokencode")
```

Although Markdown Previewer works for the first few times by entering empty string when prompted, eventually it stops working and gives "GitHub Rate Limit Reached" error.

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

### For any installation and configuration assistance, please read the [Wiki](https://github.com/manateelazycat/emacs-application-framework/wiki) first!

If you encounter a problem with EAF, and it occured after pulling the latest commit, please check the [Discussions](https://github.com/manateelazycat/emacs-application-framework/discussions/527) page first.

For any other problems, please use `emacs -q` and load a minimal setup with only EAF to verify that the bug is reproducible. If `emacs -q` works fine, probably something is wrong with your Emacs config.

If the problem persists, please report it [here](https://github.com/manateelazycat/emacs-application-framework/issues/new) with `*eaf*` buffer content, it contains many clues that can help us locate the problem faster.

If you got segfault error, please use the following way to collect crash information:
1. Install gdb and turn on option `eaf-enable-debug`
2. Use command `eaf-stop-process` stop current process
3. Restart eaf, send issue with `*eaf*` buffer content when next crash

## Join Us
Do you want to make Emacs a real "operating system"?

Do you want to live in Emacs more comfortably?

Do you want to revolutionize the capabilities of Emacs?

[Let's hack together!](https://github.com/manateelazycat/emacs-application-framework/wiki/Hacking)

## 打赏
如果我的作品让你的生活充满快乐, 欢迎请我喝瓶啤酒, 哈哈哈哈

### ManateeLazyCat
<p float="left">
    <img src="./screenshot/alipay.jpg" width="188">
    <img src="./screenshot/wechat.jpg" width="200">
</p>
