English | [简体中文](./README.zh-CN.md)

<p align="center">
  <img style='height: auto; width: 80%; object-fit: contain' src="./img/EAF_Banner_Transparent.png">
  <br>A free/libre and open-source extensible framework that revolutionizes the graphical capabilities of Emacs. <br>The key to ultimately <i>Live in Emacs</i>
</p>

## Vision
Emacs, the extensible *text editor*, is more than 45 years old. It is older than virtually all operating systems people use today, almost as old as the first UNIX system. During the decades of development, the world's brightest hackers have contributed their intelligence and creativity. Together they've constructed the most comprehensive and richest ecosystem that you can find in any software to date.

If you are a hacker who works with numerous languages and text, who's keyboard-driven and desires maximum freedom, extensibility, and introspectivity over your tool, maybe to the extent of *living* in it, Emacs will be your best bet.

Unfortunately, this 45 years of age is also one of the greatest disadvantages of Emacs. Comparing with modern software, Emacs lacks performance. Specifically Emacs Lisp lacks performance. It doesn't have proper multithreading and its graphical capabilities are seriously limited. It is far from what you'd expect from any GUI application today (such as IDEA or VSCode). You may think that Emacs, as a text-centric editor, doesn't need them, but have you ever run into a situation that, you sit comfortably typing commands and doing your Emacs sorcery, but can't help but ponder:

    What if Emacs has a real browser?
    What if this PDF or video file can be viewed efficiently without leaving Emacs?

Emacs, although infinitely extensible in text, is very limited in graphics. It shouldn't have to be this way. However, Emacs Lisp is *the* integral part of the Emacs culture, it carries decades of history with itself, it is what makes Emacs special. It is irreplaceable.

The vision of the Emacs Application Framework (EAF) project is, while fully retaining the rich history, culture, and ecosystem of Emacs and Emacs Lisp, to open up completely new doors to the ecosystems of Python, Qt6, and even JavaScript. EAF extends Emacs to the world of modern graphics, but still preserving the extensibility and customizability of Emacs. It will be the key to ultimately *Live in Emacs*.


## Features

EAF is very extensible. We ship a lot of applications, feel free to choose anything you find interesting to install:

| Browser                                          | PDF Viewer |
| :--------:                                       | :----:                                                      |
| <img src="./img/browser.png" width="400"> | <img src="./img/pdf-viewer.png" width="400"> |

| Music Player                            | File Manager                              |
| :--------:                                            | :----:                                                |
| <img src="./img/music-player.png" width="400"> | <img src="./img/file-manager.png" width="400"> |
|                                                       |                                                       |

- [Browser](https://github.com/emacs-eaf/eaf-browser): A modern, customizable and extensible browser in Emacs
- [PDF Viewer](https://github.com/emacs-eaf/eaf-pdf-viewer): Fastest PDF Viewer in Emacs
- [Music Player](https://github.com/emacs-eaf/eaf-music-player): Music player that supports playlist and audio visualization
- [Video Player](https://github.com/emacs-eaf/eaf-video-player): Video Player in Emacs
- [Image Viewer](https://github.com/emacs-eaf/eaf-image-viewer): Dynanmic image viewer
- [RSS Reader](https://github.com/emacs-eaf/eaf-rss-reader): RSS Reader in Emacs
- [Terminal](https://github.com/emacs-eaf/eaf-terminal): Full-featured terminal in Emacs
- [Camera](https://github.com/emacs-eaf/eaf-camera): Use camera in Emacs
- [Markdown Previewer](https://github.com/emacs-eaf/eaf-markdown-previewer): Real-time Markdown previewer
- [Org Previewer](https://github.com/emacs-eaf/eaf-org-previewer): Real-time Org-mode previewer
- [Git Client](https://github.com/emacs-eaf/eaf-git): Fully multi-threaded git client for Emacs
- [File Manager](https://github.com/emacs-eaf/eaf-file-manager): Fully multi-threaded replacement for dired-mode

... plus [many more](https://github.com/orgs/emacs-eaf/repositories)!

### EAF in EmacsConf
| EmacsConf 2020: <a href="https://emacsconf.org/2020/talks/34/">Extend Emacs with EAF</a>                            | EmacsConf 2021: <a href="https://emacsconf.org/2021/talks/eaf/">EAF: A 2021 Update</a>                              |
| :--------:                                            | :----:                                                |
| [<img src="https://img.youtube.com/vi/HK_f8KTuR0s/0.jpg" width=400>](https://www.youtube.com/watch?v=HK_f8KTuR0s) | [<img src="https://img.youtube.com/vi/bh37zbefZk4/0.jpg" width=400>](https://www.youtube.com/watch?v=bh37zbefZk4) |
|                                                       |                                                       |


## Install

EAF supports Linux (X11 and Wayland), Windows, macOS and FreeBSD. The installation method is very simple.

#### 1. Download EAF

```Bash
git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git ~/.emacs.d/site-lisp/emacs-application-framework/
```

#### 2. Install/Update EAF applications and dependencies

You can use `M-x eaf-install-and-update` or manually run the `install-eaf.py` script in the EAF directory:

```Bash
cd emacs-application-framework
chmod +x ./install-eaf.py
./install-eaf.py
```

There are many useful flags available for `install-eaf.py`, check it yourself using `--help`.

There are three types of support for the Wayland environment:
- XWayland: EAF can run directly under XWayland
- Gnome3 Wayland Native: You need to execute the command `cp -r emacs-application-framework/gnome-shell/eaf-wayland@emacs-eaf.org ~/.local/share/gnome-shell/extensions` and activate the `eaf-wayland@emacs-eaf.org` plugin in `gnome-extensions`
- Sway Wayland Native: [jshon](http://kmkeen.com/jshon/) needs to be installed
- Hyprland Wayland Native: [jshon](http://kmkeen.com/jshon/) needs to be installed

#### 3. Load EAF Core

From here on, you can add the full path to the EAF installation directory to your Emacs ```load-path```, then add the following to `init.el`:

```Elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)
```

- Alternatively, if you use [use-package](https://github.com/jwiegley/use-package), you can use the following *sample* configuration for your convenience.

    ```Elisp
    (use-package eaf
      :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
      :custom
      ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
      (eaf-browser-continue-where-left-off t)
      (eaf-browser-enable-adblocker t)
      (browse-url-browser-function 'eaf-open-browser)
      :config
      (defalias 'browse-web #'eaf-open-browser)
      (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
      (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
      (eaf-bind-key take_photo "p" eaf-camera-keybinding)
      (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki
    ```

#### 4. Load EAF Apps

You can use below code to load applications `browser` and `pdf-viewer` that you installed. Please check [Applications](https://github.com/emacs-eaf/emacs-application-framework#features) for the full list:

```Elisp
(require 'eaf-browser)
(require 'eaf-pdf-viewer)
```

#### 5. Hooray!

Congratulations, you just installed EAF! You can try `M-x eaf-open-demo` (that is if you have `demo` installed, of course) to see if everything works properly, and enjoy the new possibilities of Emacs.

Below are launch commands of EAF Applications:

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
| File Manager   | `M-x eaf-open-in-file-manager`                                    |
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

## Upgrade
Also, you should regularly `git pull` **and** run `install-eaf.py` (`M-x eaf-install-and-update`) to update EAF, its applications, and relating dependencies.

## Report bug

### For any installation and configuration assistance, please read the [Wiki](https://github.com/emacs-eaf/emacs-application-framework/wiki) and [FAQ](https://github.com/emacs-eaf/emacs-application-framework/wiki/FAQ).

If you encounter a problem with EAF, and it occurred after pulling the latest commit, please check the [Mandatory Procedures to Keep Your EAF Up-To-Date](https://github.com/emacs-eaf/emacs-application-framework/discussions/527?sort=new) page **first**.

For any other problems, please use `emacs -q` and load a minimal setup with only EAF to verify that the bug is reproducible. If `emacs -q` works fine, probably something is wrong with your Emacs config.

If the problem persists, please report it [here](https://github.com/emacs-eaf/emacs-application-framework/issues/new) with the `*eaf*` buffer content. It contains many clues that can help us locate the problem faster.

If you get a segfault error, please use the following way to collect crash information:
1. Install gdb and turn on option `(setq eaf-enable-debug t)`
2. Use the command `eaf-stop-process` to stop the current process
3. Restart eaf, send issue with `*eaf*` buffer content when next crash

## EAF in the community

A list of other community packages that use EAF to enhance their graphical experiences!

If we missed your package, please make a PR to add it to the list.

* ***[obr-viz](https://github.com/swhalemwo/obr-viz)***: visualizing [org-brain](https://github.com/Kungsgeten/org-brain) relationships using EAF
* ***[netease-cloud-music](https://github.com/SpringHan/netease-cloud-music.el)***: A netease music client for emacs.
* ***[2048pyqt6](https://github.com/porrige/2048pyqt6)***: A 2048 game that can run in emacs.

## Contributor
<a href = "https://github.com/emacs-eaf/emacs-application-framework/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=emacs-eaf/emacs-application-framework"/>
</a>

## Join Us
Do you want to make Emacs a real "operating system"?

Do you want to live in Emacs more comfortably?

Do you want to revolutionize the capabilities of Emacs?

[Let's hack together!](https://github.com/emacs-eaf/emacs-application-framework/wiki/Hacking)
