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
| <img src="./screenshot/file_transfer.png" width="400"> | <img src="./screenshot/file_uploader.png" width="400"> |
|                                                        |                                                        |


| Air Share                                          | Org Previewer                                          |
| :--------:                                         | :--------:                                             |
| <img src="./screenshot/air_share.png" width="400"> | <img src="./screenshot/org_previewer.gif" width="400"> |
|                                                    |                                                        |

| Terminal Emulator                                 | RSS Reader                                          |
| :--------:                                        | :------:                                            |
| <img src="./screenshot/terminal.gif" width="400"> | <img src="./screenshot/rss_reader.gif" width="400"> |
|                                                   |                                                     |

| Aria2 Download Manager                         |
| :--------:                                     |
| <img src="./screenshot/aria2.gif" width="400"> |
|                                                |



## Install
1. For Arch Linux users, one can install [emacs-eaf](https://aur.archlinux.org/packages/emacs-eaf/) in AUR and jump to step 4.

2. Make sure to have ```python3``` installed, and use ```pip3``` to install all EAF dependencies (see below list for details)

```Bash
sudo pip3 install dbus-python python-xlib pyqt5 pyqtwebengine pymupdf grip qrcode feedparser aria2p
```

3. Clone this repository.

```Bash
git clone https://github.com/manateelazycat/emacs-application-framework.git --depth=1
```

4. Add the full path to the EAF installation directory to your Emacs ```load-path```, then add the following to `init.el`:

```Elisp
(require 'eaf)
```

If you use [use-package](https://github.com/jwiegley/use-package), a sample configuration has been provided.

```Elisp
(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :custom
  (eaf-find-alternate-file-in-dired t)
  :config
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding))
```

5. For EAF Terminal to work *only*: Install and configure ```wetty```:
```Bash
# Install wetty
sudo yarn global add wetty

# Make wetty login with public key
ssh-keygen
cp ~/.ssh/id_rsa.pub ~/.ssh/authorized_keys

# You need add below in .bashrc if you are Chinese
echo 'export LANG=zh_CN.UTF-8' >> ~/.bashrc
```

6. For EAF Browser download to work, please install ```aria2```.

### Dependency List
Packages listed as **Core** are mandatory for EAF to work, whereas other packages are optional - install if you want to use corresponding EAF Application.

| Package       | Package Repo  | Dependent                                                                          | Description                               |
| :--------     | :----         | :------                                                                            | :------                                   |
| pyqt5         | pip3          | Core                                                                               | Essential GUI library                     |
| dbus-python   | pip3          | Core                                                                               | DBus IPC to connect Python with Elisp     |
| python-xlib   | pip3          | Core                                                                               | Stick application window into Emacs frame |
| pyqtwebengine | pip3          | Browser, Image Viewer, RSS Reader, <br>Terminal, Org Previewer, Markdown Previewer | Chromium based web rendering engine       |
| pymupdf       | pip3          | PDF Viewer                                                                         | PDF rendering engine                      |
| grip          | pip3          | Markdown Previewer                                                                 | Markdown rendering server                 |
| qrcode        | pip3          | File Sender, File Receiver, Airshare                                               | Render QR code pointing to local files    |
| feedparser    | pip3          | RSS Reader                                                                         | Parse RSS feeds                           |
| aria2p        | pip3          | Browser                                                                            | Send download requests to Aria2 daemon    |
| aria2         | pacman (Arch) | Browser                                                                            | Download files from the web               |
| wetty         | yarn          | Terminal                                                                           | Communicate between browser and local TTY |

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
| File Receiver       | `M-x eaf-file-receiver-qrcode`                                         |
| Airshare            | `M-x eaf-open-airshare`                                                |
| RSS Reader          | `M-x eaf-open-rss-reader`                                              |
| Demo                | `M-x eaf-open-demo` to verify basic functionality                      |

- To open the file under the cursor in `dired` using appropriate EAF Application, use `eaf-open-this-from-dired` instead.

```
NOTE:
EAF use DBus' session bus, it must run by a general user.
Please don't use EAF when Emacs is started with sudo or root user, a root user can only access DBus's system bus.
```

## Wiki
It is **highly** suggested to read the [Wiki](https://github.com/manateelazycat/emacs-application-framework/wiki) first before using EAF.

Wiki consists of documentations on Keybinding, Customization, Design and TODOLIST. There also are some helpful tips to make EAF work with Docker, Helm, etc.

## FAQ

### How does EAF work?
EAF implements three major functionalities:
1. Integrate PyQt program window into Emacs frame using Xlib Reparent technology.
2. Listen to EAF buffer's keyboard event flow and control the keyboard input of PyQt program via DBus IPC.
3. Create a window compositer to make a PyQt program window adapt Emacs's Window/Buffer design.

### EAF vs EXWM?
1. EAF and EXWM share a common goal: enhance collaboration between the standard GNU Emacs with other GUI tools.
2. EXWM is an X11 Windows Manager, it manages and controls other software using a keyboard, but it cannot modify, customize and extend the behavior of other software. For example, it cannot modify the behavior when you press a key in Chrome or a PDF viewer.
3. EAF is *not* a Windows Manager, it utilizes the windows managing capabilities of Emacs to display its applications.
4. The intention of EAF is to provide a framework to write PyQt5 applications that extends the multimedia experience of Emacs. From the perspective of Emacs' buffer/mode design, EAF is not different from any other package, with the former uses Qt for drawing contents while the latter uses Emacs' built-in text libraries instead.
5. Through EAF's design, one can use Elisp to control Python and vice versa, and even able to use Elisp to control JavaScript in EAF Browser. EAF enables Emacs to the world of **multi-language scripting**.

Both projects are similar in terms of interface, but they are two completely different projects with different goals in mind. Sometimes one may find EAF is more suitable than EXWM, sometimes it's the other way around. Please do not meaninglessly compare them.

### EAF is (currently) Linux only. Why?
1. As of now, none of EAF's core developers use MacOS or Windows or BSD family OS.
2. EAF uses X11 Reparent to stick Qt5 window to Emacs frame. Other operating systems may implement similar features, but the core developers don't know how to work with them.
3. DBus is Linux-specific technology, it's difficult to support DBus in other operating systems.
4. Qt5's QGraphicsScene technology does not work on MacOS.

If you've figure them out, PRs are always welcome!

### How about Wayland?
EAF use X11 XReparent technology, Wayland doesn't support it (right now).

We recommend to use KDE or Xfce, they supports X11 XReparent and handling of keyboard focus events correctly. Other Lighter DEs or Tiling WMs do not support EAF very well, such as ```i3wm``` or ```awesome```.

### `[EAF] *eaf* aborted (core dumped)` error
Please check the `*eaf*` buffer, something is wrong on the Python side. Usually due to Python dependencies are not installed correctly.

If you're sure Python dependences are installed correctly, please create an issue with the `*eaf*` buffer content, it contains many clues that can help us locate the problem faster.

### `undefined symbol` error
If you got "undefined symbol" error after start EAF, and you use Arch Linux, yes, it's a bug of Arch.

You need use pip install all dependences after you upgrade your Arch system, then undefine symbol error will fix.

```Bash
sudo pip3 install dbus-python python-xlib pyqt5 pyqtwebengine pymupdf grip qrcode feedparser aria2p --force-reinstall
```

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

For any installation and configuration assistance, please read the [Wiki](https://github.com/manateelazycat/emacs-application-framework/wiki) first!

If you encounter any problem with EAF, please use command `emacs -q` with a minimal setup that only contains EAF and verify the bug is reproducible. If `emacs -q` works fine, probably something is wrong with your Emacs config.

If the problem persists, please [report bug here](https://github.com/manateelazycat/emacs-application-framework/issues/new).

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
