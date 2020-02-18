### You are reading Chinese version. International users please read [English Version](https://github.com/manateelazycat/emacs-application-framework/blob/master/README.md)

# 什么是 Emacs Application Framework (EAF)?
EAF 是一个全新的图形应用框架，扩展Emacs的多媒体能力，最终达到 Living in Emacs 的终极目标。

## EAF 应用展示
EAF是一个可编程扩展的框架，你可以开发自己的Qt5应用并集成在Emacs中。

| 浏览器                                           | Markdown预览程序                                    |
| :--------:                                       | :----:                                                      |
| <img src="./screenshot/browser.gif" width="400"> | <img src="./screenshot/markdown_previewer.gif" width="400"> |

| 图片浏览器                                            | 视频播放器                                            |
| :--------:                                            | :----:                                                |
| <img src="./screenshot/image_viewer.gif" width="400"> | <img src="./screenshot/video_player.gif" width="400"> |
|                                                       |                                                       |

| PDF阅读器                                       | 摄像头程序                                     |
| :--------:                                          | :----:                                          |
| <img src="./screenshot/pdf_viewer.gif" width="400"> | <img src="./screenshot/camera.gif" width="400"> |
|                                                     |                                                 |

| 二维码下载文件 (PC到手机)                              | 二维码上传文件 (手机到PC)                                 |
| :--------:                                             | :----:                                                 |
| <img src="./screenshot/file_transfer.png" width="400"> | <img src="./screenshot/file_uploader.png" width="400"> |
|                                                        |                                                        |


| 无线文字传输程序                                   | Org预览                                                |
| :--------:                                         | :--------:                                             |
| <img src="./screenshot/air_share.png" width="400"> | <img src="./screenshot/org_previewer.gif" width="400"> |
|                                                    |                                                        |

| 终端模拟器                                        | RSS阅读器                                           |
| :--------:                                        | :------:                                            |
| <img src="./screenshot/terminal.gif" width="400"> | <img src="./screenshot/rss_reader.gif" width="400"> |
|                                                   |                                                     |

## 安装
1. 如果是Arch用户，你可以直接安装[emacs-eaf](https://aur.archlinux.org/packages/emacs-eaf/)，并跳到第4步.

2. 先确认系统中已经安装了 ```python3```，然后使用 ```pip3``` 安装EAF依赖库:

```Bash
sudo pip3 install dbus-python python-xlib pyqt5 pyqtwebengine pymupdf grip qrcode feedparser aria2p
```

3. 使用 ```git clone``` 下载这个仓库.

```Bash
git clone https://github.com/manateelazycat/emacs-application-framework.git
```

4. 把EAF加入Emacs的 ```load-path```，然后在 `init.el` 中写入:

```Elisp
(require 'eaf)
```

如果你使用[use-package](https://github.com/jwiegley/use-package)，下面有一个简单的配置文件供你参考:

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

5. 如果你*需要*使用EAF终端模拟器，请按照下面的方法安装配置 ```wetty```:
```Bash
# 安装wetty
sudo yarn global add wetty

# 确保wetty自动登录
ssh-keygen
cp ~/.ssh/id_rsa.pub ~/.ssh/authorized_keys

# 确保终端正常显示中文文件名
echo 'export LANG=zh_CN.UTF-8' >> ~/.bashrc
```

6. EAF浏览器的下载功能依赖aria2，还需要你额外安装 ```aria2``` 这个工具。

### 依赖列表
**核心** 分类表示必备依赖，这些包必须安装好EAF才能工作，**应用** 分类表示可选依赖，只有当你需要对应的应用时，你才需要安装这些依赖，当然我们推荐你把所有依赖都安装了，到你使用的时候就不用再次折腾。

| 包名          | 安装方式      | 依赖                                                                 | 解释                                               |
| :--------     | :----         | :------                                                              | :------                                            |
| pyqt5         | pip3          | 核心                                                                 | GUI图形库                                          |
| dbus-python   | pip3          | 核心                                                                 | DBus库，用于在Emacs和Python进程间通讯              |
| python-xlib   | pip3          | 核心                                                                 | X11的Python绑定，用于粘贴Qt5程序到Emacs Buffer区域 |
| pyqtwebengine | pip3          | 浏览器，图片浏览器，RSS阅读器，<br>终端模拟器，Org预览，Markdown预览 | 基于Chromium的浏览器引擎                           |
| pymupdf       | pip3          | PDF阅读器                                                            | 解析PDF文件                                        |
| grip          | pip3          | Markdown预览                                                         | 建立Markdown文件的HTML服务                         |
| qrcode        | pip3          | 文件上传，文件下载，文字传输                                         | 根据文件信息生成二维码                             |
| feedparser    | pip3          | RSS阅读器                                                            | 解析RSS/Atom信息                                   |
| aria2p        | pip3          | 浏览器                                                               | 发送下载请求给Aria2 Daemon                         |
| aria2         | pacman (Arch) | 浏览器                                                               | 下载网络文件                                       |
| wetty         | yarn          | 终端模拟器                                                           | 通过浏览器与本地TTY交互                            |

## EAF应用启动命令
| 应用名称       | 启动命令                                                                    |
| :--------      | :----                                                                       |
| 浏览器         | `M-x eaf-open-browser` 在浏览器中打开或搜索                                 |
|                | `M-x eaf-open-browser-with-history` 搜索历史或者打开URL                     |
| HTML邮件渲染   | `M-x eaf-open-mail-as-html` 在 `gnus`，`mu4e`，`notmuch` 等邮件客户端中执行 |
| PDF阅读器      | `M-x eaf-open` 输入PDF文件                                                  |
| 视频播放器     | `M-x eaf-open` 输入视频文件                                                 |
| 图片浏览器     | `M-x eaf-open` 输入图片文件                                                 |
| Markdown预览   | `M-x eaf-open` 输入Markdown文件                                             |
| Org预览        | `M-x eaf-open` 输入Org文件                                                  |
| 摄像头程序     | `M-x eaf-open-camera`                                                       |
| 终端模拟器     | `M-x eaf-open-terminal`                                                     |
| 二维码下载文件 | `M-x eaf-file-sender-qrcode` or `eaf-file-sender-qrcode-in-dired`           |
| 二维码上传文件 | `M-x eaf-file-receiver-qrcode`                                              |
| 无线分享       | `M-x eaf-open-airshare` 输入要分享给手机的字符串                            |
| RSS新闻阅读器  | `M-x eaf-open-rss-reader`                                                   |
| 演示程序       | `M-x eaf-open-demo`                                                         |

- 在`dired`文件管理器中，建议绑定按键到命令 `eaf-open-this-from-dired` ，它会自动用合适的EAF应用来打开文件。

```
注意：
EAF使用DBus的普通权限总线 (session bus)，请不要用 sudo 来启动EAF，root用户只能访问系统权限总线 (system bus)
```

## Wiki
强烈建议使用EAF之前浏览一遍[Wiki](https://github.com/manateelazycat/emacs-application-framework/wiki)。

Wiki包括架构设计，按键绑定，自定义选项和任务列表等文档。你还会在Wiki发现很多有用的技巧，比如Docker，Helm等，

## 常用问题

### EAF是怎么工作的？
EAF主要实现这几个功能：
1. 利用X11的XReparent技术来实现PyQt应用进程的窗口粘贴到Emacs对应的Buffer区域
2. 通过DBus IPC来实现Emacs进程和Python进程的控制指令和跨进程消息通讯
3. 通过Qt5的QGraphicsScene来实现镜像窗口，以对应Emacs的Buffer/Window模型

### EAF vs EXWM?
1. EAF和EXWM的共同点都是：“提升Emacs和别的程序的协作效率“
2. EXWM是一个X11窗口管理器，通过X11协议来控制Emacs和其他程序，但是EXWM只是管理其他程序，但是它并不会修改别的程序。比如它没法修改Chrome，PDF阅读器等GUI程序内在的行为
3. EAF不是一个窗口管理器，EAF只是依赖Emacs自身的窗口管理功能显示自己
4. EAF的目标是通过PyQt创造新的应用来扩展Emacs的多媒体能力。从Emacs本身的Buffer/Mode设计上看，它和你平常用的 `xx-mode` 插件没有啥区别，只是它用Qt来绘制内容，而不是Emacs自身的文本库来绘制内容
5. EAF通过造轮子的方式，把大多数程序员常用的应用写出来以后，达到Live in Emacs的最终目标
6. 基于EAF的架构设计，我们可以通过Elisp来控制Python，JavaScript和其他命令行工具，实现多语言扩展Emacs的编程模型。在坚持Emacs黑客文化和Elisp社区兼容性的前提下，让Emacs的多媒体能力能够跟上时代的发展

或许EAF和EXWM看起来有点相似，但它们在设计和理念上是两个完全不同的项目。所以请大家多多学习X11和Qt的区别，理解技术的本质，避免无意义的比较和争论。

### 为什么EAF只能在Linux下工作？
1. 核心开发者主要使用Manjaro Linux，目前为止，核心开发者并不使用其他操作系统，比如MacOS，Windows，BSD
2. EAF跨进程窗口混合技术依赖Linux下的X11 XReparent技术，其他操作系统可能有类似的技术，但是核心开发者不熟悉其他操作系统的底层技术
3. DBus是Linux下专用的进程间通讯技术，其他操作系统可能无法支持DBus
4. Qt5的QGraphicsScene技术无法在MacOS下正常工作，也就无法实现Qt5应用的镜像窗口以支持Emacs的Buffer/Window模型

欢迎操作系统级别黑客移植EAF，目前为止，我知道的主要的迁移障碍就只有三个：XReparent，DBus，QGraphicsScene

### 为什么不支持Wayland?
EAF依赖X11的XReparent技术，Wayland并不支持跨进程窗口融合技术

我们推荐你使用KDE或者Xfce这两个桌面环境，他们对XReparent技术有很好的支持，其他的轻量级窗口管理器和平铺窗口管理器对XReparent技术的支持和键盘焦点处理不完备，没法很好的支持EAF，比如i3wm，awesome都无法使EAF正常工作。

### `[EAF] *eaf* aborted (core dumped)` 奔溃了怎么办？
请检查 `*eaf*` 这个窗口的内容。通常是EAF的Python依赖没有安装好，如果你确定依赖没有问题，请附带 `*eaf*` 窗口的内容给我们提交issue，那里面有很多线索可以帮助我们排查问题。

### `undefined symbol` 错误
如果你启动的时候发现 "undefined symbol" 错误，并且你使用的是Arch系统，那肯定是因为Arch自身的bug，Arch系统每次升级以后，重新使用pip3安装一次Python依赖包就可以解决这个问题:

```Bash
sudo pip3 install dbus-python python-xlib pyqt5 pyqtwebengine pymupdf grip qrcode feedparser aria2p --force-reinstall
```

### Github 个人访问标记干什么用的？
Markdown预览程序依赖grip，你需要访问[Github Personal access token](https://github.com/settings/tokens/new?scopes=)去获取你个人的标记，然后通过下面的命令设置标记后，grip才能正常的工作：

```Elisp
(setq eaf-grip-token "yourtokencode")
```

尽管不设置访问标记一开始也能成功使用，但Github过段时间会弹出 "GitHub Rate Limit Reached" 的错误。

### 代理
可以通过下面设置来通过代理访问互联网：

```Elisp
(setq eaf-proxy-type "http")
(setq eaf-proxy-host "127.0.0.1")
(setq eaf-proxy-port "1080")
```

如果你使用Socks5代理，你可以设置代理类型为：

```Elisp
(setq eaf-proxy-type "socks5")
```

## EAF社区

下面列表列展示了EAF在Emacs社区的应用。如果我们遗漏你的应用，欢迎提交PR来加到下面列表中。

* ***[obr-viz](https://github.com/swhalemwo/obr-viz)***: visualizing [org-brain](https://github.com/Kungsgeten/org-brain) relationships using EAF

## 反馈问题

### 反馈安装和配置问题之前，请一定先阅读[Wiki](https://github.com/manateelazycat/emacs-application-framework/wiki)!!!

如果你遇到任何问题，请先用命令 `emacs -q` 并只添加EAF配置，做一个对比测试，如果 `emacs -q` 的时候可以工作，请检查你个人的配置文件。

如果```emacs -q```环境下问题依旧，请到[这里](https://github.com/manateelazycat/emacs-application-framework/issues/new)反馈。

## 加入我们
你想把Emacs开发成一个操作系统吗？

想要在Emacs里面生活的更舒适吗？

想要创建下一个激动人心的Emacs插件吗？

[一起疯吧!](https://github.com/manateelazycat/emacs-application-framework/wiki/Hacking)

## 打赏
如果我的作品让你的生活充满快乐，欢迎请我喝瓶啤酒，哈哈哈哈

<p float="left">
    <img src="./screenshot/alipay.jpg" width="188">
    <img src="./screenshot/wechat.jpg" width="200">
</p>
