[English](./README.md) | 简体中文

<p align="center">
  <img style='height: auto; width: 80%; object-fit: contain' src="./img/EAF_Banner_Transparent.png">
  <br>新一代的 Emacs 图形应用框架， 通过扩展 Emacs 的多媒体能力， 达到 Live in Emacs 的终极目标
</p>

## 愿景
<img src="./framework.png">

Emacs 距今已经有 45 年的发展历史， 比现在人们用的操作系统都老。 在这 45 年中， 全世界最顶级的黑客在贡献自己的智慧和想象力， 一起构建了 Emacs 这个伟大的开发者工具生态。

当你是一个需要使用十几门编程语言的黑客和键盘流信仰者， Emacs 绝对是你的不二之选。

Emacs 的劣势也是因为它太古老了， 导致在多线程和图形扩展能力已经无法跟上时代的步伐， 在很多地方发展落后于 IDEA 和 VSCode。

Emacs Application Framework (EAF)的愿景是在保留 Emacs 古老的黑客文化和庞大的开发者插件生态前提下， 通过 EAF 框架扩展 Emacs 的多线程和图形渲染能力， 实现 Live In Emacs 的理想。

## EAF 有哪些功能？
EAF 是一个可编程扩展的框架， 它自带一系列丰富的应用， 你可以自由选择哪些下载：

| 浏览器                                       | PDF 阅读器 |
| :--------:                                       | :----:                                                      |
| <img src="./img/browser.png" width="400"> | <img src="./img/pdf-viewer.png" width="400"> |

| 音乐播放器                       | 文件管理器                         |
| :--------:                                            | :----:                                                |
| <img src="./img/music-player.png" width="400"> | <img src="./img/file-manager.png" width="400"> |
|                                                       |                                                       |

- [Browser](https://github.com/emacs-eaf/eaf-browser): 全功能的网页浏览器， 基于 Chromium 渲染引擎
- [PDF Viewer](https://github.com/emacs-eaf/eaf-pdf-viewer): Emacs 里面渲染速度最快的 PDF 查看器
- [Music Player](https://github.com/emacs-eaf/eaf-music-player): 音乐播放器， 支持播放列表对齐渲染和实时音频反馈
- [Video Player](https://github.com/emacs-eaf/eaf-video-player): 基于 Qt 的视频播放器
- [Image Viewer](https://github.com/emacs-eaf/eaf-image-viewer): 支持实时缩放的图片查看器
- [RSS Reader](https://github.com/emacs-eaf/eaf-rss-reader): 新闻阅读器， 支持 Html 内容渲染
- [Terminal](https://github.com/mumu-lhl/eaf-pyqterminal): 支持图形绘制的全功能终端模拟器
- [MindMap](https://github.com/emacs-eaf/eaf-mind-elixir): 界面美观的全功能思维导图软件
- [Camera](https://github.com/emacs-eaf/eaf-camera): 摄像头程序
- [Markdown Previewer](https://github.com/emacs-eaf/eaf-markdown-previewer): Markdown 文档实时预览程序， 完美兼容 Github 样式, 支持 Mermaid、 PlantUML、 KaTeX、 MathJax 等内容的渲染
- [Org Previewer](https://github.com/emacs-eaf/eaf-org-previewer): Org 文件实时预览程序， 支持文件实时预览
- [Git Client](https://github.com/emacs-eaf/eaf-git): 多线程 Git 客户端
- [File Manager](https://github.com/emacs-eaf/eaf-file-manager): 多线程文件管理器

... 还有[很多](https://github.com/orgs/emacs-eaf/repositories)！

### EAF 在 EmacsConf

| EmacsConf 2020: <a href="https://emacsconf.org/2020/talks/34/">用 EAF 扩展 Emacs 图形应用</a> | EmacsConf 2021: <a href="https://emacsconf.org/2021/talks/eaf/">EAF 2021 更新报告</a>                              |
| :--------:                                            | :----:                                                |
| [<img src="https://img.youtube.com/vi/HK_f8KTuR0s/0.jpg" width=400>](https://www.youtube.com/watch?v=HK_f8KTuR0s) | [<img src="https://img.youtube.com/vi/bh37zbefZk4/0.jpg" width=400>](https://www.youtube.com/watch?v=bh37zbefZk4) |
|                                                       |                                                       |


## 安装

EAF 可以在多个操作系统下工作， 包括 Linux (X11 和 Wayland), Windows， macOS 和 FreeBSD， 安装方法非常简单。

#### 1. 下载 EAF

```Bash
git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git ~/.emacs.d/site-lisp/emacs-application-framework/
```

#### 2. 安装 EAF 依赖

调用 Elisp 函数`M-x eaf-install-and-update`或者手动在 Terminal 跑`install-eaf.py`安装脚本：

```Bash
cd emacs-application-framework
chmod +x ./install-eaf.py
./install-eaf.py
```

`install-eaf.py`脚本有许多有用的选项， 可以通过`--help`查看。

Wayland 环境的支持分四种情况：
- XWayland: EAF 可在 XWayland 下直接运行
- Gnome3 Wayland Native： 你需要执行命令 `cp -r emacs-application-framework/gnome-shell/eaf-wayland@emacs-eaf.org ~/.local/share/gnome-shell/extensions` 并在 `gnome-extensions` 中激活 `eaf-wayland@emacs-eaf.org` 插件
- Sway Wayland Native： 需要安装 jq
- Hyprland Wayland Native

Unity 桌面环境支持： 需要安装 xdotool

#### 3. 加载 EAF 核心

从这里开始， 你可以把 EAF 加入 Emacs 的 ```load-path```， 然后在 `init.el` 中写入:

```Elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)
```
- 或者， 如果你使用 [use-package](https://github.com/jwiegley/use-package)， 下面有一个简单的配置文件供你参考:

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

#### 4. 加载 EAF 应用

你可以用下面的代码来加载一部分 EAF 应用， 比如浏览器、 PDF 阅读器和视频播放器， 更多的应用请查看 [应用列表](https://github.com/emacs-eaf/emacs-application-framework#applications):

```Elisp
(require 'eaf-browser)
(require 'eaf-pdf-viewer)
```

#### 5. 下载完成！

恭喜， 到这一步你已成功下载好了 EAF！ 你可以通过`M-x eaf-open-demo`（前提是你下载了`demo`应用）看看 EAF 是否可以成功运行了。

下面是 EAF 应用的启动命令:

| 应用名称         | 启动命令                                                                    |
| :--------        | :----                                                                       |
| 浏览器           | `M-x eaf-open-browser` 在浏览器中打开或搜索                                 |
|                  | `M-x eaf-open-browser-with-history` 搜索历史或者打开 URL                     |
| HTML 邮件渲染     | `M-x eaf-open-mail-as-html` 在 `gnus`， `mu4e`， `notmuch` 等邮件客户端中执行 |
| PDF 阅读器        | `M-x eaf-open` 输入 PDF 文件                                                  |
| 视频播放器       | `M-x eaf-open` 输入视频文件                                                 |
| 图片浏览器       | `M-x eaf-open` 输入图片文件                                                 |
| Markdown 预览     | `M-x eaf-open` 输入 Markdown 文件, 选择 markdown-previewer                                             |
| Org 预览          | `M-x eaf-open` 输入 Org 文件, 选择 org-previewer                                                  |
| 摄像头程序       | `M-x eaf-open-camera`                                                       |
| 终端模拟器       | `M-x eaf-open-pyqterminal`                                                     |
| 文件管理器  | `M-x eaf-open-in-file-manager`                                         |
| 新闻阅读器  | `M-x eaf-open-rss-reader`                                         |
| Git 客户端  | `M-x eaf-open-git`                                         |
| 地图路径规划  | `M-x eaf-open-map`                                         |
| 二维码下载文件   | `M-x eaf-file-sender-qrcode` or `eaf-file-sender-qrcode-in-dired`           |
| 二维码在线浏览器 | `M-x eaf-file-browser-qrcode`                                               |
| 无线分享         | `M-x eaf-open-airshare` 输入要分享给手机的字符串                            |
| Markdown 思维导图预览         | `M-x eaf-open` 输入 Markdown 或 Org 文件， 选择 markmap
| 思维导图         | `M-x eaf-create-mindmap` or `M-x eaf-open-mindmap`                          |
| 微软 Office 阅读器 | `M-x eaf-open-office`                                                       |
| jupyter          | `M-x eaf-open-jupyter`                                                      |
| 音乐             | `M-x eaf-open-music-player`                                                 |
| 系统监视器       | `M-x eaf-open-system-monitor`                                        |
| 演示程序         | `M-x eaf-open-demo`                                                         |
| Vue.js 演示程序   | `M-x eaf-open-vue-demo`                                                     |

- EAF 浏览器以及 PDF 浏览器支持 Emacs 内置书签操作， 通过使用`M-x bookmark-set`（默认`C-x r m`）以及`M-x bookmark-bmenu-list`（默认`C-x r l`）。

## 更新
建议你时常`git pull` **并且** 运行`install-eaf.py` (`M-x eaf-install-and-update`)来更新各个 EAF 应用及其依赖。

## 反馈问题

### 反馈安装和配置问题之前， 请一定先阅读 [Wiki](https://github.com/emacs-eaf/emacs-application-framework/wiki) 以及[常用问题](https://github.com/emacs-eaf/emacs-application-framework/wiki/%E5%B8%B8%E8%A7%81%E9%97%AE%E9%A2%98)。

如果你使用中遇到任何问题， 并且问题是`git pull`后出现的， 请先阅读 [Mandatory Procedures to Keep Your EAF Up-To-Date](https://github.com/emacs-eaf/emacs-application-framework/discussions/527?sort=new) 页面。

关于其他问题， 请用命令 `emacs -q` 并只添加 EAF 配置做一个对比测试， 如果 `emacs -q` 可以正常工作， 请检查你个人的配置文件。

如果`emacs -q`环境下问题依旧， 请到[这里](https://github.com/emacs-eaf/emacs-application-framework/issues/new) 反馈, 并附带 `*eaf*` 窗口的内容给我们提交 issue， 那里面有很多线索可以帮助我们排查问题。

如果你遇到崩溃的问题, 请用下面的方式来收集崩溃信息:
1. 先安装 gdb 并打开选项 `(setq eaf-enable-debug t)`
2. 使用命令 `eaf-stop-process` 停止 EAF 进程
3. 重新打开 EAF, 并在下次崩溃时发送 `*eaf*` 的内容

## EAF 社区

下面列表列展示了 EAF 在 Emacs 社区的应用。 如果我们遗漏你的应用， 欢迎提交 PR 来加到下面列表中。

* ***[obr-viz](https://github.com/swhalemwo/obr-viz)***: visualizing [org-brain](https://github.com/Kungsgeten/org-brain) relationships using EAF
* ***[netease-cloud-music](https://github.com/SpringHan/netease-cloud-music.el)***: A netease music client for emacs.
* ***[2048pyqt6](https://github.com/porrige/2048pyqt6)***: A 2048 game that can run in emacs.
* ***[pyqterminal](https://github.com/mumu-lhl/eaf-pyqterminal)***: A terminal written in PyQt6.

## 贡献者
<a href = "https://github.com/emacs-eaf/emacs-application-framework/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=emacs-eaf/emacs-application-framework"/>
</a>

## 加入我们
你想把 Emacs 开发成一个操作系统吗？

想要在 Emacs 里面生活的更舒适吗？

想要创建下一个激动人心的 Emacs 插件吗？

[一起疯吧!](https://github.com/emacs-eaf/emacs-application-framework/wiki/Hacking)
