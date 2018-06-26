# What is Emacs Application Framework?
Emacs Application Framework is a development framework that developers can develop any PyQt program and integrate into Emacs.

This framework mainly implements three functions:
1. Integrate PyQt program window into Emacs Frame using Xlib Reparent technology
2. Listening to EAF buffer's keyboard event flow and controlling the keyboard input of PyQt program via DBus IPC
3. Created a window compositer to make the PyQt program window adapt Emacs's Window/Buffer design

Using this framework, you can use PyQt develop powerful graphics programs to extend Emacs

## Some screenshots

### Browser
![img](./screenshot/browser.gif)

### Image Viewer
![img](./screenshot/image_viewer.gif)

### Video Player
![img](./screenshot/video_player.gif)

## Installation

1. Install python libraries (below commands use for archlinux)
```Bash
sudo pacman -S python-xlib python-pyqt5 python-pymediainfo
```

2. Clone this repository and add below code in your ~/.emacs
```Elisp
(require 'eaf)
```

## Usage

```
M-x eaf-open
```

Such as,
* type www.google.com to open browser, Ctrl + LeftButton open link in new tab
* type /path/image.jpg to open image viewer, and press key j or k to select other image in same directory
* type /path/video.ogg to open video player, video player only support ogg file because it implement by HTML5 video tag

## Report bug
If you found eaf message "*eaf* exited abnormally with ...", it mean something wrong in python.

Please switch buffer *eaf* and paste content of *eaf* to me.

Thanks!


## Join Us
Do you want to make Emacs a real operating system?

Do you want to live in emacs more comfortably?

Want to create unparalleled plugins to extend emacs?

*Join us!*

## Why choose Qt?
Qt's QGraphicsView and QGraphicsScene is awesome, it's easier to implement window composite than other GUI library (such as GTK+).

If use Gtk+ or other GUI library, i need write many Widget/XComposite code to implement widget like QGraphicsView/QGraphicsScene.

## Why choose Python?
C/C++ need compile long time for every change, this will interrupt my attention and development enthusiasm.

Python is perfect language to develop Qt program and it can call pretty much every library you need.

## How to develop new plugins?

1. Create new python plugin file:
```Bash
mkdir -p emacs-application-framework/app/foo/buffer.py
```

2. Fill python file with below template:
```Python
from PyQt5.QtGui import QColor
from PyQt5.QtWidgets import QLabel
from buffer import Buffer

class FooBuffer(Buffer):
    def __init__(self, buffer_id, url, width, height):
        Buffer.__init__(self, buffer_id, url, width, height, QColor(255, 255, 255, 255))

        self.add_widget(QLabel("foo"))
        self.buffer_widget.resize(self.width, self.height)

    def resize_buffer(self, width, height):
        self.width = width
        self.height = height
        self.buffer_widget.resize(self.width, self.height)

```

3. Open emacs-application-framework/core/eaf.py, import plugins buffer module and change `new_buffer` function to launch plugin buffer

```Python
from app.foo.buffer import FooBuffer

...

    @dbus.service.method(EAF_DBUS_NAME, in_signature="ss", out_signature="s")
    def new_buffer(self, buffer_id, url):
        ...

        if url.endswith(".foo"):
	    self.create_buffer(buffer_id, FooBuffer(buffer_id, url, emacs_width, emacs_height))

...
```

4. Try new plugin:

* Call command `eaf-stop-process` to kill old python process first.
* Then call command `eaf-open' to test new plugin

## Todo list
* Browser: add cookie support
* Browser: support pop window, such as emacs-china.org
* Browser: add progressbar
* ImageViewer: add zoom support

## Contact me

lazycat dot manatee at gmail dot com

Any suggestions and patches are welcome, happy hacking!
