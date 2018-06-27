## Why choose Qt?
Qt's QGraphicsView and QGraphicsScene is awesome, it's easier to implement window composite than other GUI library (such as GTK+).

If use Gtk+ or other GUI library, I need write many Widget/XComposite code to implement widget like QGraphicsView/QGraphicsScene.

## Why choose Python?
C/C++ need compile long time for every change, this will interrupt my attention and enthusiasm of development.

Python is a perfect language to develop Qt program and it can call pretty much in every library you need.

## Let me run hello word
```
M-x eaf-open
```

    Then type "eaf rocks!" as input, will pop hello world window in emacs like below:

![img](./screenshot/hello_world.png)

    It's a big hello button, try to click it, haha.

## Develop new plugin
It's very easy if you know how to write PyQt5 code.

Here have awesome tutorial help you study PyQt5: http://zetcode.com/gui/pyqt5/

Trust me, PyQt5 is pretty easy to study.

After you know how to write PyQt5 code, developing new plugin just needs 3 steps:

1. Open file [buffer.py](app/demo/buffer.py):
```Python
from PyQt5.QtGui import QColor
from PyQt5.QtWidgets import QPushButton
from buffer import Buffer

class DemoBuffer(Buffer):
    def __init__(self, buffer_id, url):
        Buffer.__init__(self, buffer_id, url, True, QColor(0, 0, 0, 255))

        self.add_widget(QPushButton("Hello, EAF hacker, it's work!!!"))
        self.buffer_widget.setStyleSheet("font-size: 100px")
```

    Replace QPushButton with your PyQt5 widget.
    
* buffer_id and url are need by framework, you just need pass those paramaters to Buffer class
    
* third paramater True mean application content will fit size with emacs window size change, such as image viewer. 
    
* third paramater False mean applicaton content won't fit size with emacs window size change, such as browser.
    
* fourth paramater is background color to fill application background.

2. Open file [eaf.py](core/eaf.py):
```Python
@dbus.service.method(EAF_DBUS_NAME, in_signature="ss", out_signature="s")
def new_buffer(self, buffer_id, url):
    if url == "eaf rocks!":
        self.create_buffer(buffer_id, DemoBuffer(buffer_id, url))
```

    Replace "eaf rocks!" to "i am rocks!"

3. Test
```
    Execute command `eaf-stop-process' to kill old python process.

    Execute command `eaf-open' and type "i am rocks!".
```
    
    

See? It's so easy!

Above are all you need, happy hacking!

## Todolist
[Some works you can hacking ;)](TODOLIST.md)
