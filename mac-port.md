# Current status of eaf-mac-port

Because QWindow Reparent technology doesn't work on macOS as the same way on Linux or Windows, we currently use the StayOnTop QWindow to replace it. Every EAF window is placed at the right position above the Emacs window when displayed. A big limitation is that when switching to another application, a StayOnTop window will cover the application window. So we just create a temporary buffer to replace the EAF buffer when Emacs loses focus, and switch back to the previous window configuration when Emacs gets the focus again.

# Known issues
+ When Emacs is out of focus, EAF buffers can't be displayed.
+ The usage of multiple frames has not been considered, so only opening one Emacs frame works well.
+ Fullscreen display may cause the mouse moving to the corner.
+ PyQtWebEngine installed by pip isn't compiled with private codec support, so some video on the website can't be played.
