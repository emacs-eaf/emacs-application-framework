## All bindings below can be configured with the function eaf-bind-key. See README for more info
### Browser

| Browser Key         | Event                            |
| :-----:             | :----                            |
| Left Button         | Open link in current tab         |
| Ctrl + Left Button  | Open link in new tab             |
| Ctrl + Double Click | Use sdcv translate selected text |
| M-f                 | Forward page in history          |
| M-b                 | Backward page in history         |
| M-q                 | Delete all cookies               |
| C-a                 | Move cursor to beginning of text |
| C-e                 | Move cursor to end of text       |
| C-=                 | Zoom in                          |
| C--                 | Zoom out                         |
| C-0                 | Zoom reset                       |
| C-n                 | Scroll up                        |
| C-p                 | Scroll down                      |
| C-v                 | Scroll up page                   |
| C-s                 | Search forward                   |
| C-r                 | Search backward                  |
| M-v                 | Scroll down page                 |
| M-<                 | Scroll to top                    |
| M->                 | Scroll to bottom                 |

You can customize keys in the variable ```eaf-browser-keybinding```

### PDF Viewer

| PDF Viewer Key | Event                               |
| :-----:        | :----                               |
| j              | Scroll up                           |
| k              | Scroll down                         |
| Space          | Scroll up page                      |
| b              | Scroll down page                    |
| ,              | Scroll to end                       |
| .              | Scroll to home                      |
| t              | Switch scale mode                   |
| -              | Zoom out                            |
| =              | Zoom in                             |
| 0              | Zoom reset                          |
| g              | Goto page                           |
| p              | Goto to percent                     |
| [              | Remember position                   |
| ]              | Remember jump                       |
| i              | Toggle inverted mode                |
| m              | Toggle mark link                    |
| s              | Search text                         |
| n              | Jump next match of search           |
| N              | Jump last match of search       |
| Double Click   | Use sdcv translate word under point |

You can customize in the variable ```eaf-pdf-viewer-keybinding```

### Video Player

| Video Player Key | Event         |
| :-----:          | :----         |
| Space            | Play or Pause |
| h                | Seek backward |
| l                | Seek forward  |

You can customize keys in the variable ```eaf-video-player-keybinding```

### Image Viewer

| Image Viewer Key | Event                                    |
| :-----:          | :----                                    |
| j                | Load next image in current directory     |
| k                | Load previous image in current directory |

You can customize keys in the variable ```eaf-image-viewer-keybinding```

### Terminal

| Terminal Key | Event    |
| :-----:      | :----    |
| C-=          | Zoom in  |
| C--          | Zoom out |

### Camera

| Camera Key | Event      |
| :-----:    | :----      |
| j          | Take photo |

You can customize keys in the variable ```eaf-camera-keybinding```
