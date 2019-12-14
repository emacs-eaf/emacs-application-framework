;;; eaf.el --- Emacs application framework  -*- lexical-binding: t; -*-

;; Filename: eaf.el
;; Description: Emacs application framework
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: Sat Dec 14 17:34:32 2019 (-0500)
;;           By: Mingde (Matthew) Zeng
;; URL: http://www.emacswiki.org/emacs/download/eaf.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Emacs application framework
;;

;;; Installation:
;;
;; Put eaf.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eaf)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET eaf RET
;;

;;; Change log:
;;
;; 2018/06/15
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'dbus)
(require 'subr-x)

;;; Code:

(defgroup eaf nil
  "Emacs Application Framework."
  :group 'applications)

(defcustom eaf-mode-hook '()
  "EAF mode hook."
  :type 'hook)

(defvar eaf-mode-map*
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-h m") 'eaf-describe-bindings)
    (define-key map [remap describe-bindings] 'eaf-describe-bindings)
    (define-key map (kbd "C-c b") 'eaf-open-bookmark)
    (define-key map (vector 'remap 'self-insert-command) #'eaf-send-key)
    (dolist (single-key '("RET" "DEL" "TAB" "SPC" "<backtab>" "<home>" "<end>" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>"))
      (define-key map (kbd single-key) #'eaf-send-key))
    map)
  "Keymap for default bindings available in all apps.")

(defvar eaf-mode-map nil
  "Keymap used by `eaf-mode'.

Don't modify this map directly. To bind keys for all apps use
`eaf-mode-map*' and to bind keys for individual apps use
`eaf-bind-key'.")

(defun eaf-describe-bindings ()
  "Like `describe-bindings' for EAF buffers."
  (interactive)
  (let ((emulation-mode-map-alists nil)
        (eaf-mode-map (current-local-map)))
    (call-interactively 'describe-mode)))

(defvar-local eaf--buffer-id nil
  "Internal id used by EAF app.")

(defvar-local eaf--buffer-url nil
  "The buffer url.")

(defvar-local eaf--buffer-app-name nil
  "The buffer app name.")

(define-derived-mode eaf-mode fundamental-mode "EAF"
  "Major mode for Emacs Application Framework.

This mode is used by all apps. Each app can setup app specific
hooks by declaring `eaf-<app-name>-hook'. This hook runs after
the app buffer has initialized."
  ;; Split window combinations proportionally.
  ;; FIXME: this changes this setting globally for the user
  ;; which may not want this, introduce EAF user option?
  (setq window-combination-resize t)
  (set (make-local-variable 'eaf--buffer-id) (eaf-generate-id))
  (setq-local bookmark-make-record-function #'eaf--bookmark-make-record)
  ;; copy default value in case user already has bindings there
  (setq-local emulation-mode-map-alists
              (default-value 'emulation-mode-map-alists))
  (push (list (cons t eaf-mode-map))
        emulation-mode-map-alists)
  (add-hook 'kill-buffer-hook #'eaf-monitor-buffer-kill nil t))

(defvar eaf-python-file (expand-file-name "eaf.py" (file-name-directory load-file-name)))

(defvar eaf-process nil)

(defvar eaf-first-start-url nil)

(defvar eaf-first-start-app-name nil)

(defvar eaf-first-start-arguments nil)

(defvar eaf-title-length 30)

(defvar eaf-org-file-list '())

(defvar eaf-org-killed-file-list '())

(defvar eaf-last-frame-width 0)

(defvar eaf-last-frame-height 0)

(defvar eaf-grip-token nil)

(defvar eaf-http-proxy-host "")

(defvar eaf-http-proxy-port "")

(defvar eaf-find-alternate-file-in-dired nil
  "If non-nil, when calling `eaf-file-open-in-dired', EAF unrecognizable files will be opened
by `dired-find-alternate-file'. Otherwise they will be opened normally with `dired-find-file'.")

(defcustom eaf-name "*eaf*"
  "Name of EAF buffer."
  :type 'string)

(defcustom eaf-python-command "python3"
  "The Python interpreter used to run eaf.py."
  :type 'string)

(defcustom eaf-var-list
  '(
    (eaf-camera-save-path . "~/Downloads")
    (eaf-browser-enable-plugin . "true")
    (eaf-browser-enable-javascript . "true")
    )
  "The alist storing user-defined variables that's shared with EAF Python side.

Try not to modify this alist directly. Use `eaf-setq' to modify instead."
  :type 'cons)

(defcustom eaf-browser-keybinding
  '(("M-f" . "history_forward")
    ("M-b" . "history_backward")
    ("M-q" . "clean_all_cookie")
    ("C--" . "zoom_out")
    ("C-=" . "zoom_in")
    ("C-0" . "zoom_reset")
    ("C-n" . "scroll_up")
    ("C-p" . "scroll_down")
    ("C-v" . "scroll_up_page")
    ("M-v" . "scroll_down_page")
    ("M-<" . "scroll_to_begin")
    ("M->" . "scroll_to_bottom")
    ("<f5>" . "refresh_page"))
  "The keybinding of EAF Browser."
  :type 'cons)

(defcustom eaf-browser-key-alias
  '(("C-a" . "<home>")
    ("C-e" . "<end>"))
  "The key alias of EAF Browser."
  :type 'cons)

(defcustom eaf-pdf-viewer-keybinding
  '(("j" . "scroll_up")
    ("k" . "scroll_down")
    ("<down>" . "scroll_up")
    ("<up>" . "scroll_down")
    ("SPC" . "scroll_up_page")
    ("b" . "scroll_down_page")
    ("t" . "switch_to_read_mode")
    ("." . "scroll_to_home")
    ("," . "scroll_to_end")
    ("0" . "zoom_reset")
    ("=" . "zoom_in")
    ("-" . "zoom_out")
    ("g" . "jump_to_page")
    ("p" . "jump_to_percent")
    ("[" . "remember_current_position")
    ("]" . "remember_jump")
    ("i" . "toggle_inverted_mode"))
  "The keybinding of EAF PDF Viewer."
  :type 'cons)

(defcustom eaf-video-player-keybinding
  '(("SPC" . "toggle_play")
    ("h" . "play_backward")
    ("l" . "play_forward"))
  "The keybinding of EAF Video Player."
  :type 'cons)

(defcustom eaf-image-viewer-keybinding
  '(("j" . "load_next_image")
    ("k" . "load_prev_image"))
  "The keybinding of EAF Image Viewer."
  :type 'cons)

(defcustom eaf-terminal-keybinding
  '(("C--" . "zoom_out")
    ("C-=" . "zoom_in"))
  "The keybinding of EAF Terminal."
  :type 'cons)

(defcustom eaf-camera-keybinding
  '(("j" . "take_photo"))
  "The keybinding of EAF Camera."
  :type 'cons)

(defcustom eaf-pdf-extension-list
  '("pdf" "xps" "oxps" "cbz" "epub" "fb2" "fbz" "djvu")
  "The extension list of pdf application."
  :type 'cons)

(defcustom eaf-markdown-extension-list
  '("md")
  "The extension list of markdown previewer application."
  :type 'cons)

(defcustom eaf-image-extension-list
  '("jpg" "jpeg" "png" "bmp")
  "The extension list of image viewer application."
  :type 'cons)

(defcustom eaf-video-extension-list
  '("avi" "rmvb" "ogg" "mp4" "mkv")
  "The extension list of video player application."
  :type 'cons)

(defcustom eaf-browser-extension-list
  '("html")
  "The extension list of browser application."
  :type 'cons)

(defcustom eaf-org-extension-list
  '("org")
  "The extension list of org previewer application."
  :type 'cons)

(defvar eaf-app-binding-alist
  '(("browser" . eaf-browser-keybinding)
    ("pdf-viewer" . eaf-pdf-viewer-keybinding)
    ("video-player" . eaf-video-player-keybinding)
    ("image-viewer" . eaf-image-viewer-keybinding)
    ("camera" . eaf-camera-keybinding)
    ("terminal" . eaf-terminal-keybinding))
  "Mapping app names to keybinding variables.

Any new app should add the its name and the corresponding
keybinding variable to this list.")

(defvar eaf-app-display-function-alist
  '(("markdown-previewer" . eaf--markdown-preview-display)
    ("org-previewer" . eaf--org-preview-display))
  "Mapping app names to display functions.

Display functions are called to initilize the initial view when
starting an app.

A display function receives the initialized app buffer as
argument and defaults to `switch-to-buffer'.")


(defvar eaf-app-bookmark-handlers-alist
  '(("browser" . eaf--browser-bookmark)
    ("pdf-viewer" . eaf--pdf-viewer-bookmark))
  "Mapping app names to bookmark handler functions.

A bookmark handler function is used as
`bookmark-make-record-function' and should follow its spec.")

(defvar eaf-app-extensions-alist
  '(("pdf-viewer" . eaf-pdf-extension-list)
    ("markdown-previewer" . eaf-markdown-extension-list)
    ("image-viewer" . eaf-image-extension-list)
    ("video-player" . eaf-video-extension-list)
    ("browser" . eaf-browser-extension-list)
    ("org-previewer" . eaf-org-extension-list))
  "Mapping app names to extension list variables.

A new app can use this to configure extensions which should
handled by it.")


(defvar-local eaf--bookmark-title nil)

(defun eaf--bookmark-make-record ()
  "Create a EAF bookmark.

The bookmark will try to recreate EAF buffer session.
For now only EAF browser app is supported."
  (let ((handler (cdr
                  (assoc eaf--buffer-app-name
                         eaf-app-bookmark-handlers-alist))))
    (when handler
      (funcall handler))))

(defun eaf--browser-bookmark ()
  `((handler . eaf--bookmark-restore)
    (eaf-app . "browser")
    (defaults . ,(list eaf--bookmark-title))
    (filename . ,(eaf-get-path-or-url))))

(defun eaf--pdf-viewer-bookmark ()
  `((handler . eaf--bookmark-restore)
    (eaf-app . "pdf-viewer")
    (defaults . ,(list eaf--bookmark-title))
    (filename . ,(eaf-get-path-or-url))))

(defun eaf--bookmark-restore (bookmark)
  "Restore EAF buffer according to BOOKMARK."
  (let ((app (cdr (assq 'eaf-app bookmark))))
    (cond ((equal app "browser")
           (eaf-open-url (cdr (assq 'filename bookmark))))
          ((equal app "pdf-viewer")
           (eaf-open (cdr (assq 'filename bookmark)))))))

(defun eaf-open-bookmark ()
  "Command to open or create EAF bookmarks with completion."
  (interactive)
  (bookmark-maybe-load-default-file)
  (let* ((bookmarks (cl-remove-if-not
                     (lambda (entry)
                       (bookmark-prop-get entry 'eaf-app))
                     bookmark-alist))
         (names (mapcar #'car bookmarks))
         (cand (completing-read "EAF Bookmarks: " bookmarks)))
    (cond ((member cand names)
           (bookmark-jump cand))
          (t
           (unless (derived-mode-p 'eaf-mode)
             (user-error "Not in an EAF buffer"))
           ;; create new one for current buffer with provided name
           (bookmark-set cand)))))

(defun eaf-call (method &rest args)
  (apply #'dbus-call-method
         :session                   ; use the session (not system) bus
         "com.lazycat.eaf"          ; service name
         "/com/lazycat/eaf"         ; path name
         "com.lazycat.eaf"          ; interface name
         method args))

(defun eaf-get-emacs-xid (frame)
  (frame-parameter frame 'window-id))

(defun eaf-start-process ()
  "Start EAF process if it hasn't started yet."
  (interactive)
  (if (process-live-p eaf-process)
      (message "EAF process has started.")
    (setq eaf-process
          (apply #'start-process
                 eaf-name
                 eaf-name
                 eaf-python-command (append (list eaf-python-file) (eaf-get-render-size) (list eaf-http-proxy-host eaf-http-proxy-port))
                 ))
    (set-process-query-on-exit-flag eaf-process nil)
    (set-process-sentinel
     eaf-process
     #'(lambda (process event)
         (message "%s %s" process event)))
    (message "EAF process starting...")))

(defun eaf-stop-process ()
  (interactive)
  ;; Kill EAF buffers.
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when (derived-mode-p 'eaf-mode)
        (cl-incf count)
        (kill-buffer buffer)))
    ;; Just report to me when EAF buffer exists.
    (if (> count 1)
        (message "Killed EAF %s buffer%s" count (if (> count 1) "s" ""))))

  ;; Clean cache url and app name, avoid next start process to open buffer.
  (setq eaf-first-start-url nil)
  (setq eaf-first-start-app-name nil)
  (setq eaf-first-start-arguments nil)

  ;; Clean `eaf-org-file-list' and `eaf-org-killed-file-list'.
  (dolist (org-file-name eaf-org-file-list)
    (eaf-delete-org-preview-file org-file-name))
  (setq eaf-org-file-list nil)
  (setq eaf-org-killed-file-list nil)

  ;; Kill process after kill buffer, make application can save session data.
  (eaf-kill-python-process))

(defun eaf-kill-python-process ()
  "Kill EAF background python process for debug.
NOTE: this function just use for developer debug.
Don't call this function if you not EAF developer."
  (interactive)
  (if (process-live-p eaf-process)
      ;; Delete EAF server process.
      (delete-process eaf-process)
    (message "EAF process has dead.")))

(defun eaf-restart-process ()
  (interactive)
  (eaf-stop-process)
  (eaf-start-process))

(defun eaf-get-render-size ()
  "Get allocation for render application in backend.
We need calcuate render allocation to make sure no black border around render content."
  (let* (;; We use `window-inside-pixel-edges' and `window-absolute-pixel-edges' calcuate height of window header, such as tabbar.
         (window-header-height (- (nth 1 (window-inside-pixel-edges)) (nth 1 (window-absolute-pixel-edges))))
         (width (frame-pixel-width))
         ;; Render height should minus mode-line height, minibuffer height, header height.
         (height (- (frame-pixel-height) (window-mode-line-height) (window-pixel-height (minibuffer-window)) window-header-height)))
    (mapcar (lambda (x) (format "%s" x)) (list width height))))

(defun eaf-get-window-allocation (&optional window)
  (let* ((window-edges (window-inside-pixel-edges window))
         (x (nth 0 window-edges))
         (y (nth 1 window-edges))
         (w (- (nth 2 window-edges) x))
         (h (- (nth 3 window-edges) y)))
    (list x y w h)))

(defun eaf-generate-id ()
  (format "%04x-%04x-%04x-%04x-%04x-%04x-%04x"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))))

(defun eaf-execute-app-cmd (cmd &optional buf)
  "Execute app CMD.

If BUF is given it should be the EAF buffer for the command
otherwise it is assumed that the current buffer is the EAF
buffer."
  (with-current-buffer (or buf (current-buffer))
    (let ((this-command cmd))
      (call-interactively cmd))))

(defun eaf-get-path-or-url ()
  "Get the current file path or web URL, and copy to ‘kill-ring’."
  (interactive)
  (if (derived-mode-p 'eaf-mode)
      (message (kill-new (eaf-call "call_function" eaf--buffer-id "get_url")))
    (user-error "This command can only be called in an EAF buffer!")))

(defun eaf-proxy-function (fun)
  "Define elisp command which can call python function FUN."
  (let ((sym (intern (format "eaf--proxy-%s" fun))))
    (unless (fboundp sym)
      (defalias sym
        (lambda nil
          (interactive)
          ;; Ensure this is only called from EAF buffer
          (unless eaf--buffer-id
            (error "%s command can only be called in an EAF buffer" sym))
          (eaf-call "execute_function" eaf--buffer-id fun))
        (format
         "Proxy function to call \"%s\" on the Python side.

Use `eaf-execute-app-cmd' if you want to execute this command programmatically.
Please ONLY use `eaf-bind-key' and use the unprefixed command name (\"%s\")
to edit EAF keybindings!" fun fun)))
    sym))

(defun eaf-gen-keybinding-map (keybinding)
  "Configure the `eaf-mode-map' from KEYBINDING, one of the eaf-.*-keybinding variables."
  (setq eaf-mode-map
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map eaf-mode-map*)
          (cl-loop for (key . fun) in keybinding
                   do (define-key map (kbd key)
                        (if (symbolp fun) fun (eaf-proxy-function fun)))
                   finally return map))))

(defun eaf-get-app-bindings (app-name)
  (symbol-value
   (cdr (assoc app-name eaf-app-binding-alist))))

(defun eaf-create-buffer (input-content app-name)
  "Create an EAF buffer given INPUT-CONTENT and APP-NAME."
  (eaf-gen-keybinding-map (eaf-get-app-bindings app-name))
  (let* ((file-or-command-name (substring input-content (string-match "[^/]*/?$" input-content)))
         (eaf-buffer (generate-new-buffer (truncate-string-to-width file-or-command-name eaf-title-length))))
    (with-current-buffer eaf-buffer
      (eaf-mode)
      (set (make-local-variable 'eaf--buffer-url) input-content)
      (set (make-local-variable 'eaf--buffer-app-name) app-name)
      (run-hooks (intern (format "eaf-%s-hook" app-name))))
    eaf-buffer))

(defun eaf-is-support (url)
  (dbus-call-method
   :session "com.lazycat.eaf"
   "/com/lazycat/eaf"
   "com.lazycat.eaf"
   "is_support"
   url))

(defun eaf-monitor-window-size-change (frame)
  (when (process-live-p eaf-process)
    (setq eaf-last-frame-width (frame-pixel-width frame))
    (setq eaf-last-frame-height (frame-pixel-height frame))
    (run-with-timer 1 nil (lambda () (eaf-try-adjust-view-with-frame-size)))))

(defun eaf-try-adjust-view-with-frame-size ()
  (when (and (equal (frame-pixel-width) eaf-last-frame-width)
             (equal (frame-pixel-height) eaf-last-frame-height))
    (eaf-monitor-configuration-change)))

(defun eaf-monitor-configuration-change (&rest _)
  (when (process-live-p eaf-process)
    (ignore-errors
      (let (view-infos)
        (dolist (frame (frame-list))
          (dolist (window (window-list frame))
            (let ((buffer (window-buffer window)))
              (with-current-buffer buffer
                (if (derived-mode-p 'eaf-mode)
                    (let* ((window-allocation (eaf-get-window-allocation window))
                           (x (nth 0 window-allocation))
                           (y (nth 1 window-allocation))
                           (w (nth 2 window-allocation))
                           (h (nth 3 window-allocation))
                           )
                      (push (format "%s:%s:%s:%s:%s:%s"
                                    eaf--buffer-id
                                    (eaf-get-emacs-xid frame)
                                    x y w h)
                            view-infos)
                      ))))))
        ;; I don't know how to make Emacs send dbus-message with two-dimensional list.
        ;; So I package two-dimensional list in string, then unpack on server side. ;)
        (eaf-call "update_views" (mapconcat #'identity view-infos ","))
        ))))

(defun eaf-delete-org-preview-file (org-file)
  (let ((org-html-file (concat (file-name-sans-extension org-file) ".html")))
    (when (file-exists-p org-html-file)
      (delete-file org-html-file)
      (message (format "Clean org preview file %s (%s)" org-html-file org-file)))))

(defun eaf-org-killed-buffer-clean ()
  (dolist (org-killed-buffer eaf-org-killed-file-list)
    (unless (get-file-buffer org-killed-buffer)
      (setq eaf-org-file-list (remove org-killed-buffer eaf-org-file-list))
      (eaf-delete-org-preview-file org-killed-buffer)))
  (setq eaf-org-killed-file-list nil))

(defun eaf-monitor-buffer-kill ()
  (ignore-errors
    (eaf-call "kill_buffer" eaf--buffer-id)
    (message (format "Kill %s" eaf--buffer-id))))

(defun eaf--org-preview-monitor-kill ()
  ;; NOTE:
  ;; Because save org buffer will trigger `kill-buffer' action,
  ;; but org buffer still live after do `kill-buffer' action.
  ;; So i run a timer to check org buffer is live after `kill-buffer' aciton.
  (when (member (buffer-file-name) eaf-org-file-list)
    (unless (member (buffer-file-name) eaf-org-killed-file-list)
      (push (buffer-file-name) eaf-org-killed-file-list))
    (run-with-timer 1 nil (lambda () (eaf-org-killed-buffer-clean)))))


(defun eaf--org-preview-monitor-buffer-save ()
  (when (process-live-p eaf-process)
    (ignore-errors
      ;; eaf-org-file-list?
      (org-html-export-to-html)
      (eaf-call "update_buffer_with_url" "app.org-previewer.buffer" (buffer-file-name) "")
      (message (format "export %s to html" (buffer-file-name))))))

(defun eaf-send-key ()
  (interactive)
  (with-current-buffer (current-buffer)
    (eaf-call "send_key" eaf--buffer-id (key-description (this-command-keys-vector)))))

(defun eaf-set (sym val)
  "Similar to `set', but store SYM with VAL in the EAF Python side.

For convenience, use the Lisp macro `eaf-setq' instead."
  (map-put eaf-var-list sym val))

(defmacro eaf-setq (var val)
  "Similar to `setq', but store VAR with VAL in the EAF Python side.

Use it as (eaf-setq var val)"
  `(eaf-set ',var ,val))

(defmacro eaf-bind-key (command key eaf-app-keybinding)
  "This function binds COMMAND to KEY in EAF-APP-KEYBINDING list.

COMMAND is a command with `eaf--proxy-' prefix found by calling
`eaf-describe-bindings' in an EAF buffer, but dropping the prefix.

KEY is a string representing a sequence of keystrokes and events.

EAF-APP-KEYBINDING is one of the eaf-.*-keybinding variables.

This is used to bind key to EAF Python applications."
  `(map-put ,eaf-app-keybinding ,key ,(symbol-name command)))

(defun eaf-focus-buffer (msg)
  (let* ((coordinate-list (split-string msg ","))
         (mouse-press-x (string-to-number (nth 0 coordinate-list)))
         (mouse-press-y (string-to-number (nth 1 coordinate-list))))
    (catch 'find-window
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (with-current-buffer buffer
            (if (derived-mode-p 'eaf-mode)
                (let* ((window-allocation (eaf-get-window-allocation window))
                       (x (nth 0 window-allocation))
                       (y (nth 1 window-allocation))
                       (w (nth 2 window-allocation))
                       (h (nth 3 window-allocation))
                       )
                  (when (and
                         (< x mouse-press-x (+ x w))
                         (< y mouse-press-y (+ y h)))
                    (select-window window)
                    (throw 'find-window t))))))))))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "message_to_emacs"
 #'message)

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "create_new_browser_buffer"
 #'eaf-create-new-browser-buffer)

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "set_emacs_var"
 #'eaf-set-emacs-var)

(defun eaf-set-emacs-var (var-name var-value)
  (set (intern var-name) var-value))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "eval_in_emacs"
 #'eaf-eval-in-emacs)

(defun eaf-eval-in-emacs (elisp-code-string)
  (eval (read elisp-code-string) 'lexical))

(defun eaf-create-new-browser-buffer (new-window-buffer-id)
  (let ((eaf-buffer (generate-new-buffer (concat "Browser Popup Window " new-window-buffer-id))))
    (with-current-buffer eaf-buffer
      (eaf-mode)
      (set (make-local-variable 'eaf--buffer-id) new-window-buffer-id)
      (set (make-local-variable 'eaf--buffer-url) "")
      (set (make-local-variable 'eaf--buffer-app-name) "browser"))
    (switch-to-buffer eaf-buffer)))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "request_kill_buffer"
 #'eaf-request-kill-buffer)

(defun eaf-request-kill-buffer (kill-buffer-id)
  (catch 'found-match-buffer
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when (derived-mode-p 'eaf-mode)
        (when (string= eaf--buffer-id kill-buffer-id)
          (kill-buffer buffer)
          (message (format "Request kill buffer %s" kill-buffer-id))
          (throw 'found-match-buffer t))))))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "focus_emacs_buffer"
 #'eaf-focus-buffer)

(defun eaf-start-finish ()
  "Call `eaf-open-internal' after receive `start_finish' signal from server process."
  (eaf-open-internal eaf-first-start-url eaf-first-start-app-name eaf-first-start-arguments))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "start_finish"
 #'eaf-start-finish)

(defun eaf-update-buffer-title (bid title)
  (when (> (length title) 0)
    (catch 'find-buffer
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (with-current-buffer buffer
            (when (and
                   (derived-mode-p 'eaf-mode)
                   (equal eaf--buffer-id bid))
              (setq-local eaf--bookmark-title title)
              (rename-buffer (truncate-string-to-width title eaf-title-length))
              (throw 'find-buffer t))))))))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "update_buffer_title"
 #'eaf-update-buffer-title)

(defun eaf-open-buffer-url (url)
  (eaf-open-browser url))

(defun eaf-translate-text (text)
  (when (featurep 'sdcv)
    (sdcv-search-input+ text)))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "open_buffer_url"
 #'eaf-open-buffer-url)

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "translate_text"
 #'eaf-translate-text)

(defun eaf-read-string (interactive-string)
  "Like `read-string' which read an INTERACTIVE-STRING, but return nil if user execute `keyboard-quit' when input."
  (condition-case nil (read-string interactive-string) (quit nil)))

(defun eaf-input-message (input-buffer-id interactive-string callback-type)
  "Handles input message INTERACTIVE-STRING on the Python side given INPUT-BUFFER-ID and CALLBACK-TYPE."
  (let* ((input-message (eaf-read-string interactive-string)))
    (when input-message
      (eaf-call "handle_input_message" input-buffer-id callback-type input-message))))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "input_message"
 #'eaf-input-message)

(defun eaf-send-var-to-python ()
  "Send variables defined in `eaf-var-list' to the Python side."
  (eaf-call "store_emacs_var"
            (string-join (cl-loop for (key . value) in eaf-var-list
                                  collect (format "%s,%s" key value)) ":")))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "get_emacs_var"
 #'eaf-send-var-to-python)

(defun eaf-open-internal (url app-name arguments)
  (let* ((buffer (eaf-create-buffer url app-name))
         (buffer-result
          (with-current-buffer buffer
            (eaf-call "new_buffer"
                            eaf--buffer-id url app-name arguments))))
    (cond ((equal buffer-result "")
           (eaf--display-app-buffer app-name buffer))
          (t
           ;; Kill buffer and show error message from python server.
           (kill-buffer buffer)
           (switch-to-buffer eaf-name)
           (message buffer-result)))))

(defun eaf--markdown-preview-display (buf)
  ;; Split window to show file and previewer.
  (eaf-split-preview-windows
   (buffer-local-value
    'eaf--buffer-url buf))
  (switch-to-buffer buf)
  (other-window +1))

(defun eaf--org-preview-display (buf)
  (let ((url (buffer-local-value
              'eaf--buffer-url buf)))
    ;; Find file first, because `find-file' will trigger `kill-buffer' operation.
    (save-excursion
      (find-file url)
      (org-html-export-to-html)
      (add-hook 'after-save-hook #'eaf--org-preview-monitor-buffer-save nil t)
      (add-hook 'kill-buffer-hook #'eaf--org-preview-monitor-kill nil t))
    ;; Add file name to `eaf-org-file-list' after command `find-file'.
    (unless (member url eaf-org-file-list)
      (push url eaf-org-file-list))
    ;; Split window to show file and previewer.
    (eaf-split-preview-windows url)
    ;; Switch to new buffer if buffer create successful.
    (switch-to-buffer buf)
    (other-window +1)))


;;;###autoload
(defun eaf-open-browser (url &optional arguments)
  "Open EAF browser application given a URL and ARGUMENTS."
  (interactive "MEAF Browser - Enter URL: ")
  ;; Validate URL legitimacy
  (if (and (not (string-prefix-p "/" url))
           (not (string-prefix-p "~" url))
           (string-match "^\\(https?://\\)?[a-z0-9]+\\([-.]\\{1\\}[a-z0-9]+\\)*.+[a-z0-9.]\\{2,5\\}\\(:[0-9]{1,5}\\)?\\(/.*\\)?$" url))
      (progn
        (unless (or (string-prefix-p "http://" url)
                    (string-prefix-p "https://" url))
          (setq url (concat "http://" url)))
        (eaf-open url "browser" arguments))
    (message (format "EAF: %s is an invalid URL." url))))

;;;###autoload
(defalias 'eaf-open-url #'eaf-open-browser)

;;;###autoload
(defun eaf-open-demo ()
  "Open EAF demo screen to verify that EAF is working properly."
  (interactive)
  (eaf-open "eaf-demo" "demo"))

;;;###autoload
(defun eaf-open-camera ()
  "Open EAF camera application."
  (interactive)
  (eaf-open "eaf-camera" "camera"))

;;;###autoload
(defun eaf-open-terminal ()
  "Open EAF terminal application."
  (interactive)
  (eaf-open "eaf-terminal" "terminal"))

;;;###autoload
(defun eaf-open-qutebrowser ()
  "Open EAF Qutebrowser application."
  (interactive)
  (eaf-open "eaf-qutebrowser" "qutebrowser"))

(defun eaf--get-app-for-extension (extension-name)
  (cl-loop for (app . ext) in eaf-app-extensions-alist
           if (member extension-name (symbol-value ext))
           return app))

;;;###autoload
(defun eaf-open (url &optional app-name arguments)
  "Open an EAF application with URL, optional APP-NAME and ARGUMENTS.

When called interactively, URL accepts a file that can be opened by EAF."
  (interactive "FOpen with EAF: ")
  ;; Try to set app-name along with url if app-name is unset.
  (when (and (not app-name) (file-exists-p url))
    (setq url (expand-file-name url))
    (when (featurep 'recentf)
      (recentf-add-file url))
    (let* ((extension-name (file-name-extension url)))
      ;; init app name, url and arguments
      (setq app-name (eaf--get-app-for-extension extension-name))
      (when (equal app-name "markdown-previewer")
        ;; Try get user's github token if `eaf-grip-token' is nil.
        (setq arguments
              (or eaf-grip-token
                  (read-string "Fill your own github token (or set `eaf-grip-token' with token string): "))))
      (when (equal app-name "browser")
        (setq url (concat "file://" url)))))
  (unless arguments (setq arguments ""))
  ;; hooks are only added if not present already...
  (add-hook 'window-size-change-functions #'eaf-monitor-window-size-change)
  (add-hook 'window-configuration-change-hook #'eaf-monitor-configuration-change)
  ;; Now that app-name should hopefully be set
  (if app-name
      ;; Open url with EAF application if app-name is not empty.
      (if (process-live-p eaf-process)
          (let (exists-eaf-buffer)
            ;; Try to open buffer
            (catch 'found-match-buffer
              (dolist (buffer (buffer-list))
                (set-buffer buffer)
                (when (derived-mode-p 'eaf-mode)
                  (when (and (string= eaf--buffer-url url)
                             (string= eaf--buffer-app-name app-name))
                    (setq exists-eaf-buffer buffer)
                    (throw 'found-match-buffer t)))))
            ;; Switch to exists buffer,
            ;; if no match buffer found, call `eaf-open-internal'.
            (if exists-eaf-buffer
                (eaf--display-app-buffer app-name exists-eaf-buffer)
              (eaf-open-internal url app-name arguments)))
        ;; Record user input, and call `eaf-open-internal' after receive `start_finish' signal from server process.
        (setq eaf-first-start-url url)
        (setq eaf-first-start-app-name app-name)
        (setq eaf-first-start-arguments arguments)
        (eaf-start-process)
        (message "Opening %s with EAF-%s..." url app-name))
    ;; Output something to user if app-name is empty string.
    (message (cond
              ((not (or (string-prefix-p "/" url)
                        (string-prefix-p "~" url)))
               "EAF doesn't know how to open %s.")
              ((file-exists-p url)
               "EAF doesn't know how to open %s.")
              (t "EAF: %s does not exist."))
             url)))

(defun eaf--display-app-buffer (app-name buffer)
  (let ((display-fun (or (cdr (assoc app-name
                                     eaf-app-display-function-alist))
                         #'switch-to-buffer)))
    (funcall display-fun buffer)))

(defun eaf-split-preview-windows (url)
  (delete-other-windows)
  (find-file url)
  (split-window-horizontally)
  (other-window +1))

(defun eaf-file-transfer-airshare ()
  "Open EAF Airshare application."
  (interactive)
  (let* ((current-symbol (if (use-region-p)
                             (buffer-substring-no-properties (region-beginning) (region-end))
                           (thing-at-point 'symbol)))
         (input-string (string-trim (read-string (format "EAF Airshare - Info (%s): " current-symbol)))))
    (when (string-empty-p input-string)
      (setq input-string current-symbol))
    (eaf-open input-string "airshare")))

(defun eaf-file-sender-qrcode (file)
  "Open EAF File Sender application.

Select the file FILE to send to your smartphone, a QR code for the corresponding file will appear.

Make sure that your smartphone is connected to the same WiFi network as this computer."
  (interactive "FEAF File Sender - Select File: ")
  (eaf-open file "file-sender"))

(defun eaf-file-sender-qrcode-in-dired ()
  "Open EAF File Transfer application using `eaf-file-sender-qrcode' on
the file at current cursor position in dired."
  (interactive)
  (eaf-file-sender-qrcode (dired-get-filename)))

(defun eaf-file-receiver-qrcode (dir)
  "Open EAF File Receiver application.

Select directory DIR to receive the uploaded file.

Make sure that your smartphone is connected to the same WiFi network as this computer."
  (interactive "DEAF File Receiver - Specify Destination: ")
  (eaf-open dir "file-receiver"))

(defun eaf-file-open-in-dired ()
  "Open html/pdf/image/video files whenever possible with EAF in dired.
Other files will open normally with `dired-find-file' or `dired-find-alternate-file'"
  (interactive)
  (dolist (file (dired-get-marked-files))
    (cond ((eaf--get-app-for-extension
            (file-name-extension file))
           (eaf-open file))
          (eaf-find-alternate-file-in-dired
           (dired-find-alternate-file))
          (t (dired-find-file)))))

;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eaf-get-view-info ()
  (let* ((window-allocation (eaf-get-window-allocation (selected-window)))
         (x (nth 0 window-allocation))
         (y (nth 1 window-allocation))
         (w (nth 2 window-allocation))
         (h (nth 3 window-allocation)))
    (format "%s:%s:%s:%s:%s" eaf--buffer-id x y w h)))

;;;;;;;;;;;;;;;;;;;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: In the code below we should use `save-selected-window' (or even
;; better `with-selected-window') rather than (other-window +1) followed by
;; (other-window -1) since this is not always a no-op.

(advice-add 'scroll-other-window :around #'eaf--scroll-other-window)
(defun eaf--scroll-other-window (orig-fun &optional arg &rest args)
  "When next buffer is `eaf-mode', do `eaf-scroll-up-or-next-page'."
  (other-window +1)
  (if (derived-mode-p 'eaf-mode)
      (progn
        (eaf-call "scroll_buffer" (eaf-get-view-info) "up"
                  (if arg "line" "page"))
        (other-window -1))
    (other-window -1)
    (apply orig-fun arg args)))

(advice-add 'scroll-other-window-down :around #'eaf--scroll-other-window-down)
(defun eaf--scroll-other-window-down (orig-fun &optional arg &rest args)
  "When next buffer is `eaf-mode', do `eaf-scroll-down-or-previous-page'."
  (other-window +1)
  (if (derived-mode-p 'eaf-mode)
      (progn
        (eaf-call "scroll_buffer" (eaf-get-view-info) "down"
                  (if arg "line" "page"))
        (other-window -1))
    (other-window -1)
    (apply orig-fun arg args)))

(advice-add 'watch-other-window-internal :around
            #'eaf--watch-other-window-internal)
(defun eaf--watch-other-window-internal (orig-fun &optional direction line
                                                  &rest args)
  "When next buffer is `eaf-mode', do `eaf-watch-other-window'."
  (other-window +1)
  (if (derived-mode-p 'eaf-mode)
      (progn
        (eaf-call "scroll_buffer" (eaf-get-view-info)
                  (if (string-equal direction "up") "up" "down")
                  (if line "line" "page"))
        (other-window -1))
    (other-window -1)
    (apply orig-fun direction line args)))

(provide 'eaf)

;;; eaf.el ends here
