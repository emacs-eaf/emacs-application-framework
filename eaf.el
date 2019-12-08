;;; eaf.el --- Emacs application framework  -*- lexical-binding: t; -*-

;; Filename: eaf.el
;; Description: Emacs application framework
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.4
;; Last-Updated: 2019-12-07 21:46:44
;;           By: Andy Stewart
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
(defcustom eaf-mode-hook '()
  "Eaf mode hook."
  :type 'hook
  :group 'eaf)

(defvar eaf-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used by `eaf-mode'.")

(define-derived-mode eaf-mode text-mode "EAF"
  "Major mode for Emacs Application Framework."
  ;; Kill all local variables first.
  (kill-all-local-variables)
  ;; Set mode.
  (setq major-mode 'eaf-mode)
  (setq mode-name "EAF")
  ;; Split window combinations proportionally.
  (setq window-combination-resize t)
  (set (make-local-variable 'buffer-id) (eaf-generate-id))
  ;; Load local map.
  (use-local-map eaf-mode-map)
  ;; Fix #110 , make `eaf-monitor-key-event' buffer locally to pre-command-hook of the eaf-mode buffer.
  ;; To fix interactive command run twice because `eaf-monitor-key-event' runs inside minibuffer and can not handle minibuffer quit signal.
  (add-hook 'pre-command-hook #'eaf-monitor-key-event nil t)
  ;; Run eaf-mode hooks.
  (run-hooks 'eaf-mode-hook))

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
  "Name of eaf buffer."
  :type 'string
  :group 'eaf)

(defcustom eaf-python-command "python3"
  "The Python interpreter used to run eaf.py."
  :type 'string
  :group 'eaf)

(defcustom eaf-var-list
  '(
    (eaf-camera-save-path . "~/Downloads")
    (eaf-browser-enable-plugin . "true")
    (eaf-browser-enable-javascript . "true")
    )
  "The alist storing user-defined variables that's shared with EAF Python side.

Try not to modify this alist directly. Use `eaf-setq' to modify instead."
  :type 'cons
  :group 'eaf)

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
    ("M->" . "scroll_to_bottom"))
  "The keybinding of EAF Browser."
  :type 'cons
  :group 'eaf)

(defcustom eaf-browser-key-alias
  '(("C-a" . "<home>")
    ("C-e" . "<end>"))
  "The key alias of EAF Browser."
  :type 'cons
  :group 'eaf)

(defcustom eaf-pdf-viewer-keybinding
  '(("j" . "scroll_up")
    ("k" . "scroll_down")
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
  :type 'cons
  :group 'eaf)

(defcustom eaf-video-player-keybinding
  '(("SPC" . "toggle_play")
    ("h" . "play_backward")
    ("l" . "play_forward"))
  "The keybinding of EAF Video Player."
  :type 'cons
  :group 'eaf)

(defcustom eaf-image-viewer-keybinding
  '(("j" . "load_next_image")
    ("k" . "load_prev_image"))
  "The keybinding of EAF Image Viewer."
  :type 'cons
  :group 'eaf)

(defcustom eaf-terminal-keybinding
  '(("C--" . "zoom_out")
    ("C-=" . "zoom_in"))
  "The keybinding of EAF Terminal."
  :type 'cons
  :group 'eaf)

(defcustom eaf-camera-keybinding
  '(("j" . "take_photo"))
  "The keybinding of EAF Camera."
  :type 'cons
  :group 'eaf)

(defcustom eaf-pdf-extension-list
  '("pdf" "xps" "oxps" "cbz" "epub" "fb2" "fbz" "djvu")
  "The extension list of pdf application."
  :type 'cons
  :group 'eaf)

(defcustom eaf-markdown-extension-list
  '("md")
  "The extension list of markdown previewer application."
  :type 'cons
  :group 'eaf)

(defcustom eaf-image-extension-list
  '("jpg" "jpeg" "png" "bmp")
  "The extension list of image viewer application."
  :type 'cons
  :group 'eaf)

(defcustom eaf-video-extension-list
  '("avi" "rmvb" "ogg" "mp4" "mkv")
  "The extension list of video player application."
  :type 'cons
  :group 'eaf)

(defcustom eaf-browser-extension-list
  '("html")
  "The extension list of browser application."
  :type 'cons
  :group 'eaf)

(defcustom eaf-org-extension-list
  '("org")
  "The extension list of org previewer application."
  :type 'cons
  :group 'eaf)

(defcustom eaf-single-key-list
  '("RET" "DEL" "TAB" "SPC" "<backtab>" "<home>" "<end>" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>")
  "The single key use to send key to EAF app."
  :type 'cons
  :group 'eaf)

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

(defun eaf-call (method &rest args)
  (apply 'dbus-call-method
         :session                   ; use the session (not system) bus
         "com.lazycat.eaf"          ; service name
         "/com/lazycat/eaf"         ; path name
         "com.lazycat.eaf"          ; interface name
         method args))

(defun eaf-get-emacs-xid (frame)
  (frame-parameter frame 'window-id))

(defun eaf-start-process ()
  "Start EAF process if it haven't started yet."
  (interactive)
  (if (process-live-p eaf-process)
      (message "EAF process has started.")
    (setq eaf-process
          (apply 'start-process
                 eaf-name
                 eaf-name
                 eaf-python-command (append (list eaf-python-file) (eaf-get-render-size) (list eaf-http-proxy-host eaf-http-proxy-port))
                 ))
    (set-process-query-on-exit-flag eaf-process nil)
    (set-process-sentinel
     eaf-process
     #'(lambda (process event)
         (message (format "%s %s" process event))
         ))
    (message "EAF process starting...")))

(defun eaf-stop-process ()
  (interactive)
  ;; Kill eaf buffers.
  (let ((current-buf (current-buffer))
        (count 0))
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when (equal major-mode 'eaf-mode)
        (cl-incf count)
        (kill-buffer buffer)))
    ;; Just report to me when eaf buffer exists.
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
  "Kill eaf background python process for debug.
NOTE: this function just use for developer debug.
Don't call this function if you not eaf developer."
  (interactive)
  (if (process-live-p eaf-process)
      ;; Delete eaf server process.
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

If BUF is given it should be the eaf buffer for the command
otherwise it is assumed that the current buffer is the eaf
buffer."
  (with-current-buffer (or buf (current-buffer))
    (let ((this-command cmd))
      (call-interactively cmd))))


(defun eaf-dummy-function (sym key)
  "Define an alias from SYM to a dummy function that acts as a placeholder."
  (defalias sym (lambda nil
                  "This Lisp function is a placeholder, the actual function will be handled on the Python side.

Use `eaf-execute-app-cmd' if you want to execute this command programmatically.
Please ONLY use `eaf-bind-key' to edit EAF keybindings!"
                  (interactive)
                  ;; ensure this is only called from eaf buffer
                  (unless (boundp 'buffer-id)
                    (error "%s command can only be called in eaf buffer" sym))
                  ;; Enable the command to be called by M-x or from lisp code in
                  ;; the case that this command is invoked by key-sequence.
                  (when (and (eq this-command sym)
                             (not (equal (this-command-keys-vector) key)))
                    (eaf-call "execute_function" buffer-id (symbol-name sym))))))

(defun eaf-gen-keybinding-map (keybinding)
  "Configure the eaf-mode-map from KEYBINDING, one of the eaf-*-keybinding variables."
  (setq eaf-mode-map
        (let ((map (make-sparse-keymap)))
          (cl-loop for (key . fun) in keybinding
                   do (eaf-dummy-function (intern fun) key)
                      (define-key map (kbd key) (intern fun))) map)))

(defun eaf-get-app-bindings (app-name)
  (symbol-value
   (cdr (assoc app-name eaf-app-binding-alist))))

(defun eaf-create-buffer (input-content app-name)
  "Create an EAF buffer given INPUT-CONTENT and APP-NAME."
  (eaf-gen-keybinding-map (eaf-get-app-bindings app-name))
  (let* ((file-or-command-name (substring input-content (string-match "[^\/]*\/?$" input-content)))
         (eaf-buffer (generate-new-buffer (truncate-string-to-width file-or-command-name eaf-title-length))))
    (with-current-buffer eaf-buffer
      (eaf-mode))
    eaf-buffer))

(defun eaf-identify-key-in-app (key-command app-name)
  "Given a KEY-COMMAND string, identify whether command is in EAF keybindings based on APP-NAME."
  (rassoc key-command (eaf-get-app-bindings app-name)))

(defun eaf-is-support (url)
  (dbus-call-method
   :session "com.lazycat.eaf"
   "/com/lazycat/eaf"
   "com.lazycat.eaf"
   "is_support"
   url))

(defun eaf-monitor-window-size-change (frame)
  (setq eaf-last-frame-width (frame-pixel-width frame))
  (setq eaf-last-frame-height (frame-pixel-height frame))
  (run-with-timer 1 nil (lambda () (eaf-try-adjust-view-with-frame-size))))

(defun eaf-try-adjust-view-with-frame-size ()
  (when (and (equal (frame-pixel-width) eaf-last-frame-width)
             (equal (frame-pixel-height) eaf-last-frame-height))
    (eaf-monitor-configuration-change)))

(defun eaf-monitor-configuration-change (&rest _)
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
                    (add-to-list 'view-infos (format "%s:%s:%s:%s:%s:%s" buffer-id (eaf-get-emacs-xid frame) x y w h))
                    ))))))
      ;; I don't know how to make emacs send dbus-message with two-dimensional list.
      ;; So i package two-dimensional list in string, then unpack on server side. ;)
      (eaf-call "update_views" (mapconcat 'identity view-infos ","))
      )))

(defun eaf-delete-org-preview-file (org-file)
  (setq org-html-file (concat (file-name-sans-extension org-file) ".html"))
  (when (file-exists-p org-html-file)
    (delete-file org-html-file)
    (message (format "Clean org preview file %s (%s)" org-html-file org-file))
    ))

(defun eaf-org-killed-buffer-clean ()
  (dolist (org-killed-buffer eaf-org-killed-file-list)
    (unless (get-file-buffer org-killed-buffer)
      (setq eaf-org-file-list (remove org-killed-buffer eaf-org-file-list))
      (eaf-delete-org-preview-file org-killed-buffer)
      ))
  (setq eaf-org-killed-file-list nil))

(defun eaf-monitor-buffer-kill ()
  (ignore-errors
    (with-current-buffer (buffer-name)
      (cond ((derived-mode-p 'org-mode)
             ;; NOTE:
             ;; Because save org buffer will trigger `kill-buffer' action,
             ;; but org buffer still live after do `kill-buffer' action.
             ;; So i run a timer to check org buffer is live after `kill-buffer' aciton.
             (when (member (buffer-file-name) eaf-org-file-list)
               (unless (member (buffer-file-name) eaf-org-killed-file-list)
                 (push (buffer-file-name) eaf-org-killed-file-list))
               (run-with-timer 1 nil (lambda () (eaf-org-killed-buffer-clean)))
               ))
            ((derived-mode-p 'eaf-mode)
             (eaf-call "kill_buffer" buffer-id)
             (message (format "Kill %s" buffer-id)))
            ))))

(defun eaf-monitor-buffer-save ()
  (ignore-errors
    (with-current-buffer (buffer-name)
      (cond ((and
              (derived-mode-p 'org-mode)
              (member (buffer-file-name) eaf-org-file-list))
             (org-html-export-to-html)
             (eaf-call "update_buffer_with_url" "app.orgpreviewer.buffer" (buffer-file-name) "")
             (message (format "export %s to html" (buffer-file-name))))))))

(defun eaf-monitor-key-event ()
  "Monitor key events during EAF process."
  (ignore-errors
    (let* ((key (this-command-keys-vector))
           (key-command (symbol-name (key-binding key)))
           (key-desc (key-description key)))

      ;; Uncomment for debug.
      ;; (message (format "!!!!! %s %s %s %s %s" key key-command key-desc buffer-app-name))

      (cond
        ;; Fix #51 , don't handle F11 to make emacs toggle frame fullscreen status successfully.
        ((equal key-desc "<f11>")
         t)
        ;; Call function on the Python side if matched key in the keybinding.
        ((eaf-identify-key-in-app key-command buffer-app-name)
         (eaf-call "execute_function" buffer-id
                   (cdr (assoc key-desc (eaf-get-app-bindings buffer-app-name)))))
        ;; Send key to Python side if key-command is single character key.
        ((or (equal key-command "self-insert-command")
             (equal key-command "completion-select-if-within-overlay")
             (equal key-command "nil")
             (member key-desc eaf-single-key-list))
         (eaf-call "send_key" buffer-id key-desc))
        (t
         nil)))))

(defun eaf-set (sym val)
  "Similar to `set', but store SYM with VAL in the EAF Python side.

For convenience, use the Lisp macro `eaf-setq' instead."
  (when (symbol-name sym)
    (map-put eaf-var-list sym val)))

(defmacro eaf-setq (var val)
  "Similar to `setq', but store VAR with VAL in the EAF Python side.

Use it as (eaf-setq var val)"
  `(eaf-set ',var ,val))

(defmacro eaf-bind-key (var key eaf-app-keybinding)
  "Similar to `bind-key', but store VAR with KEY in EAF-APP-KEYBINDING list.
This is used to bind key to EAF Python applications.

Use it as (eaf-bind-key var key eaf-app-keybinding)"
  `(map-put ,eaf-app-keybinding ,key (symbol-name ',var)))

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
                         (> mouse-press-x x)
                         (< mouse-press-x (+ x w))
                         (> mouse-press-y y)
                         (< mouse-press-y (+ y h)))
                    (select-window window)
                    (throw 'find-window t))))))))))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "message_to_emacs"
 'message)

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "create_new_browser_buffer"
 'eaf-create-new-browser-buffer)

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "set_emacs_var"
 'eaf-set-emacs-var)

(defun eaf-set-emacs-var (var-name var-value)
  (set (intern var-name) var-value))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "eval_in_emacs"
 'eaf-eval-in-emacs)

(defun eaf-eval-in-emacs (elisp-code-string)
  (eval (read elisp-code-string)))

(defun eaf-create-new-browser-buffer (new-window-buffer-id)
  (let ((eaf-buffer (generate-new-buffer (concat "Browser Popup Window " new-window-buffer-id))))
    (with-current-buffer eaf-buffer
      (eaf-mode)
      (set (make-local-variable 'buffer-id) new-window-buffer-id)
      (set (make-local-variable 'buffer-url) "")
      (set (make-local-variable 'buffer-app-name) "browser"))
    (switch-to-buffer eaf-buffer)))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "request_kill_buffer"
 'eaf-request-kill-buffer)

(defun eaf-request-kill-buffer (kill-buffer-id)
  (catch 'found-match-buffer
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when (equal major-mode 'eaf-mode)
        (when (string= buffer-id kill-buffer-id)
          (kill-buffer buffer)
          (message (format "Request kill buffer %s" kill-buffer-id))
          (throw 'found-match-buffer t))))))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "focus_emacs_buffer"
 'eaf-focus-buffer)

(defun eaf-start-finish ()
  "Call `eaf-open-internal' after receive `start_finish' signal from server process."
  (eaf-open-internal eaf-first-start-url eaf-first-start-app-name eaf-first-start-arguments))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "start_finish"
 'eaf-start-finish)

(defun eaf-update-buffer-title (bid title)
  (when (> (length title) 0)
    (catch 'find-buffer
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (with-current-buffer buffer
            (when (and
                   (derived-mode-p 'eaf-mode)
                   (equal buffer-id bid))
              (rename-buffer (truncate-string-to-width title eaf-title-length))
              (throw 'find-buffer t)
              )))))))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "update_buffer_title"
 'eaf-update-buffer-title)

(defun eaf-open-buffer-url (url)
  (eaf-open-browser url))

(defun eaf-translate-text (text)
  (when (featurep 'sdcv)
    (sdcv-search-input+ text)))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "open_buffer_url"
 'eaf-open-buffer-url)

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "translate_text"
 'eaf-translate-text)

(defun eaf-read-string (interactive-string)
  "Like `read-string', but return nil if user execute `keyboard-quit' when input."
  (condition-case nil (read-string interactive-string) (quit nil)))

(defun eaf-input-message (buffer-id interactive-string callback-type)
  (let* ((input-message (eaf-read-string interactive-string)))
    (when input-message
      (eaf-call "handle_input_message" buffer-id callback-type input-message)
      )))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "input_message"
 'eaf-input-message)

(defun eaf-send-var-to-python ()
  "Send variables defined in `eaf-var-list' to the Python side."
  (eaf-call "store_emacs_var"
            (string-join (cl-loop for (key . value) in eaf-var-list
                                  collect (format "%s,%s" key value)) ":")))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "get_emacs_var"
 'eaf-send-var-to-python)

(add-hook 'window-size-change-functions #'eaf-monitor-window-size-change)
(add-hook 'window-configuration-change-hook #'eaf-monitor-configuration-change)
(add-hook 'kill-buffer-hook #'eaf-monitor-buffer-kill)
(add-hook 'after-save-hook #'eaf-monitor-buffer-save)

(defun eaf-open-internal (url app-name arguments)
  (let* ((buffer (eaf-create-buffer url app-name))
         buffer-result)
    (with-current-buffer buffer
      (setq buffer-result (eaf-call "new_buffer" buffer-id url app-name arguments)))
    (if (equal buffer-result "")
        (progn
          ;; Switch to new buffer if buffer create successful.
          (switch-to-buffer buffer)
          (set (make-local-variable 'buffer-url) url)
          (set (make-local-variable 'buffer-app-name) app-name)
          ;; Focus to file window if is previewer application.
          (when (or (string= app-name "markdown-previewer")
                    (string= app-name "org-previewer"))
            (other-window +1)))
      ;; Kill buffer and show error message from python server.
      (kill-buffer buffer)
      (switch-to-buffer eaf-name)
      (message buffer-result))))

(defun eaf-open-browser (url &optional arguments)
  "Open EAF browser application given a URL and ARGUMENTS."
  (interactive "MEAF Browser - Enter URL: ")
  ;; Validate URL legitimacy
  (if (and (not (string-prefix-p "/" url))
           (not (string-prefix-p "~" url))
           (string-match "^\\(https?:\/\/\\)?[a-z0-9]+\\([\-\.]\\{1\\}[a-z0-9]+\\)*\.+[a-z0-9\.]\\{2,5\\}\\(:[0-9]{1,5}\\)?\\(\/.*\\)?$" url))
      (progn
        (unless (or (string-prefix-p "http://" url)
                    (string-prefix-p "https://" url))
          (setq url (concat "http://" url)))
        (eaf-open url "browser" arguments))
    (message (format "EAF: %s is an invalid URL." url))))

(defalias 'eaf-open-url 'eaf-open-browser)

(defun eaf-open-demo ()
  "Open EAF demo screen to verify that EAF is working properly."
  (interactive)
  (eaf-open "eaf-demo" "demo"))

(defun eaf-open-camera ()
  "Open EAF camera application."
  (interactive)
  (eaf-open "eaf-camera" "camera"))

(defun eaf-open-terminal ()
  "Open EAF terminal application."
  (interactive)
  (eaf-open "eaf-terminal" "terminal"))

(defun eaf-open-qutebrowser ()
  "Open EAF Qutebrowser application."
  (interactive)
  (eaf-open "eaf-qutebrowser" "qutebrowser"))

(defun eaf-open (url &optional app-name arguments)
  "Open an EAF application with URL, optional APP-NAME and ARGUMENTS.

When called interactively, URL accepts a file that can be opened by EAF."
  (interactive "FOpen with EAF: ")
  ;; Try to set app-name along with url if app-name is unset.
  (when (and (not app-name) (file-exists-p url))
    (setq url (expand-file-name url))
    (setq extension-name (file-name-extension url))
    (cond ((member extension-name eaf-pdf-extension-list)
           (setq app-name "pdf-viewer"))
          ((member extension-name eaf-markdown-extension-list)
           ;; Try get user's github token if `eaf-grip-token' is nil.
           (if eaf-grip-token
               (setq arguments eaf-grip-token)
             (setq arguments (read-string "Fill your own github token (or set `eaf-grip-token' with token string): ")))
           ;; Split window to show file and previewer.
           (eaf-split-preview-windows)
           (setq app-name "markdown-previewer"))
          ((member extension-name eaf-image-extension-list)
           (setq app-name "image-viewer"))
          ((member extension-name eaf-video-extension-list)
           (setq app-name "video-player"))
          ((member extension-name eaf-browser-extension-list)
           (setq url (concat "file://" url))
           (setq app-name "browser"))
          ((member extension-name eaf-org-extension-list)
           ;; Find file first, because `find-file' will trigger `kill-buffer' operation.
           (save-excursion
             (find-file url)
             (with-current-buffer (buffer-name)
               (org-html-export-to-html)))
           ;; Add file name to `eaf-org-file-list' after command `find-file'.

           (unless (member url eaf-org-file-list)
             (push url eaf-org-file-list))
           ;; Split window to show file and previewer.
           (eaf-split-preview-windows)
           (setq app-name "org-previewer"))))
  (unless arguments (setq arguments ""))
  ;; Now that app-name should hopefully be set
  (if app-name
      ;; Open url with eaf application if app-name is not empty.
      (if (process-live-p eaf-process)
          (let (exists-eaf-buffer)
            ;; Try to open buffer
            (catch 'found-match-buffer
              (dolist (buffer (buffer-list))
                (set-buffer buffer)
                (when (equal major-mode 'eaf-mode)
                  (when (and (string= buffer-url url)
                             (string= buffer-app-name app-name))
                    (setq exists-eaf-buffer buffer)
                    (throw 'found-match-buffer t)))))
            ;; Switch to exists buffer,
            ;; if no match buffer found, call `eaf-open-internal'.
            (if exists-eaf-buffer
                (switch-to-buffer exists-eaf-buffer)
              (eaf-open-internal url app-name arguments)))
        ;; Record user input, and call `eaf-open-internal' after receive `start_finish' signal from server process.
        (setq eaf-first-start-url url)
        (setq eaf-first-start-app-name app-name)
        (setq eaf-first-start-arguments arguments)
        (eaf-start-process)
        (message (format "Opening %s with EAF-%s..." url app-name)))
    ;; Output something to user if app-name is empty string.
    (if (or (string-prefix-p "/" url)
            (string-prefix-p "~" url))
        (if (not (file-exists-p url))
            (message (format "EAF: %s does not exist." url))
          (message (format "EAF doesn't know how to open %s." url)))
      (message (format "EAF doesn't know how to open %s." url)))))

(defun eaf-split-preview-windows ()
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
    (cond ((member (file-name-extension file)
                   (append eaf-pdf-extension-list
                           eaf-markdown-extension-list
                           eaf-image-extension-list
                           eaf-video-extension-list
                           eaf-browser-extension-list
                           eaf-org-extension-list
                           ))
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
    (format "%s:%s:%s:%s:%s" buffer-id x y w h)))

;;;;;;;;;;;;;;;;;;;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice scroll-other-window (around eaf-scroll-up-or-next-page activate)
  "When next buffer is `eaf-mode', do `eaf-scroll-up-or-next-page'."
  (other-window +1)
  (if (derived-mode-p 'eaf-mode)
      (let ((arg (ad-get-arg 0)))
        (if (null arg)
            (eaf-call "scroll_buffer" (eaf-get-view-info) "up" "page")
          (eaf-call "scroll_buffer" (eaf-get-view-info) "up" "line"))
        (other-window -1))
    (other-window -1)
    ad-do-it))

(defadvice scroll-other-window-down (around eaf-scroll-down-or-previous-page activate)
  "When next buffer is `eaf-mode', do `eaf-scroll-down-or-previous-page'."
  (other-window +1)
  (if (derived-mode-p 'eaf-mode)
      (let ((arg (ad-get-arg 0)))
        (if (null arg)
            (eaf-call "scroll_buffer" (eaf-get-view-info) "down" "page")
          (eaf-call "scroll_buffer" (eaf-get-view-info) "down" "line"))
        (other-window -1))
    (other-window -1)
    ad-do-it))

(defadvice watch-other-window-internal (around eaf-watch-other-window activate)
  "When next buffer is `eaf-mode', do `eaf-watch-other-window'."
  (other-window +1)
  (if (derived-mode-p 'eaf-mode)
      (let ((direction (ad-get-arg 0))
            (line (ad-get-arg 1)))
        (if (string-equal direction "up")
            (if (null line)
                (eaf-call "scroll_buffer" (eaf-get-view-info) "up" "page")
              (eaf-call "scroll_buffer" (eaf-get-view-info) "up" "line")
              )
          (if (null line)
              (eaf-call "scroll_buffer" (eaf-get-view-info) "down" "page")
            (eaf-call "scroll_buffer" (eaf-get-view-info) "down" "line")
            ))
        (other-window -1))
    (other-window -1)
    ad-do-it))

(provide 'eaf)

;;; eaf.el ends here
