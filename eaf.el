;; eaf.el --- Emacs application framework  -*- lexical-binding: t; -*-

;; Filename: eaf.el
;; Description: Emacs application framework
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: Sat Feb 22 03:07:37 2020 (-0500)
;;           By: Mingde (Matthew) Zeng
;; URL: http://www.emacswiki.org/emacs/download/eaf.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; Please check README
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
;; Emacs Application Framework
;;

;;; Installation:
;;
;; Please check README
;;

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
(require 'map)
(require 'bookmark)

;;; Code:

(defgroup eaf nil
  "Emacs Application Framework."
  :group 'applications)

(defcustom eaf-mode-hook '()
  "EAF mode hook."
  :type 'hook)

(defvar eaf-mode-map*
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-h m") #'eaf-describe-bindings)
    (define-key map [remap describe-bindings] #'eaf-describe-bindings)
    (define-key map (kbd "C-c b") #'eaf-open-bookmark)
    (define-key map (kbd "C-c e") #'eaf-open-external)
    (define-key map (kbd "M-/") #'eaf-get-path-or-url)
    (define-key map (kbd "M-[") #'eaf-share-path-or-url)
    (define-key map (vector 'remap #'keyboard-quit) #'eaf-keyboard-quit)
    (define-key map (vector 'remap #'self-insert-command) #'eaf-send-key)
    (dolist (single-key '("RET" "DEL" "TAB" "SPC" "<backtab>" "<home>" "<end>" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>"))
      (define-key map (kbd single-key) #'eaf-send-key))
    map)
  "Keymap for default bindings available in all apps.")

(defvar eaf-mode-map nil
  "Keymap used by `eaf-mode'.

Don't modify this map directly.  To bind keys for all apps use
`eaf-mode-map*' and to bind keys for individual apps use
`eaf-bind-key'.")

(defvar eaf-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") #'eaf-edit-buffer-switch-to-org-mode)
    (define-key map (kbd "C-c C-k") #'eaf-edit-buffer-cancel)
    (define-key map (kbd "C-c C-c") #'eaf-edit-buffer-confirm)
    map))

(define-derived-mode eaf-edit-mode text-mode "EAF/edit"
  "The major mode to edit focus text input.")

(defun eaf-describe-bindings ()
  "Like `describe-bindings' for EAF buffers."
  (interactive)
  (let ((emulation-mode-map-alists nil)
        (eaf-mode-map (current-local-map)))
    (call-interactively 'describe-mode)))

(defvar-local eaf--buffer-id nil
  "Internal id used by EAF app.")

(defvar-local eaf--buffer-url nil
  "The EAF buffer url.")

(defvar-local eaf--buffer-app-name nil
  "The EAF buffer app-name.")

(define-derived-mode eaf-mode fundamental-mode "EAF"
  "Major mode for Emacs Application Framework buffers.

This mode is used by all apps. The mode map `eaf-mode-map' is
created dynamically for each app and should not be changed
manually. See `eaf-bind-key' for customization of app bindings.

Within EAF buffers the variable `eaf--buffer-app-name' holds the
name of the current app. Each app can setup app hooks by using
`eaf-<app-name>-hook'. This hook runs after the app buffer has
been initialized."
  ;; Split window combinations proportionally.
  (setq-local window-combination-resize t)
  (set (make-local-variable 'eaf--buffer-id) (eaf--generate-id))
  (setq-local bookmark-make-record-function #'eaf--bookmark-make-record)
  ;; Copy default value in case user already has bindings there
  (setq-local emulation-mode-map-alists (default-value 'emulation-mode-map-alists))
  ;; Disable cursor in eaf buffer.
  (setq-local cursor-type nil)
  (push (list (cons t eaf-mode-map))
        emulation-mode-map-alists)
  (add-hook 'kill-buffer-hook #'eaf--monitor-buffer-kill nil t)
  (add-hook 'kill-emacs-hook #'eaf--monitor-emacs-kill))

(defvar eaf-python-file (expand-file-name "eaf.py" (file-name-directory load-file-name)))

(defvar eaf-process nil)

(defvar eaf--first-start-url nil)

(defvar eaf--first-start-app-name nil)

(defvar eaf--first-start-arguments nil)

(defvar eaf-org-file-list '())

(defvar eaf-org-killed-file-list '())

(defvar eaf-last-frame-width 0)

(defvar eaf-last-frame-height 0)

(defvar eaf-grip-token nil)

(defvar eaf-find-alternate-file-in-dired nil
  "If non-nil, calling `eaf-open-this-from-dired' determines file types to open.

EAF unrecognizable files will be opened by `dired-find-alternate-file' normally.
Otherwise they will be opened normally with `dired-find-file'.")

(defcustom eaf-browser-search-engines `(("google" . "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")
                                        ("duckduckgo" . "https://duckduckgo.com/?q=%s"))
  "The default search engines offered by EAF.

Each element has the form (NAME . URL).
 NAME is a search engine name, as a string.
 URL pecifies the url format for the search engine.
  It should have a %s as placeholder for search string."
  :type '(alist :key-type (string :tag "Search engine name")
                :value-type (string :tag "Search engine url")))

(defcustom eaf-browser-default-search-engine "google"
  "The default search engine used by `eaf-open-browser' and `eaf-search-it'.

It must defined at `eaf-browser-search-engines'."
  :type 'string)

(defcustom eaf-name "*eaf*"
  "Name of EAF buffer."
  :type 'string)

(defcustom eaf-python-command "python3"
  "The Python interpreter used to run eaf.py."
  :type 'string)

(defcustom eaf-config-location (expand-file-name (locate-user-emacs-file "eaf/"))
  "Directory where eaf will store configuration files."
  :type 'directory)

(defcustom eaf-var-list
  '(
    (eaf-camera-save-path . "~/Downloads")
    (eaf-browser-enable-plugin . "true")
    (eaf-browser-enable-javascript . "true")
    (eaf-browser-remember-history . "true")
    (eaf-browser-default-zoom . "1.0")
    (eaf-browser-blank-page-url . "https://www.google.com")
    (eaf-browser-dark-mode . "false")
    (eaf-browser-scroll-behavior . "auto")
    (eaf-browser-download-path . "~/Downloads")
    (eaf-browser-aria2-proxy-host . "")
    (eaf-browser-aria2-proxy-port . "")
    (eaf-marker-letters . "ASDFHJKLWEOPCNM")
    )
  "The alist storing user-defined variables that's shared with EAF Python side.

Try not to modify this alist directly.  Use `eaf-setq' to modify instead."
  :type 'cons)

(defcustom eaf-browser-keybinding
  '(("C--" . "zoom_out")
    ("C-=" . "zoom_in")
    ("C-0" . "zoom_reset")
    ("C-s" . "search_text_forward")
    ("C-r" . "search_text_backward")
    ("C-n" . "scroll_up")
    ("C-p" . "scroll_down")
    ("C-f" . "scroll_right")
    ("C-b" . "scroll_left")
    ("C-v" . "scroll_up_page")
    ("C-y" . "yank_text")
    ("C-w" . "kill_text")
    ("M-e" . "edit_focus_text")
    ("M-s" . "open_link")
    ("M-S" . "open_link_new_buffer")
    ("M-d" . "open_link_background_buffer")
    ("C-/" . "undo_action")
    ("M-_" . "redo_action")
    ("M-w" . "copy_text")
    ("M-f" . "history_forward")
    ("M-b" . "history_backward")
    ("M-q" . "clear_all_cookies")
    ("C-M-q" . "clear_history")
    ("M-v" . "scroll_down_page")
    ("M-<" . "scroll_to_begin")
    ("M->" . "scroll_to_bottom")
    ("M-t" . "new_blank_page")
    ("SPC" . "insert_or_scroll_up_page")
    ("J" . "insert_or_goto_left_tab")
    ("K" . "insert_or_goto_right_tab")
    ("j" . "insert_or_scroll_up")
    ("k" . "insert_or_scroll_down")
    ("h" . "insert_or_scroll_left")
    ("l" . "insert_or_scroll_right")
    ("f" . "insert_or_open_link")
    ("F" . "insert_or_open_link_new_buffer")
    ("D" . "insert_or_open_link_background_buffer")
    ("d" . "insert_or_scroll_up_page")
    ("u" . "insert_or_scroll_down_page")
    ("H" . "insert_or_history_backward")
    ("L" . "insert_or_history_forward")
    ("t" . "insert_or_new_blank_page")
    ("T" . "insert_or_recover_prev_close_page")
    ("i" . "insert_or_open_download_manage_page")
    ("r" . "insert_or_refresh_page")
    ("g" . "insert_or_scroll_to_begin")
    ("x" . "insert_or_close_buffer")
    ("G" . "insert_or_scroll_to_bottom")
    ("-" . "insert_or_zoom_out")
    ("=" . "insert_or_zoom_in")
    ("0" . "insert_or_zoom_reset")
    ("m" . "insert_or_save_as_bookmark")
    ("o" . "insert_or_open_url")
    ("C-a" . "select_all_or_input_text")
    ("M-u" . "clear_focus")
    ("M-i" . "open_download_manage_page")
    ("M-p" . "eval_js_file")
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
    ("C-n" . "scroll_up")
    ("C-p" . "scroll_down")
    ("SPC" . "scroll_up_page")
    ("b" . "scroll_down_page")
    ("C-v" . "scroll_up_page")
    ("M-v" . "scroll_down_page")
    ("t" . "toggle_read_mode")
    ("." . "scroll_to_home")
    ("," . "scroll_to_end")
    ("0" . "zoom_reset")
    ("=" . "zoom_in")
    ("-" . "zoom_out")
    ("g" . "jump_to_page")
    ("p" . "jump_to_percent")
    ("[" . "save_current_pos")
    ("]" . "jump_to_saved_pos")
    ("i" . "toggle_inverted_mode")
    ("m" . "toggle_mark_link")
    ("f" . "jump_to_link")
    ("M-w" . "copy_select")
    ("C-s" . "search_text_forward")
    ("C-r" . "search_text_backward")
    ("h" . "add_annot_highlight")
    ("u" . "add_annot_underline")
    ("s" . "add_annot_squiggly")
    ("d" . "add_annot_strikeout_or_delete_annot")
    ("e" . "add_annot_text_or_edit_annot"))
  "The keybinding of EAF PDF Viewer."
  :type 'cons)

(defcustom eaf-video-player-keybinding
  '(("SPC" . "toggle_play")
    ("h" . "play_backward")
    ("l" . "play_forward"))
  "The keybinding of EAF Video Player."
  :type 'cons)

(defcustom eaf-image-viewer-keybinding
  '(("n" . "load_next_image")
    ("p" . "load_prev_image")
    ("SPC" . "load_prev_image")
    ("-" . "zoom_out")
    ("=" . "zoom_in")
    ("0" . "zoom_reset")
    ("j" . "scroll_up")
    ("k" . "scroll_down")
    ("h" . "scroll_left")
    ("l" . "scroll_right")
    ("," . "scroll_up_page")
    ("." . "scroll_down_page")
    ("<" . "scroll_to_begin")
    (">" . "scroll_to_bottom"))
  "The keybinding of EAF Image Viewer."
  :type 'cons)

(defcustom eaf-terminal-keybinding
  '(("C--" . "zoom_out")
    ("C-=" . "zoom_in")
    ("C-0" . "zoom_reset")
    ("C-S-c" . "copy_text")
    ("C-S-v" . "yank_text")
    ("C-c C-c" . "eaf-send-cancel-key-sequence")
    ("C-a" . "eaf-send-key-sequence")
    ("C-e" . "eaf-send-key-sequence")
    ("C-d" . "eaf-send-key-sequence")
    ("C-n" . "eaf-send-key-sequence")
    ("C-p" . "eaf-send-key-sequence")
    ("C-r" . "eaf-send-key-sequence")
    ("C-y" . "eaf-send-key-sequence")
    ("C-k" . "eaf-send-key-sequence")
    ("M-f" . "eaf-send-key-sequence")
    ("M-b" . "eaf-send-key-sequence")
    ("M-d" . "eaf-send-key-sequence"))
  "The keybinding of EAF Terminal."
  :type 'cons)

(defcustom eaf-camera-keybinding
  '(("j" . "take_photo"))
  "The keybinding of EAF Camera."
  :type 'cons)

(defcustom eaf-rss-reader-keybinding
  '(("a" . "add_subscription")
    ("d" . "delete_subscription")
    ("n" . "next_subscription")
    ("p" . "prev_subscription")
    ("N" . "last_subscription")
    ("P" . "first_subscription")
    ("j" . "next_article")
    ("k" . "prev_article")
    ("J" . "last_article")
    ("K" . "first_article")
    ("," . "scroll_up")
    ("." . "scroll_down")
    ("SPC" . "scroll_up_page")
    ("b" . "scroll_down_page")
    ("<" . "scroll_to_begin")
    (">" . "scroll_to_bottom")
    ("M-s" . "open_link")
    ("M-S" . "open_link_new_buffer")
    ("C-s" . "search_text_forward")
    ("C-r" . "search_text_backward")
    ("C-n" . "scroll_up")
    ("C-p" . "scroll_down")
    ("C-v" . "scroll_up_page")
    ("M-v" . "scroll_down_page")
    ("M-<" . "scroll_to_begin")
    ("M->" . "scroll_to_bottom"))
  "The keybinding of EAF RSS Reader."
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
  '("jpg" "jpeg" "png" "bmp" "gif" "svg" "webp")
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

(defcustom eaf-mua-get-html
  '(("^gnus-" . eaf-gnus-get-html)
    ("^mu4e-" . eaf-mu4e-get-html)
    ("^notmuch-" . eaf-notmuch-get-html))
  "An alist regex mapping a MUA `major-mode' to a function to retrieve HTML part of a mail."
  :type 'alist)


(defcustom eaf-proxy-host ""
  "Proxy Host used by EAF Browser."
  :type 'string)

(defcustom eaf-proxy-port ""
  "Proxy Port used by EAF Browser."
  :type 'string)

(defcustom eaf-proxy-type ""
  "Proxy Type used by EAF Browser.  The value is either \"http\" or \"socks5\"."
  :type 'string)

(defvar eaf-app-binding-alist
  '(("browser" . eaf-browser-keybinding)
    ("pdf-viewer" . eaf-pdf-viewer-keybinding)
    ("video-player" . eaf-video-player-keybinding)
    ("image-viewer" . eaf-image-viewer-keybinding)
    ("camera" . eaf-camera-keybinding)
    ("terminal" . eaf-terminal-keybinding)
    ("markdown-previewer" . eaf-browser-keybinding)
    ("org-previewer" . eaf-browser-keybinding)
    ("rss-reader" . eaf-rss-reader-keybinding))
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

(defvar eaf--monitor-configuration-p t
  "When this variable is non-nil, `eaf-monitor-configuration-change' execute.
This variable use to open buffer in backend and avoid graphics blink.

EAF call python method `new_buffer' to create EAF application buffer.
EAF call python method `update_views' to create EAF application view.

Python process only create application view when Emacs window or buffer state change.")

(defvar eaf-buffer-title-format "%s")

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
           (eaf-open-browser (cdr (assq 'filename bookmark))))
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
             (message "This command can only be called in an EAF buffer!"))
           ;; create new one for current buffer with provided name
           (bookmark-set cand)))))

(defun eaf-open-external ()
  "Command to open current path or url with external application."
  (interactive)
  (let ((path-or-url (eaf-get-path-or-url)))
    (cond ((string-equal system-type "windows-nt")
           (w32-shell-execute "open" path-or-url))
          ((string-equal system-type "darwin")
           (concat "open " (shell-quote-argument path-or-url)))
          ((string-equal system-type "gnu/linux")
           (let ((process-connection-type nil))
             (start-process "" nil "xdg-open" path-or-url))))))

(defun eaf-call (method &rest args)
  "Call EAF Python process using `dbus-call-method' with METHOD and ARGS."
  (apply #'dbus-call-method
         :session                   ; use the session (not system) bus
         "com.lazycat.eaf"          ; service name
         "/com/lazycat/eaf"         ; path name
         "com.lazycat.eaf"          ; interface name
         method args))

(defun eaf-get-emacs-xid (frame)
  (frame-parameter frame 'window-id))

(defun eaf-serialization-var-list ()
  (string-join (cl-loop for (key . value) in eaf-var-list
                        collect (format "%sᛝ%s" key value)) "ᛡ"))

(defun eaf-start-process ()
  "Start EAF process if it isn't started."
  (cond
   ((or (eq eaf--first-start-url nil) (eq eaf--first-start-app-name nil) (eq eaf--first-start-arguments nil))
    (message "[EAF] Please initiate EAF with eaf-open-... functions only"))
   ((process-live-p eaf-process)
    (message "[EAF] Process is already running."))
   (t
    (setq eaf-process
          (apply #'start-process
                 eaf-name
                 eaf-name
                 eaf-python-command (append (list eaf-python-file) (eaf-get-render-size)
                                            (list eaf-proxy-host eaf-proxy-port eaf-proxy-type eaf-config-location)
                                            (list (eaf-serialization-var-list)))))
    (set-process-query-on-exit-flag eaf-process nil)
    (set-process-sentinel
     eaf-process
     #'(lambda (process event)
         (when (string-prefix-p "exited abnormally with code" event)
           (switch-to-buffer eaf-name))
         (message "[EAF] %s %s" process (replace-regexp-in-string "\n$" "" event))))
    (message "[EAF] Process starting..."))))

(defun eaf-stop-process (&optional restart)
  "Stop EAF process and kill all EAF buffers.

When RESTART is non-nil, cached URL and app-name will not be cleared."
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
        (message "[EAF] Killed %s EAF buffer%s" count (if (> count 1) "s" ""))))
  (when (get-buffer eaf-name)
    (kill-buffer eaf-name))

  ;; Clear cached URL and app-name, avoid next start process to open buffer.
  (unless restart
    (setq eaf--first-start-url nil)
    (setq eaf--first-start-app-name nil)
    (setq eaf--first-start-arguments nil))

  ;; Clean `eaf-org-file-list' and `eaf-org-killed-file-list'.
  (dolist (org-file-name eaf-org-file-list)
    (eaf--delete-org-preview-file org-file-name))
  (setq eaf-org-file-list nil)
  (setq eaf-org-killed-file-list nil)

  ;; Kill process after kill buffer, make application can save session data.
  (eaf--kill-python-process))

(defun eaf--kill-python-process ()
  "Kill EAF background python process."
  (interactive)
  (if (process-live-p eaf-process)
      ;; Delete EAF server process.
      (progn
        (delete-process eaf-process)
        (message "[EAF] Process terminated."))
    (message "[EAF] Process already terminated.")))

(defun eaf-restart-process ()
  "Stop and restart EAF process."
  (interactive)
  (eaf-stop-process t)
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
  (let* ((window-edges (window-pixel-edges window))
         (x (nth 0 window-edges))
         (y (+ (nth 1 window-edges) (window-header-line-height window)))
         (w (- (nth 2 window-edges) x))
         (h (- (nth 3 window-edges) (window-mode-line-height window) y)))
    (list x y w h)))

(defun eaf--generate-id ()
  "Randomly generate a seven digit id used for EAF buffers."
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
  "Get the current file path or web URL.

When called interactively, copy to ‘kill-ring’."
  (interactive)
  (if (derived-mode-p 'eaf-mode)
      (if (called-interactively-p 'any)
          (message (kill-new (eaf-call "call_function" eaf--buffer-id "get_url")))
        (eaf-call "call_function" eaf--buffer-id "get_url"))
    (message "This command can only be called in an EAF buffer!")))

(defun eaf-share-path-or-url ()
  "Share the current file path or web URL as QRCode."
  (interactive)
  (eaf-open (eaf-get-path-or-url) "airshare"))

(defun eaf--make-proxy-function (fun)
  "Define elisp command which can call python function string FUN."
  (let ((sym (intern (format "eaf-proxy-%s" fun))))
    (unless (fboundp sym)
      (defalias sym
        (lambda nil
          (interactive)
          ;; Ensure this is only called from EAF buffer
          (if (derived-mode-p 'eaf-mode)
              (eaf-call "execute_function" eaf--buffer-id fun (key-description (this-command-keys-vector)))
            (message "%s command can only be called in an EAF buffer!" sym)))
        (format
         "Proxy function to call \"%s\" on the Python side.

Use `eaf-execute-app-cmd' if you want to execute this command programmatically.
Please ONLY use `eaf-bind-key' and use the unprefixed command name (\"%s\")
to edit EAF keybindings!" fun fun)))
    sym))

(defun eaf--gen-keybinding-map (keybinding)
  "Configure the `eaf-mode-map' from KEYBINDING, one of the eaf-.*-keybinding variables."
  (setq eaf-mode-map
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map eaf-mode-map*)
          (cl-loop for (key . fun) in keybinding
                   do (define-key map (kbd key)
                        (cond ((symbolp fun)
                               fun)
                              ((member fun (list "eaf-send-key-sequence" "eaf-send-cancel-key-sequence"))
                               (intern fun))
                              (t
                               (eaf--make-proxy-function fun))))
                   finally return map))))

(defun eaf--get-app-bindings (app-name)
  (symbol-value
   (cdr (assoc app-name eaf-app-binding-alist))))

(defun eaf--create-buffer (url app-name)
  "Create an EAF buffer given URL and APP-NAME."
  (eaf--gen-keybinding-map (eaf--get-app-bindings app-name))
  (let* ((eaf-buffer-name (if (equal (file-name-nondirectory url) "")
                              url
                            (file-name-nondirectory url)))
         (eaf-buffer (generate-new-buffer eaf-buffer-name)))
    (with-current-buffer eaf-buffer
      (eaf-mode)
      ;; `eaf-buffer-url' should record full path of url, otherwise `eaf-open' will open duplicate PDF tab for same url.
      (set (make-local-variable 'eaf--buffer-url) url)
      (set (make-local-variable 'eaf--buffer-app-name) app-name)
      (run-hooks (intern (format "eaf-%s-hook" app-name)))
      (setq mode-name (concat "EAF/" app-name)))
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
  (when (and eaf--monitor-configuration-p
             (process-live-p eaf-process))
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

(defun eaf--delete-org-preview-file (org-file)
  (let ((org-html-file (concat (file-name-sans-extension org-file) ".html")))
    (when (file-exists-p org-html-file)
      (delete-file org-html-file)
      (message "[EAF] Cleaned org-preview file %s (%s)." org-html-file org-file))))

(defun eaf--org-killed-buffer-clean ()
  (dolist (org-killed-buffer eaf-org-killed-file-list)
    (unless (get-file-buffer org-killed-buffer)
      (setq eaf-org-file-list (remove org-killed-buffer eaf-org-file-list))
      (eaf--delete-org-preview-file org-killed-buffer)))
  (setq eaf-org-killed-file-list nil))

(defun eaf--monitor-buffer-kill ()
  "Function monitoring when an EAF buffer is killed."
  (ignore-errors
    (eaf-call "kill_buffer" eaf--buffer-id)
    (message "[EAF] Killed %s." eaf--buffer-id)))

(defun eaf--monitor-emacs-kill ()
  "Function monitoring when Emacs is killed, kill all EAF buffers."
  (ignore-errors
    (eaf-call "kill_emacs")))

(defun eaf--org-preview-monitor-kill ()
  "Function monitoring when org-preview application is killed."
  ;; Because save org buffer will trigger `kill-buffer' action,
  ;; but org buffer still live after do `kill-buffer' action.
  ;; So I run a timer to check org buffer is live after `kill-buffer' action.
  (when (member (buffer-file-name) eaf-org-file-list)
    (unless (member (buffer-file-name) eaf-org-killed-file-list)
      (push (buffer-file-name) eaf-org-killed-file-list))
    (run-with-timer 1 nil (lambda () (eaf--org-killed-buffer-clean)))))


(defun eaf--org-preview-monitor-buffer-save ()
  (when (process-live-p eaf-process)
    (ignore-errors
      ;; eaf-org-file-list?
      (org-html-export-to-html)
      (eaf-call "update_buffer_with_url" "app.org-previewer.buffer" (buffer-file-name) "")
      (message "[EAF] Export %s to HTML." (buffer-file-name)))))

(defun eaf-keyboard-quit ()
  "Wrap around `keyboard-quit' and signals a ‘quit’ condition to EAF applications."
  (interactive)
  (eaf-call "action_quit" eaf--buffer-id)
  (call-interactively 'keyboard-quit))

(defun eaf-send-key ()
  "Directly send key to EAF Python side."
  (interactive)
  (eaf-call "send_key" eaf--buffer-id (key-description (this-command-keys-vector))))

(defun eaf-send-key-sequence ()
  "Directly send key sequence to EAF Python side."
  (interactive)
  (eaf-call "send_key_sequence" eaf--buffer-id (key-description (this-command-keys-vector))))

(defun eaf-send-cancel-key-sequence ()
  "Send C-c to terminal."
  (interactive)
  (eaf-call "send_key_sequence" eaf--buffer-id "C-c"))

(defun eaf-set (sym val)
  "Similar to `set', but store SYM with VAL in EAF Python side, and return VAL.

For convenience, use the Lisp macro `eaf-setq' instead."
  (map-put eaf-var-list sym val)
  (when (process-live-p eaf-process)
    ;; Update python side variable dynamically.
    (eaf-call "update_emacs_var_dict" (eaf-serialization-var-list)))
  val)

(defmacro eaf-setq (var val)
  "Similar to `setq', but store VAR with VAL in EAF Python side, and return VAL.

Use it as (eaf-setq var val)"
  `(eaf-set ',var ,val))

(defmacro eaf-bind-key (command key eaf-app-keybinding)
  "This function binds COMMAND to KEY in EAF-APP-KEYBINDING list.

Use this to bind keys for EAF applications.

COMMAND is a symbol of a regular Emacs command or a python app
command.  You can see a list of available commands by calling
`eaf-describe-bindings' in an EAF buffer.  The `eaf-proxy-' prefix
should be dropped for the COMMAND symbol.

KEY is a string representing a sequence of keystrokes and events.

EAF-APP-KEYBINDING is one of the `eaf-<app-name>-keybinding'
variables, where <app-name> can be obtained by checking the value
of `eaf--buffer-app-name' inside the EAF buffer."
  `(map-put ,eaf-app-keybinding ,key
            ,(if (string-match "_" (symbol-name command))
                 (symbol-name command)
               `(quote ,command)) #'equal))


(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "focus_emacs_buffer"
 #'eaf-focus-buffer)

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
 (lambda (format-string) (message (concat "[EAF/" eaf--buffer-app-name "] " format-string))))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "set_emacs_var"
 #'eaf--set-emacs-var)

(defun eaf--set-emacs-var (var-name var-value)
  "Used by Python applications to set Lisp variable VAR-NAME with value VAR-VALUE on the Emacs side."
  (set (intern var-name) var-value))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "eval_in_emacs"
 #'eaf--eval-in-emacs)

(defun eaf--eval-in-emacs (elisp-code-string)
  "Used by Python applications to evaluate ELISP-CODE-STRING as Emacs Lisp code on the Emacs side."
  (eval (read elisp-code-string) 'lexical))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "create_new_browser_buffer"
 #'eaf--create-new-browser-buffer)

(defun eaf--create-new-browser-buffer (new-window-buffer-id)
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
          (message "[EAF] Request to kill buffer %s." kill-buffer-id)
          (throw 'found-match-buffer t))))))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "first_start"
 #'eaf--first-start)

(defun eaf--first-start ()
  "Call `eaf--open-internal' after receive `start_finish' signal from server process."
  (eaf--open-internal eaf--first-start-url eaf--first-start-app-name eaf--first-start-arguments))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "update_buffer_details"
 #'eaf--update-buffer-details)

(defun eaf--update-buffer-details (buffer-id title url)
  (when (> (length title) 0)
    (catch 'find-buffer
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (with-current-buffer buffer
            (when (and
                   (derived-mode-p 'eaf-mode)
                   (equal eaf--buffer-id buffer-id))
              (setq mode-name (concat "EAF/" eaf--buffer-app-name))
              (setq-local eaf--bookmark-title title)
              (setq-local eaf--buffer-url url)
              (rename-buffer (format eaf-buffer-title-format title))
              (throw 'find-buffer t))))))))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "translate_text"
 #'eaf-translate-text)

(defun eaf-translate-text (text)
  (when (featurep 'sdcv)
    (sdcv-search-input+ text)))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "input_message"
 #'eaf--input-message)

(defun eaf--input-message (input-buffer-id interactive-string callback-type interactive_type)
  "Handles input message INTERACTIVE-STRING on the Python side given INPUT-BUFFER-ID and CALLBACK-TYPE."
  (let* ((input-message (eaf-read-input (concat "[EAF/" eaf--buffer-app-name "] " interactive-string) interactive_type)))
    (if input-message
        (eaf-call "handle_input_message" input-buffer-id callback-type input-message)
      (eaf-call "cancel_input_message" input-buffer-id callback-type))))

(defun eaf-read-input (interactive-string interactive_type)
  "Like `read-string' which read an INTERACTIVE-STRING, but return nil if user execute `keyboard-quit' when input."
  (condition-case nil
      (cond ((string-equal interactive_type "string")
             (read-string interactive-string))
            ((string-equal interactive_type "file")
             (expand-file-name (read-file-name interactive-string))))
    (quit nil)))

(defun eaf--open-internal (url app-name arguments)
  (let* ((buffer (eaf--create-buffer url app-name))
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
  "Given BUF, split window to show file and previewer."
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

(defun eaf--gnus-htmlp (part)
  "Determine whether the gnus mail PART is HTML."
  (when-let ((type (mm-handle-type part)))
    (string= "text/html" (car type))))

(defun eaf--notmuch-htmlp (part)
  "Determine whether the notmuch mail PART is HTML."
  (when-let ((type (plist-get part :content-type)))
    (string= "text/html" type)))

(defun eaf--get-html-func ()
  "The function returning a function used to extract HTML of different MUAs."
  (catch 'get-html
    (cl-loop for (regex . func) in eaf-mua-get-html
             do (when (string-match regex (symbol-name major-mode))
                  (throw 'get-html func))
             finally return (error "[EAF] You are either not in a MUA buffer or your MUA is not supported!"))))

(defun eaf-gnus-get-html ()
  "Retrieve HTML part of a gnus mail."
  (with-current-buffer gnus-original-article-buffer
    (when-let* ((dissect (mm-dissect-buffer t t))
                (buffer (if (bufferp (car dissect))
                            (when (eaf--gnus-htmlp dissect)
                              (car dissect))
                          (car (cl-find-if #'eaf--gnus-htmlp (cdr dissect))))))
      (with-current-buffer buffer
        (buffer-string)))))

(defun eaf-mu4e-get-html ()
  "Retrieve HTML part of a mu4e mail."
  (let ((msg mu4e~view-message))
    (mu4e-message-field msg :body-html)))

(defun eaf-notmuch-get-html ()
  "Retrieve HTML part of a notmuch mail."
  (when-let* ((msg (cond ((derived-mode-p 'notmuch-show-mode)
                          (notmuch-show-get-message-properties))
                         ((derived-mode-p 'notmuch-tree-mode)
                          (notmuch-tree-get-message-properties))
                         (t nil)))
              (body (plist-get msg :body))
              (parts (car body))
              (content (plist-get parts :content))
              (part (if (listp content)
                        (cl-find-if #'eaf--notmuch-htmlp content)
                      (when (eaf--notmuch-htmlp parts)
                        parts))))
    (notmuch-get-bodypart-text msg part notmuch-show-process-crypto)))

;;;###autoload
(defun eaf-open-mail-as-html ()
  "Open the html mail in EAF Browser.

The value of `mail-user-agent' must be a KEY of the alist `eaf-mua-get-html'.

In that way the corresponding function will be called to retrieve the HTML
 part of the current mail."
  (interactive)
  (when-let* ((html (funcall (eaf--get-html-func)))
              (default-directory user-emacs-directory) ; force (temporary-file-directory) to ignore remote directory
              (file (concat (temporary-file-directory) (make-temp-name "eaf-mail-") ".html")))
    (with-temp-file file
      (insert html))
    (eaf-open file "browser" "temp_html_file")))

;;;###autoload
(defun eaf-open-rss-reader ()
  "Open EAF RSS Reader."
  (interactive)
  (eaf-open "RSS Reader" "rss-reader"))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "open_url_in_new_tab"
 #'eaf-open-browser)

;;;###autoload
(defun eaf-open-browser (url &optional arguments)
  "Open EAF browser application given a URL and ARGUMENTS."
  (interactive "M[EAF/browser] URL: ")
  (eaf-open (eaf-wrap-url url) "browser" arguments))

(defun eaf-is-valid-url (url)
  "Return non-nil if URL is valid."
  (and url
       ;; URL should not include blank char.
       (< (length (split-string url)) 2)
       ;; Use regexp matching URL.
       (or
        (and
         (string-prefix-p "file://" url)
         (string-suffix-p ".html" url))
        ;; Normal url address.
        (string-match "^\\(https?://\\)?[a-z0-9]+\\([-.][a-z0-9]+\\)*.+\\..+[a-z0-9.]\\{1,6\\}\\(:[0-9]{1,5}\\)?\\(/.*\\)?$" url)
        ;; Localhost url.
        (string-match "^\\(https?://\\)?\\(localhost\\|127.0.0.1\\):[0-9]+/?" url))))

(defun eaf-wrap-url (url)
  "Wraps URL with prefix http:// if URL does not include it."
  (if (or (string-prefix-p "http://" url)
          (string-prefix-p "https://" url)
          (string-prefix-p "file://" url))
      url
    (concat "http://" url)))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "open_url_in_background_tab"
 #'eaf-open-browser-in-background)

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "goto_left_tab"
 #'eaf-goto-left-tab)

(defun eaf-goto-left-tab ()
  (interactive)
  (when (ignore-errors (require 'awesome-tab))
    (awesome-tab-backward-tab)))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "goto_right_tab"
 #'eaf-goto-right-tab)

(defun eaf-goto-right-tab ()
  (interactive)
  (when (ignore-errors (require 'awesome-tab))
    (awesome-tab-forward-tab)))

;;;###autoload
(defun eaf-open-browser-in-background (url &optional arguments)
  (setq eaf--monitor-configuration-p nil)
  (let ((save-buffer (current-buffer)))
    (eaf-open-browser url arguments)
    (switch-to-buffer save-buffer))
  (setq eaf--monitor-configuration-p t))

;;;###autoload
(defun eaf-open-browser-with-history ()
  "A wrapper around `eaf-open-browser' that provides browser history candidates.

If URL is an invalid URL, it will use `eaf-browser-default-search-engine' to search URL as string literal.

This function works best if paired with a fuzzy search package."
  (interactive)
  (let* ((browser-history-file-path
          (concat eaf-config-location
                  (file-name-as-directory "browser")
                  (file-name-as-directory "history")
                  "log.txt"))
         (history-pattern "^\\(.+\\)ᛝ\\(.+\\)ᛡ\\(.+\\)$")
         (history-file-exists (file-exists-p browser-history-file-path))
         (history (completing-read
                   "[EAF/browser] Search || URL || History: "
                   (if history-file-exists
                       (mapcar
                        (lambda (h) (when (string-match history-pattern h)
                                 (format "[%s] ⇰ %s" (match-string 1 h) (match-string 2 h))))
                        (with-temp-buffer (insert-file-contents browser-history-file-path)
                                          (split-string (buffer-string) "\n" t)))
                     nil)))
         (history-url (eaf-is-valid-url (when (string-match "[^\s]+$" history)
                                          (match-string 0 history)))))
    (cond (history-url (eaf-open-browser history-url))
          ((eaf-is-valid-url history) (eaf-open-browser history))
          (t (eaf-search-it history)))))

;;;###autoload
(defun eaf-search-it (&optional search-string search-engine)
  "Use SEARCH-ENGINE search SEARCH-STRING.

If called interactively, SEARCH-STRING is defaulted to symbol or region string.
The user can enter a customized SEARCH-STRING.  SEARCH-ENGINE is defaulted
to `eaf-browser-default-search-engine' with a prefix arg, the user is able to
choose a search engine defined in `eaf-browser-search-engines'"
  (interactive)
  (let* ((real-search-engine (if current-prefix-arg
                                 (let ((all-search-engine (mapcar #'car eaf-browser-search-engines)))
                                   (completing-read
                                    (format "Choose search engine (default %s): " eaf-browser-default-search-engine)
                                    all-search-engine nil t nil nil eaf-browser-default-search-engine))
                               (or search-engine eaf-browser-default-search-engine)))
         (link (or (cdr (assoc real-search-engine
                               eaf-browser-search-engines))
                   (error (format "[EAF/browser] search engine %s is unknown to EAF!" real-search-engine))))
         (current-symbol (if mark-active
                             (buffer-substring (region-beginning) (region-end))
                           (symbol-at-point)))
         (search-url (if search-string
                         (format link search-string)
                       (let ((search-string (read-string (format "[EAF/browser] Search (%s): " current-symbol))))
                         (if (string-blank-p search-string)
                             (format link current-symbol)
                           (format link search-string))))))
    (eaf-open search-url "browser")))

;;;###autoload
(define-obsolete-function-alias 'eaf-open-url #'eaf-open-browser)

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

(defun eaf--get-app-for-extension (extension-name)
  (cl-loop for (app . ext) in eaf-app-extensions-alist
           if (member extension-name (symbol-value ext))
           return app))

;;;###autoload
(defun eaf-get-file-name-extension (file)
  (downcase (file-name-extension file)))

(defun eaf-open-this-from-dired ()
  "Open html/pdf/image/video files whenever possible with EAF in dired.
Other files will open normally with `dired-find-file' or `dired-find-alternate-file'"
  (interactive)
  (dolist (file (dired-get-marked-files))
    (cond ((eaf--get-app-for-extension
            (eaf-get-file-name-extension file))
           (eaf-open file))
          (eaf-find-alternate-file-in-dired
           (dired-find-alternate-file))
          (t (dired-find-file)))))

;;;###autoload
(define-obsolete-function-alias 'eaf-file-open-in-dired #'eaf-open-this-from-dired)

;;;###autoload
(defun eaf-open (url &optional app-name arguments open-always)
  "Open an EAF application with URL, optional APP-NAME and ARGUMENTS.

By default, `eaf-open' will switch to buffer if corresponding url exists.
`eaf-open' always open new buffer if option OPEN-ALWAYS is non-nil.

When called interactively, URL accepts a file that can be opened by EAF."
  (interactive "F[EAF] EAF Open: ")
  ;; Try to set app-name along with url if app-name is unset.
  (when (and (not app-name) (file-exists-p url))
    (setq url (expand-file-name url))
    (when (featurep 'recentf)
      (recentf-add-file url))
    (let* ((extension-name (eaf-get-file-name-extension url)))
      ;; Initialize app name, url and arguments
      (setq app-name (eaf--get-app-for-extension extension-name))
      (when (equal app-name "markdown-previewer")
        ;; Try get user's github token if `eaf-grip-token' is nil.
        (setq arguments
              (or eaf-grip-token
                  (read-string (concat "[EAF/" app-name "] Fill your own Github token (or set `eaf-grip-token' with token string): ")))))
      (when (equal app-name "browser")
        (setq url (concat "file://" url)))))
  ;; Now that app-name should hopefully be set
  (unless app-name
    ;; Output error to user if app-name is empty string.
    (message
     (concat (if app-name (concat "[EAF/" app-name "] ") "[EAF] ")
             (cond
              ((not (or (string-prefix-p "/" url)
                        (string-prefix-p "~" url))) "File %s cannot be opened.")
              ((file-exists-p url) "File %s cannot be opened.")
              (t "File %s does not exist.")))
     url))
  (unless arguments (setq arguments ""))
  ;; Hooks are only added if not present already...
  (add-hook 'window-size-change-functions #'eaf-monitor-window-size-change)
  (add-hook 'window-configuration-change-hook #'eaf-monitor-configuration-change)
  ;; Open URL with EAF application
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
        ;; if no match buffer found, call `eaf--open-internal'.
        (if (and exists-eaf-buffer
                 (not open-always))
            (progn
              (eaf--display-app-buffer app-name exists-eaf-buffer)
              (message (concat "[EAF/" app-name "] " "Switch to %s") url))
          (eaf--open-internal url app-name arguments)
          (message (concat "[EAF/" app-name "] " "Opening %s") url)))
    ;; Record user input, and call `eaf--open-internal' after receive `start_finish' signal from server process.
    (setq eaf--first-start-url url)
    (setq eaf--first-start-app-name app-name)
    (setq eaf--first-start-arguments arguments)
    (eaf-start-process)
    (message (concat "[EAF/" app-name "] " "Opening %s") url)))

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

(defun eaf-open-airshare ()
  "Open EAF Airshare application, share text string with your phone."
  (interactive)
  (let* ((current-symbol (if (use-region-p)
                             (buffer-substring-no-properties (region-beginning) (region-end))
                           (thing-at-point 'symbol)))
         (input-string (string-trim (read-string (format "[EAF/airshare] Share Text (%s): " current-symbol)))))
    (when (string-empty-p input-string)
      (setq input-string current-symbol))
    (eaf-open input-string "airshare")))

(define-obsolete-function-alias 'eaf-file-transfer-airshare #'eaf-open-airshare)

(defun eaf-file-sender-qrcode (file)
  "Open EAF File Sender application.

Select the file FILE to send to your smartphone, a QR code for the corresponding file will appear.

Make sure that your smartphone is connected to the same WiFi network as this computer."
  (interactive "F[EAF/file-sender] Select File: ")
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
  (interactive "D[EAF/file-receiver] Specify Destination: ")
  (eaf-open dir "file-receiver"))

(defun eaf-edit-buffer-cancel ()
  "Cancel EAF Browser focus text input and closes the buffer."
  (interactive)
  (kill-buffer)
  (delete-window)
  (message "[EAF/%s] Edit cancelled!" eaf--buffer-app-name))

(defun eaf-edit-buffer-confirm ()
  "Confirm EAF Browser focus text input and send the text to EAF Browser."
  (interactive)
  ;; Note: pickup buffer-id from buffer name and not restore buffer-id from buffer local variable.
  ;; Then we can switch edit buffer to any other mode, such as org-mode, to confirm buffer string.
  (eaf-call "update_focus_text"
            (replace-regexp-in-string "eaf-\\(.*?\\)-edit-focus-text-" "" (buffer-name))
            (buffer-string))
  (kill-buffer)
  (delete-window))

(defun eaf-edit-buffer-switch-to-org-mode ()
  "Switch to org-mode to edit table handly."
  (interactive)
  (org-mode)
  (outline-show-all)
  (beginning-of-buffer)
  (local-set-key (kbd "C-c C-c") 'eaf-edit-buffer-confirm)
  (local-set-key (kbd "C-c C-k") 'eaf-edit-buffer-cancel)
  (eaf--edit-set-header-line))

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "edit_focus_text"
 #'eaf--edit-focus-text)

(defun eaf--edit-focus-text (buffer-id focus-text)
  (split-window-below -10)
  (other-window 1)
  (let ((edit-text-buffer (generate-new-buffer (format "eaf-%s-edit-focus-text-%s" eaf--buffer-app-name buffer-id))))
    (switch-to-buffer edit-text-buffer)
    (eaf-edit-mode)
    (eaf--edit-set-header-line)
    (insert focus-text)
    ;; When text line number above
    (when (> (line-number-at-pos) 30)
      (beginning-of-buffer))
    ))

(defun eaf--edit-set-header-line ()
  (setq header-line-format
        (substitute-command-keys
         (concat
          "\\<eaf-edit-mode-map>"
          " EAF/" eaf--buffer-app-name " EDIT: "
          "Confirm with `\\[eaf-edit-buffer-confirm]', "
          "Cancel with `\\[eaf-edit-buffer-cancel]'. "
          "Switch to org-mode with `\\[eaf-edit-buffer-switch-to-org-mode]'. "
          ))))

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
