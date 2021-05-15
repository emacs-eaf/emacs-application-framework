;;; eaf.el --- Emacs application framework  -*- lexical-binding: t; -*-

;; Filename: eaf.el
;; Description: Emacs application framework
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: Tue Apr 20 18:47:41 2021 (-0400)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/manateelazycat/emacs-application-framework
;; Keywords:
;; Compatibility: emacs-version >= 27
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

;;; Code:
(require 'cl-lib)

(defun add-subdirs-to-load-path (dir)
  "Recursive add directory DIR to `load-path'."
  (mapcar
   (lambda (path) (add-to-list 'load-path path))
   (delete-dups (mapcar 'file-name-directory (directory-files-recursively dir "\.el$")))))

(add-subdirs-to-load-path (expand-file-name "app" (file-name-directory (locate-library "eaf"))))

;;;###autoload
(defun eaf-install-dependencies ()
  "An interactive function that run install-eaf.sh or install-eaf-win32.js or install-eaf-mac.sh for Linux or Windows or macOS respectively."
  (interactive)
  (let ((eaf-dir (file-name-directory (locate-library "eaf"))))
    (cond ((eq system-type 'gnu/linux)
           (shell-command (concat eaf-dir "install-eaf.sh" "&")))
          ((memq system-type '(cygwin windows-nt ms-dos))
           (shell-command (format "node %s" (concat eaf-dir "install-eaf-win32.js" "&"))))
          ((eq system-type 'darwin)
           (shell-command (concat eaf-dir "install-eaf-mac.sh" "&"))))))

(require 'bookmark)
(require 'cl-lib)
(require 'eaf-interleave)
(require 'eaf-mindmap)
(require 'epc)
(require 'epcs)
(require 'json)
(require 'map)
(require 's)
(require 'seq)
(require 'subr-x)

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
    (define-key map (kbd "C-c i") #'eaf-import-chrome-bookmarks)
    (define-key map (kbd "C-c e") #'eaf-open-external)
    (define-key map (kbd "M-'") #'eaf-toggle-fullscreen)
    (define-key map (kbd "M-/") #'eaf-get-path-or-url)
    (define-key map (kbd "M-[") #'eaf-share-path-or-url)
    (define-key map (vector 'remap #'keyboard-quit) #'eaf-keyboard-quit)
    (define-key map (vector 'remap #'self-insert-command) #'eaf-send-key)
    (dolist (single-key '("RET" "DEL" "TAB" "SPC" "<backtab>" "<home>" "<end>" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>" "<backspace>" "<return>"))
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
  "EAF buffer-local URL.")

(defvar-local eaf--buffer-app-name nil
  "EAF buffer-local app-name.")

(defvar-local eaf--buffer-args nil
  "EAF buffer-local args.")

(defvar-local eaf--buffer-map-alist nil
  "EAF buffer-local map alist.")

(defvar-local eaf--buffer-map-alist-order 1
  "Order of EAF buffer-local map alist in `emulation-mode-map-alists'.")

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
  ;; Disable cursor in eaf buffer.
  (setq-local cursor-type nil)

  (set (make-local-variable 'eaf--buffer-id) (eaf--generate-id))
  (setq-local bookmark-make-record-function #'eaf--bookmark-make-record)

  ;; Copy default value in case user already has bindings there
  (setq-local emulation-mode-map-alists
              (copy-alist (default-value 'emulation-mode-map-alists)))
  ;; Construct map alist
  (setq-local eaf--buffer-map-alist (list (cons t eaf-mode-map)))
  ;; Eanble mode map and make it the first priority
  (add-to-ordered-list
   'emulation-mode-map-alists
   'eaf--buffer-map-alist
   'eaf--buffer-map-alist-order)

  (add-hook 'kill-buffer-hook #'eaf--monitor-buffer-kill nil t)
  (add-hook 'kill-emacs-hook #'eaf--monitor-emacs-kill))

(defvar eaf-python-file (expand-file-name "eaf.py" (file-name-directory load-file-name)))

(defvar eaf-server-port nil)

(defun epcs:server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "EPC Server %s" (epc:uid)))
       (buf (epc:make-procbuf (format " *%s*" name)))
       (main-process
        (make-network-process
         :name name
         :buffer buf
         :family 'ipv4
         :server t
         :host "127.0.0.1"
         :service (or port t)
         :sentinel
         (lambda (process message)
           (epcs:sentinel process message connect-function)))))
    (unless port
      ;; notify port number to the parent process via STDOUT.
      (message "%s\n" (process-contact main-process :service)))
    (push (cons main-process
                (make-epcs:server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          epcs:server-processes)
    main-process))

(defvar eaf-server
  (let (server)
    (setq server (epcs:server-start
                  (lambda (mngr)
                    (let ((mngr mngr))
                      (epc:define-method
                       mngr 'eval-in-emacs
                       (lambda (&rest args)
                         ;; Decode argument with Base64 format automatically.
                         (apply (read (car args))
                                (mapcar
                                 (lambda (arg)
                                   (let ((arg (eaf--decode-string arg)))
                                     (cond ((string-prefix-p "'" arg)
                                            (read (substring arg 1)))
                                           (t arg)))) (cdr args)))))))
                  ))
    (if server
        (setq eaf-server-port (process-contact server :service))
      (message "eaf-server fails to start.")
      )
    server))

(when noninteractive
  ;; Start "event loop".
  (cl-loop repeat 600
           do (sleep-for 0.1)))

(defvar eaf-epc-process nil)

(defvar eaf-internal-process nil)
(defvar eaf-internal-process-prog nil)
(defvar eaf-internal-process-args nil)

(defvar eaf--active-buffers nil
  "Contains a list of '(buffer-url buffer-app-name buffer-args).")

(defvar eaf--webengine-include-private-codec nil)

(defvar eaf-org-file-list '())

(defvar eaf-org-killed-file-list '())

(defvar eaf-last-frame-width 0)

(defvar eaf-last-frame-height 0)

(when (eq system-type 'darwin)
  (defvar eaf--mac-switch-to-python nil
    "Record if Emacs switchs to Python process")

  (defvar eaf--mac-has-focus t
    "Record if Emacs has focus"))

(defcustom eaf-name "*eaf*"
  "Name of EAF buffer."
  :type 'string)

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

(defcustom eaf-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python3.exe" "python3")
  "The Python interpreter used to run eaf.py."
  :type 'string)

(defcustom eaf-config-location (expand-file-name (locate-user-emacs-file "eaf/"))
  "Directory where eaf will store configuration files."
  :type 'directory)

(defcustom eaf-chrome-bookmark-file "~/.config/google-chrome/Default/Bookmarks"
  "The default chrome bookmark file to import."
  :type 'string)

(defcustom eaf-var-list
  '((eaf-camera-save-path . "~/Downloads")
    (eaf-browser-enable-plugin . "true")
    (eaf-browser-enable-adblocker . "false")
    (eaf-browser-enable-autofill . "false")
    (eaf-browser-enable-javascript . "true")
    (eaf-browser-remember-history . "true")
    (eaf-browser-default-zoom . "1.0")
    (eaf-browser-font-family . "")
    (eaf-browser-blank-page-url . "https://www.google.com")
    (eaf-browser-scroll-behavior . "auto")
    (eaf-browser-download-path . "~/Downloads")
    (eaf-browser-aria2-proxy-host . "")
    (eaf-browser-aria2-proxy-port . "")
    (eaf-browser-aria2-auto-file-renaming . "false")
    (eaf-browser-dark-mode . "follow")
    (eaf-browser-pc-user-agent . "Mozilla/5.0 (X11; Linux x86_64; rv:85.0) Gecko/20100101 Firefox/85.0")
    (eaf-browser-phone-user-agent . "Mozilla/5.0 (iPhone; CPU iPhone OS 11_0 like Mac OS X) AppleWebKit/604.1.38 (KHTML, like Gecko) Version/11.0 Mobile/15A5370a Safari/604.1")
    (eaf-browser-chrome-history-file . "~/.config/google-chrome/Default/History")
    ;; DisallowUnknownUrlSchemes, AllowUnknownUrlSchemesFromUserInteraction, or AllowAllUnknownUrlSchemes
    (eaf-browser-unknown-url-scheme-policy . "AllowUnknownUrlSchemesFromUserInteraction")
    (eaf-pdf-dark-mode . "follow")
    (eaf-pdf-default-zoom . "1.0")
    (eaf-pdf-dark-exclude-image . "true")
    (eaf-pdf-scroll-ratio . "0.05")
    (eaf-terminal-dark-mode . "follow")
    (eaf-terminal-font-size . "13")
    (eaf-terminal-font-family . "")
    (eaf-markdown-dark-mode . "follow")
    (eaf-mindmap-dark-mode . "follow")
    (eaf-mindmap-save-path . "~/Documents")
    (eaf-mindmap-edit-mode . "false")
    (eaf-jupyter-font-size . "13")
    (eaf-jupyter-font-family . "")
    (eaf-jupyter-dark-mode . "follow")
    (eaf-marker-letters . "ASDFHJKLWEOPCNM")
    (eaf-emacs-theme-mode . "")
    (eaf-emacs-theme-background-color . "")
    (eaf-emacs-theme-foreground-color . ""))
  "The alist storing user-defined variables that's shared with EAF Python side.

Try not to modify this alist directly.  Use `eaf-setq' to modify instead."
  :type 'cons)

(defcustom eaf-browser-caret-mode-keybinding
  '(("j"   . "caret_next_line")
    ("k"   . "caret_previous_line")
    ("l"   . "caret_next_character")
    ("h"   . "caret_previous_character")
    ("w"   . "caret_next_word")
    ("b"   . "caret_previous_word")
    (")"   . "caret_next_sentence")
    ("("   . "caret_previous_sentence")
    ("g"   . "caret_to_bottom")
    ("G"   . "caret_to_top")
    ("/"   . "caret_search_forward")
    ("?"   . "caret_search_backward")
    ("."   . "caret_clear_search")
    ("v"   . "caret_toggle_mark")
    ("o"   . "caret_rotate_selection")
    ("y"   . "caret_translate_text")
    ("q"   . "caret_exit")
    ("C-n" . "caret_next_line")
    ("C-p" . "caret_previous_line")
    ("C-f" . "caret_next_character")
    ("C-b" . "caret_previous_character")
    ("M-f" . "caret_next_word")
    ("M-b" . "caret_previous_word")
    ("M-e" . "caret_next_sentence")
    ("M-a" . "caret_previous_sentence")
    ("C-<" . "caret_to_bottom")
    ("C->" . "caret_to_top")
    ("C-s" . "caret_search_forward")
    ("C-r" . "caret_search_backward")
    ("C-." . "caret_clear_search")
    ("C-SPC" . "caret_toggle_mark")
    ("C-o" . "caret_rotate_selection")
    ("C-y" . "caret_translate_text")
    ("C-q" . "caret_exit")
    ("c"   . "insert_or_caret_at_line")
    ("M-c" . "caret_toggle_browsing")
    ("<escape>" . "caret_exit"))
  "The keybinding of EAF Browser Caret Mode."
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
    ("M-e" . "atomic_edit")
    ("M-c" . "caret_toggle_browsing")
    ("M-D" . "select_text")
    ("M-s" . "open_link")
    ("M-S" . "open_link_new_buffer")
    ("M-B" . "open_link_background_buffer")
    ("C-/" . "undo_action")
    ("M-_" . "redo_action")
    ("M-w" . "copy_text")
    ("M-f" . "history_forward")
    ("M-b" . "history_backward")
    ("M-q" . "clear_cookies")
    ("C-t" . "toggle_password_autofill")
    ("C-d" . "save_page_password")
    ("M-a" . "toggle_adblocker")
    ("C-M-q" . "clear_history")
    ("C-M-i" . "import_chrome_history")
    ("M-v" . "scroll_down_page")
    ("M-<" . "scroll_to_begin")
    ("M->" . "scroll_to_bottom")
    ("M-p" . "duplicate_page")
    ("M-t" . "new_blank_page")
    ("M-d" . "toggle_dark_mode")
    ("SPC" . "insert_or_scroll_up_page")
    ("J" . "insert_or_select_left_tab")
    ("K" . "insert_or_select_right_tab")
    ("j" . "insert_or_scroll_up")
    ("k" . "insert_or_scroll_down")
    ("h" . "insert_or_scroll_left")
    ("l" . "insert_or_scroll_right")
    ("f" . "insert_or_open_link")
    ("F" . "insert_or_open_link_new_buffer")
    ("B" . "insert_or_open_link_background_buffer")
    ("c" . "insert_or_caret_at_line")
    ("u" . "insert_or_scroll_down_page")
    ("d" . "insert_or_scroll_up_page")
    ("H" . "insert_or_history_backward")
    ("L" . "insert_or_history_forward")
    ("t" . "insert_or_new_blank_page")
    ("T" . "insert_or_recover_prev_close_page")
    ("i" . "insert_or_focus_input")
    ("I" . "insert_or_open_downloads_setting")
    ("r" . "insert_or_refresh_page")
    ("g" . "insert_or_scroll_to_begin")
    ("x" . "insert_or_close_buffer")
    ("G" . "insert_or_scroll_to_bottom")
    ("-" . "insert_or_zoom_out")
    ("=" . "insert_or_zoom_in")
    ("0" . "insert_or_zoom_reset")
    ("m" . "insert_or_save_as_bookmark")
    ("o" . "insert_or_open_browser")
    ("y" . "insert_or_download_youtube_video")
    ("Y" . "insert_or_download_youtube_audio")
    ("p" . "insert_or_toggle_device")
    ("P" . "insert_or_duplicate_page")
    ("1" . "insert_or_save_as_pdf")
    ("2" . "insert_or_save_as_single_file")
    ("v" . "insert_or_view_source")
    ("e" . "insert_or_edit_url")
    ("n" . "insert_or_export_text")
    ("," . "insert_or_switch_to_reader_mode")
    ("." . "insert_or_translate_text")
    ("C-M-c" . "copy_code")
    ("C-M-l" . "copy_link")
    ("C-a" . "select_all_or_input_text")
    ("M-u" . "clear_focus")
    ("C-j" . "open_downloads_setting")
    ("M-o" . "eval_js")
    ("M-O" . "eval_js_file")
    ("<escape>" . "eaf-browser-send-esc-or-exit-fullscreen")
    ("M-," . "eaf-send-down-key")
    ("M-." . "eaf-send-up-key")
    ("M-m" . "eaf-send-return-key")
    ("<f5>" . "refresh_page")
    ("<f12>" . "open_devtools")
    ("<C-return>" . "eaf-send-ctrl-return-sequence")
    )
  "The keybinding of EAF Browser."
  :type 'cons)

(defcustom eaf-browser-key-alias
  '(("C-a" . "<home>")
    ("C-e" . "<end>"))
  "The key alias of EAF Browser."
  :type 'cons)

(defcustom eaf-pdf-viewer-keybinding
  '(("j" . "scroll_up")
    ("<down>" . "scroll_up")
    ("C-n" . "scroll_up")
    ("k" . "scroll_down")
    ("<up>" . "scroll_down")
    ("C-p" . "scroll_down")
    ("h" . "scroll_left")
    ("<left>" . "scroll_left")
    ("C-b" . "scroll_left")
    ("l" . "scroll_right")
    ("<right>" . "scroll_right")
    ("C-f" . "scroll_right")
    ("SPC" . "scroll_up_page")
    ("b" . "scroll_down_page")
    ("C-v" . "scroll_up_page")
    ("M-v" . "scroll_down_page")
    ("t" . "toggle_read_mode")
    ("0" . "zoom_reset")
    ("=" . "zoom_in")
    ("-" . "zoom_out")
    ("g" . "scroll_to_begin")
    ("G" . "scroll_to_end")
    ("p" . "jump_to_page")
    ("P" . "jump_to_percent")
    ("[" . "save_current_pos")
    ("]" . "jump_to_saved_pos")
    ("i" . "toggle_inverted_mode")
    ("m" . "toggle_mark_link")
    ("f" . "jump_to_link")
    ("M-w" . "copy_select")
    ("C-s" . "search_text_forward")
    ("C-r" . "search_text_backward")
    ("x" . "close_buffer")
    ("C-<right>" . "rotate_clockwise")
    ("C-<left>" . "rotate_counterclockwise")
    ("M-h" . "add_annot_highlight")
    ("M-u" . "add_annot_underline")
    ("M-s" . "add_annot_squiggly")
    ("M-d" . "add_annot_strikeout_or_delete_annot")
    ("M-e" . "add_annot_text_or_edit_annot")
    ("M-p" . "toggle_presentation_mode")
    ("J" . "select_left_tab")
    ("K" . "select_right_tab")
    ("o" . "eaf-pdf-outline"))
  "The keybinding of EAF PDF Viewer."
  :type 'cons)

(defcustom eaf-video-player-keybinding
  '(("SPC" . "toggle_play")
    ("x" . "close_buffer")
    ("h" . "play_backward")
    ("l" . "play_forward"))
  "The keybinding of EAF Video Player."
  :type 'cons)

(defcustom eaf-js-video-player-keybinding
  '(("SPC" . "toggle_play")
    ("M-g" . "exit_fullscreen")
    ("<f12>" . "open_devtools")
    ("h" . "backward")
    ("f" . "toggle_fullscreen")
    ("l" . "forward")
    ("r" . "restart")
    ("j" . "decrease_volume")
    ("k" . "increase_volume")
    ("x" . "close_buffer")
    ("c--" . "zoom_out")
    ("C-=" . "zoom_in")
    ("C-0" . "zoom_reset")
    )
  "The keybinding of EAF JS Video Player."
  :type 'cons)

(defcustom eaf-image-viewer-keybinding
  '(("n" . "load_next_image")
    ("p" . "load_prev_image")
    ("SPC" . "load_prev_image")
    ("," . "zoom_out")
    ("." . "zoom_in")
    ("/" . "zoom_reset")
    ("-" . "zoom_out")
    ("=" . "zoom_in")
    ("0" . "zoom_reset")
    ("9" . "zoom_toggle")
    ("x" . "close_buffer")
    ("u" . "rotate_left")
    ("i" . "rotate_right")
    ("y" . "flip_horizontal")
    ("o" . "flip_vertical")
    ("j" . "move_down")
    ("k" . "move_up")
    ("h" . "move_left")
    ("l" . "move_right")
    ("d" . "delete_current_image")
    ("<f12>" . "open_devtools")
    )
  "The keybinding of EAF Image Viewer."
  :type 'cons)

(defcustom eaf-terminal-keybinding
  '(("M-n" . "scroll_up")
    ("M-p" . "scroll_down")
    ("C-v" . "scroll_up_page")
    ("M-v" . "scroll_down_page")
    ("M-<" . "scroll_to_begin")
    ("M->" . "scroll_to_bottom")
    ("C--" . "zoom_out")
    ("C-=" . "zoom_in")
    ("C-0" . "zoom_reset")
    ("C-S-c" . "copy_text")
    ("C-S-v" . "yank_text")
    ("C-s" . "search_text_forward")
    ("M-s" . "search_text_backward")
    ("C-a" . "eaf-send-key-sequence")
    ("C-e" . "eaf-send-key-sequence")
    ("C-f" . "eaf-send-key-sequence")
    ("C-b" . "eaf-send-key-sequence")
    ("C-d" . "eaf-send-key-sequence")
    ("C-n" . "eaf-send-key-sequence")
    ("C-p" . "eaf-send-key-sequence")
    ("C-r" . "eaf-send-key-sequence")
    ("C-y" . "eaf-send-key-sequence")
    ("C-k" . "eaf-send-key-sequence")
    ("C-o" . "eaf-send-key-sequence")
    ("C-u" . "eaf-send-key-sequence")
    ("C-l" . "eaf-send-key-sequence")
    ("C-w" . "eaf-send-key-sequence")
    ("M-f" . "eaf-send-key-sequence")
    ("M-b" . "eaf-send-key-sequence")
    ("M-d" . "eaf-send-key-sequence")
    ("C-c C-c" . "eaf-send-second-key-sequence")
    ("C-c C-x" . "eaf-send-second-key-sequence")
    ("<f12>" . "open_devtools")
    ("M-w" . "copy_text")
    ("C-y" . "yank_text")
    ("C-S-a" . "select_all")
    ("C-S-l" . "clear_selection")
    ("C-M-l" . "clear")
    ("M-DEL" . "eaf-send-alt-backspace-sequence")
    ("M-<backspace>" . "eaf-send-alt-backspace-sequence"))
  "The keybinding of EAF Terminal."
  :type 'cons)

(defcustom eaf-camera-keybinding
  '(("p" . "take_photo"))
  "The keybinding of EAF Camera."
  :type 'cons)

(defcustom eaf-mindmap-keybinding
  '(("TAB" . "add_sub_node")
    ("RET" . "add_brother_node")
    ("<deletechar>" . "remove_node")
    ("M-m" . "update_node_topic")
    ("M-e" . "update_node_topic_inline")
    ("M-r" . "refresh_page")
    ("C--" . "zoom_out")
    ("C-=" . "zoom_in")
    ("C-0" . "zoom_reset")
    ("M-q" . "add_multiple_sub_nodes")
    ("M-RET" . "add_multiple_brother_nodes")
    ("M-i" . "add_multiple_middle_nodes")
    ("M-j" . "select_down_node")
    ("M-k" . "select_up_node")
    ("M-h" . "select_left_node")
    ("M-l" . "select_right_node")
    ("C-n" . "eaf-send-down-key")
    ("C-p" . "eaf-send-up-key")
    ("C-f" . "eaf-send-right-key")
    ("C-b" . "eaf-send-left-key")
    ("SPC" . "insert_or_toggle_node_selection")
    ("x" . "insert_or_close_buffer")
    ("j" . "insert_or_select_down_node")
    ("k" . "insert_or_select_up_node")
    ("h" . "insert_or_select_left_node")
    ("l" . "insert_or_select_right_node")
    ("w" . "insert_or_copy_node_topic")
    ("y" . "insert_or_paste_node_topic")
    ("W" . "insert_or_cut_node_tree")
    ("Y" . "insert_or_paste_node_tree")
    ("J" . "insert_or_select_left_tab")
    ("K" . "insert_or_select_right_tab")
    ("-" . "insert_or_zoom_out")
    ("=" . "insert_or_zoom_in")
    ("0" . "insert_or_zoom_reset")
    ("d" . "insert_or_remove_node")
    ("D" . "insert_or_remove_middle_node")
    ("i" . "insert_or_add_middle_node")
    ("f" . "insert_or_update_node_topic")
    ("t" . "insert_or_toggle_node")
    ("b" . "insert_or_change_node_background")
    ("c" . "insert_or_change_background_color")
    ("C" . "insert_or_change_text_color")
    ("1" . "insert_or_save_screenshot")
    ("2" . "insert_or_save_file")
    ("3" . "insert_or_save_org_file")
    ("4" . "insert_or_save_freemind_file")
    ("M-o" . "eval_js")
    ("M-p" . "eval_js_file")
    ("<f12>" . "open_devtools")
    )
  "The keybinding of EAF Mindmap."
  :type 'cons)

(defcustom eaf-jupyter-keybinding
  '(("C-+" . "zoom_in")
    ("C--" . "zoom_out")
    ("C-0" . "zoom_reset")
    ("C-l" . "eaf-send-key-sequence")
    ("C-a" . "eaf-send-key-sequence")
    ("C-e" . "eaf-send-key-sequence")
    ("C-u" . "eaf-send-key-sequence")
    ("C-k" . "eaf-send-key-sequence")
    ("C-y" . "eaf-send-key-sequence")
    ("C-p" . "eaf-send-key-sequence")
    ("C-n" . "eaf-send-key-sequence")
    ("C-f" . "eaf-send-key-sequence")
    ("C-b" . "eaf-send-key-sequence")
    ("C-d" . "eaf-send-key-sequence")
    ("M-b" . "eaf-send-key-sequence")
    ("M-f" . "eaf-send-key-sequence")
    ("M-d" . "eaf-send-key-sequence")
    ("M-<" . "eaf-send-key-sequence")
    ("M->" . "eaf-send-key-sequence")
    ("<C-return>" . "eaf-send-ctrl-return-sequence")
    ("<S-return>" . "eaf-send-shift-return-sequence")
    )
  "The keybinding of EAF Jupyter."
  :type 'cons)

(defcustom eaf-music-player-keybinding
  '(("<f12>" . "open_devtools")
    ("j" . "play_next")
    ("k" . "play_prev")
    ("," . "backward")
    ("." . "forward")
    ("SPC" . "toggle")
    ("C-n" . "scroll_up")
    ("C-p" . "scroll_down")
    ("C-v" . "scroll_up_page")
    ("M-v" . "scroll_down_page")
    ("M-<" . "scroll_to_begin")
    ("M->" . "scroll_to_bottom")
    )
  "The keybinding of EAF Music Player."
  :type 'cons)

(defcustom eaf-system-monitor-keybinding
  '(("<f12>" . "open_devtools")
    ("C-n" . "scroll_up")
    ("C-p" . "scroll_down")
    ("C-v" . "scroll_up_page")
    ("M-v" . "scroll_down_page")
    ("M-<" . "scroll_to_begin")
    ("M->" . "scroll_to_bottom")
    )
  "The keybinding of EAF System Monitor."
  :type 'cons)

(defcustom eaf-pdf-extension-list
  '("pdf" "xps" "oxps" "cbz" "epub" "fb2" "fbz")
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
  '("avi" "webm" "rmvb" "ogg" "mp4" "mkv" "m4v")
  "The extension list of video player application."
  :type 'cons)

(defcustom eaf-browser-extension-list
  '("html" "htm")
  "The extension list of browser application."
  :type 'cons)

(defcustom eaf-org-extension-list
  '("org")
  "The extension list of org previewer application."
  :type 'cons)

(defcustom eaf-mindmap-extension-list
  '("emm" "mm")
  "The extension list of mindmap application."
  :type 'cons)

(defcustom eaf-office-extension-list
  '("docx" "doc" "ppt" "pptx" "xlsx" "xls")
  "The extension list of office application."
  :type 'cons)

(defcustom eaf-find-file-ext-blacklist '()
  "A blacklist of extensions to avoid when opening `find-file' file using EAF."
  :type 'cons)

(defcustom eaf-mua-get-html
  '(("^gnus-" . eaf-gnus-get-html)
    ("^mu4e-" . eaf-mu4e-get-html)
    ("^notmuch-" . eaf-notmuch-get-html))
  "An alist regex mapping a MUA `major-mode' to a function to retrieve HTML part of a mail."
  :type 'alist)

(defcustom eaf-browser-continue-where-left-off nil
  "Similar to Chromium's Setting -> On start-up -> Continue where you left off.

If non-nil, all active EAF Browser buffers will be saved before Emacs is killed,
and will re-open them when calling `eaf-browser-restore-buffers' in the future session."
  :type 'boolean)

(defcustom eaf-browser-fullscreen-move-cursor-corner nil
  "If non-nil, move the mouse cursor to the corner when fullscreen in the browser."
  :type 'boolean)

(defcustom eaf-proxy-host ""
  "Proxy Host used by EAF Browser."
  :type 'string)

(defcustom eaf-proxy-port ""
  "Proxy Port used by EAF Browser."
  :type 'string)

(defcustom eaf-proxy-type ""
  "Proxy Type used by EAF Browser.  The value is either \"http\" or \"socks5\"."
  :type 'string)

(defcustom eaf-elfeed-split-direction "below"
  "Elfeed browser page display location.
Default is `below', you can chang it with `right'."
  :type 'string)

(defcustom eaf-enable-debug nil
  "If you got segfault error, please turn this option.
Then EAF will start by gdb, please send new issue with `*eaf*' buffer content when next crash."
  :type 'boolean)

(defcustom eaf-wm-name ""
  "The desktop name, set by function `eaf--get-current-desktop-name'."
  :type 'string)

(defcustom eaf-wm-focus-fix-wms
  `(
    "i3"                                ;i3
    "LG3D"                              ;QTile
    "Xpra"                              ;Windows WSL
    "EXWM"                              ;EXWM
    "Xfwm4"                             ;Xfce4
    )
  "Set mouse cursor to frame bottom in these wms, to make EAF receive input event.

EAF confirms that the desktop environment or window manager you can work includes:
KDE, Gnome2, Gnome3, Mate, Xfce, LXDE, Sway, i3, QTile, Xpra, EXWM.

If your window manager can't receive input event, you can try add `NAME' of command `wmctrl -m' to this list.

Please send PR if it works.
Please fill an issue if it still doesn't work."
  :type 'list)

(defvar eaf-app-binding-alist
  '(("browser" . eaf-browser-keybinding)
    ("pdf-viewer" . eaf-pdf-viewer-keybinding)
    ("video-player" . eaf-video-player-keybinding)
    ("js-video-player" . eaf-js-video-player-keybinding)
    ("image-viewer" . eaf-image-viewer-keybinding)
    ("music-player" . eaf-music-player-keybinding)
    ("system-monitor" . eaf-system-monitor-keybinding)
    ("camera" . eaf-camera-keybinding)
    ("terminal" . eaf-terminal-keybinding)
    ("markdown-previewer" . eaf-browser-keybinding)
    ("org-previewer" . eaf-browser-keybinding)
    ("mindmap" . eaf-mindmap-keybinding)
    ("jupyter" . eaf-jupyter-keybinding)
    )
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
    ("org-previewer" . eaf-org-extension-list)
    ("mindmap" . eaf-mindmap-extension-list)
    ("office" . eaf-office-extension-list))
  "Mapping app names to extension list variables.

A new app can use this to configure extensions which should
handled by it.")

(defvar eaf--monitor-configuration-p t
  "When this variable is non-nil, `eaf-monitor-configuration-change' executes.
This variable is used to open buffer in backend and avoid graphics blink.

EAF call python method `new_buffer' to create EAF application buffer.
EAF call python method `update_views' to create EAF application view.

Python process only create application view when Emacs window or buffer state change.")

(defvar eaf-fullscreen-p nil
  "When non-nil, EAF will intelligently hide modeline as necessray.")

(defvar eaf-buffer-title-format "%s")

(defvar eaf-pdf-outline-buffer-name "*eaf pdf outline*"
  "The name of pdf-outline-buffer.")

(defvar eaf-pdf-outline-window-configuration nil
  "Save window configure before popup outline buffer.")

(defvar-local eaf--bookmark-title nil)

(defvar-local eaf-mindmap--current-add-mode nil)

(defmacro eaf-for-each-eaf-buffer (&rest body)
  "A syntactic sugar to loop through each EAF buffer and evaluat BODY.

Within BODY, `buffer' can be used to"
  `(dolist (buffer (eaf--get-eaf-buffers))
     (with-current-buffer buffer
       ,@body)))

(defun eaf-browser-restore-buffers ()
  "EAF restore all opened EAF Browser buffers in the previous Emacs session.

This should be used after setting `eaf-browser-continue-where-left-off' to t."
  (interactive)
  (if eaf-browser-continue-where-left-off
      (let* ((browser-restore-file-path
              (concat eaf-config-location
                      (file-name-as-directory "browser")
                      (file-name-as-directory "history")
                      "restore.txt"))
             (browser-url-list
              (with-temp-buffer (insert-file-contents browser-restore-file-path)
                                (split-string (buffer-string) "\n" t))))
        (if (epc:live-p eaf-epc-process)
            (dolist (url browser-url-list)
              (eaf-open-browser url))
          (dolist (url browser-url-list)
            (push `(,url "browser" "") eaf--active-buffers))
          (when eaf--active-buffers (eaf-open-browser (nth 0 (car eaf--active-buffers))))))
    (user-error "Please set `eaf-browser-continue-where-left-off' to t first!")))


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
  "Restore EAF buffer according to browser bookmark from the current file path or web URL."
  `((handler . eaf--bookmark-restore)
    (eaf-app . "browser")
    (defaults . ,(list eaf--bookmark-title))
    (filename . ,(eaf-get-path-or-url))))

(defun eaf--browser-chrome-bookmark (name url)
  "Restore EAF buffer according to chrome bookmark of given title and web URL."
  `((handler . eaf--bookmark-restore)
    (eaf-app . "browser")
    (defaults . ,(list name))
    (filename . ,url)))

(defun eaf--pdf-viewer-bookmark ()
  "Restore EAF buffer according to pdf bookmark from the current file path or web URL."
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

;;;###autoload
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

(defun eaf-import-chrome-bookmarks ()
  "Command to import chrome bookmarks."
  (interactive)
  (when (eaf-read-input "Are you sure to import chrome bookmarks to EAF" "yes-or-no" "")
    (if (not (file-exists-p eaf-chrome-bookmark-file))
        (message "Chrome bookmark file: '%s' is not exist, check `eaf-chrome-bookmark-file` setting." eaf-chrome-bookmark-file)
      (let ((orig-bookmark-record-fn bookmark-make-record-function)
            (data (json-read-file eaf-chrome-bookmark-file)))
        (cl-labels ((fn (item)
                        (pcase (alist-get 'type item)
                          ("url"
                           (let ((name (alist-get 'name item))
                                 (url (alist-get 'url item)))
                             (if (not (equal "chrome://bookmarks/" url))
                                 (progn
                                   (setq-local bookmark-make-record-function
                                               #'(lambda () (eaf--browser-chrome-bookmark name url)))
                                   (bookmark-set name)))))
                          ("folder"
                           (mapc #'fn (alist-get 'children item))))))
          (fn (alist-get 'bookmark_bar (alist-get 'roots data)))
          (setq-local bookmark-make-record-function orig-bookmark-record-fn)
          (bookmark-save)
          (message "Import success."))))))

(defalias 'eaf--browser-firefox-bookmark 'eaf--browser-chrome-bookmark)

(defvar eaf--existing-bookmarks nil
  "Existing bookmarks in Emacs.
A hashtable, key is url and value is title.")

(defvar eaf--firefox-bookmarks nil
  "Bookmarks that should be imported from firefox.")

(defun eaf--load-existing-bookmarks()
  "Load existing bookmarks."
  (let ((bookmarks (make-hash-table :test 'equal)))
    (dolist (bm bookmark-alist)
      (let* ((name (car bm))
             (file (bookmark-get-filename name)))
        (puthash file name bookmarks)))
    bookmarks))

(defun eaf--useful-firefox-bookmark? (uri)
  "Check whether uri is a website url."
  (or (string-prefix-p "http://" uri)
      (string-prefix-p "https://" uri)))

(defun eaf--firefox-bookmark-to-import? (title uri)
  "Check whether uri should be imported."
  (when (eaf--useful-firefox-bookmark? uri)
    (let ((old (gethash uri eaf--existing-bookmarks)))
      (when (or
             (not old)
             (and (string-equal old "") (not (string-equal title ""))))
        t))))

(defun eaf--firefox-bookmark-to-import (title uri)
  (puthash uri title eaf--existing-bookmarks)
  (add-to-list 'eaf--firefox-bookmarks (cons uri title)))

(defun eaf-import-firefox-bookmarks ()
  "Command to import firefox bookmarks."
  (interactive)
  (when (eaf-read-input "In order to import, you should first backup firefox's bookmarks to a json file. Continue?" "yes-or-no" "")
    (let ((fx-bookmark-file (read-file-name "Choose firefox bookmark file:")))
      (if (not (file-exists-p fx-bookmark-file))
          (message "Firefox bookmark file: '%s' is not exist." fx-bookmark-file)
        (setq eaf--firefox-bookmarks nil)
        (setq eaf--existing-bookmarks (eaf--load-existing-bookmarks))
        (let ((orig-bookmark-record-fn bookmark-make-record-function)
              (data (json-read-file fx-bookmark-file)))
          (cl-labels ((fn (item)
                          (pcase (alist-get 'typeCode item)
                            (1
                             (let ((title (alist-get 'title item ""))
                                   (uri (alist-get 'uri item)))
                               (when (eaf--firefox-bookmark-to-import? title uri)
                                 (eaf--firefox-bookmark-to-import title uri))))
                            (2
                             (mapc #'fn (alist-get 'children item))))))
            (fn data)
            (dolist (bm eaf--firefox-bookmarks)
              (let ((uri (car bm))
                    (title (cdr bm)))
                (setq-local bookmark-make-record-function
                            #'(lambda () (eaf--browser-firefox-bookmark title uri)))
                (bookmark-set title)))
            (setq-local bookmark-make-record-function orig-bookmark-record-fn)
            (bookmark-save)
            (message "Import success.")))))))

(defun eaf-open-external ()
  "Command to open current path or url with external application."
  (interactive)
  (let ((path-or-url (eaf-get-path-or-url)))
    (cond ((memq system-type '(cygwin windows-nt ms-dos))
           (w32-shell-execute "open" path-or-url))
          ((eq system-type 'darwin)
           (shell-command (concat "open " (shell-quote-argument path-or-url))))
          ((eq system-type 'gnu/linux)
           (let ((process-connection-type nil))
             (start-process "" nil "xdg-open" path-or-url))))))

(defun eaf-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (deferred:$
    (epc:call-deferred eaf-epc-process (read method) args)))

(defun eaf-call-sync (method &rest args)
  "Call Python EPC function METHOD and ARGS synchronously."
  (epc:call-sync eaf-epc-process (read method) args))

(defun eaf-get-emacs-xid (frame)
  "Get Emacs FRAME xid."
  (if (eaf--called-from-wsl-on-windows-p)
      (eaf-call-sync "get_emacs_wsl_window_id")
    (frame-parameter frame 'window-id)))

(defun eaf-serialization-var-list ()
  "Serialize variable list."
  (json-encode eaf-var-list))

(defun eaf-start-process ()
  "Start EAF process if it isn't started."
  (cond
   ((not eaf--active-buffers)
    (user-error "[EAF] Please initiate EAF with eaf-open-... functions only"))
   ((epc:live-p eaf-epc-process)
    (user-error "[EAF] Process is already running")))
  (let* ((eaf-args (append
                    (list eaf-python-file)
                    (eaf-get-render-size)
                    (list eaf-proxy-host eaf-proxy-port eaf-proxy-type)
                    (list eaf-config-location)
                    (list (number-to-string eaf-server-port))
                    (list (eaf-serialization-var-list))))
         (gdb-args (list "-batch" "-ex" "run" "-ex" "bt" "--args" eaf-python-command)))
    (if (and (getenv "WAYLAND_DISPLAY") (not (string= (getenv "WAYLAND_DISPLAY") "")))
        (progn
          ;; We need manually set scale factor when at Gnome/Wayland environment.
          ;; It is important to set QT_AUTO_SCREEN_SCALE_FACTOR=0
          ;; otherwise Qt which explicitly force high DPI enabling get scaled TWICE.
          (setenv "QT_AUTO_SCREEN_SCALE_FACTOR" "0")
          ;; Set EAF application scale factor.
          (setenv "QT_SCALE_FACTOR" "1")
          ;; Force xwayland to ensure SWay works.
          (setenv "QT_QPA_PLATFORM" "xcb"))
      (setq process-environment
            (seq-filter
             (lambda (var)
               (and (not (string-match-p "QT_SCALE_FACTOR" var))
                    (not (string-match-p "QT_SCREEN_SCALE_FACTOR" var))))
             process-environment)))
    ;; Start python process.
    (if eaf-enable-debug
        (progn
          (setq eaf-internal-process-prog "gdb")
          (setq eaf-internal-process-args (append gdb-args eaf-args)))
      (setq eaf-internal-process-prog eaf-python-command)
      (setq eaf-internal-process-args eaf-args))
    (let ((process-connection-type (not (eaf--called-from-wsl-on-windows-p))))
      (setq eaf-internal-process
            (apply 'start-process
                   eaf-name eaf-name
                   eaf-internal-process-prog eaf-internal-process-args)))
    (set-process-query-on-exit-flag eaf-internal-process nil))
  (message "[EAF] Process starting..."))

(defun eaf-stop-process (&optional restart)
  "Stop EAF process and kill all EAF buffers.

If RESTART is non-nil, cached URL and app-name will not be cleared."
  (interactive)
  (unless restart
    ;; Clear active buffers
    (setq eaf--active-buffers nil)
    ;; Remove all EAF related hooks since the EAF process is stopped.
    (remove-hook 'kill-buffer-hook #'eaf--monitor-buffer-kill)
    (remove-hook 'kill-emacs-hook #'eaf--monitor-emacs-kill)
    (remove-hook 'after-save-hook #'eaf--org-preview-monitor-buffer-save)
    (remove-hook 'kill-buffer-hook #'eaf--org-preview-monitor-kill)
    (remove-hook 'window-size-change-functions #'eaf-monitor-window-size-change)
    (remove-hook 'window-configuration-change-hook #'eaf-monitor-configuration-change))

  ;; Clean `eaf-org-file-list' and `eaf-org-killed-file-list'.
  (dolist (org-file-name eaf-org-file-list)
    (eaf--delete-org-preview-file org-file-name))
  (setq eaf-org-file-list nil)
  (setq eaf-org-killed-file-list nil)
  (setq-local eaf-fullscreen-p nil)

  ;; Kill EAF-mode buffers.
  (let* ((eaf-buffers (eaf--get-eaf-buffers))
         (count (length eaf-buffers)))
    (dolist (buffer eaf-buffers)
      (kill-buffer buffer))
    ;; Just report to me when EAF buffer exists.
    (message "[EAF] Killed %s EAF buffer%s" count (if (> count 1) "s!" "!")))

  ;; Kill process after kill buffer, make application can save session data.
  (eaf--kill-python-process))

(defalias 'eaf-kill-process #'eaf-stop-process)

(defun eaf--kill-python-process ()
  "Kill EAF background python process."
  (interactive)
  (if (epc:live-p eaf-epc-process)
      (progn
        ;; Cleanup before exit EAF server process.
        (eaf-call-async "cleanup")
        ;; Delete EAF server process.
        (epc:stop-epc eaf-epc-process)
        ;; Kill *eaf* buffer.
        (when (get-buffer eaf-name)
          (kill-buffer eaf-name))
        (message "[EAF] Process terminated."))
    (message "[EAF] Process already terminated.")))

(defun eaf-restart-process ()
  "Stop and restart EAF process."
  (interactive)
  (when (get-buffer "DevTools - file:///")
    (kill-buffer "DevTools - file:///"))
  (setq eaf--active-buffers nil)
  (eaf-for-each-eaf-buffer
   (push `(,eaf--buffer-url ,eaf--buffer-app-name ,eaf--buffer-args) eaf--active-buffers))
  (eaf-stop-process t)
  (eaf-start-process))

(defun eaf--decode-string (str)
  "Decode string STR with UTF-8 coding using Base64."
  (decode-coding-string (base64-decode-string str) 'utf-8))

(defun eaf--encode-string (str)
  "Encode string STR with UTF-8 coding using Base64."
  (base64-encode-string (encode-coding-string str 'utf-8)))

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
  "Get WINDOW allocation."
  (let* ((window-edges (window-pixel-edges window))
         (x (nth 0 window-edges))
         (y (+ (nth 1 window-edges)
               (if (version< emacs-version "27.0")
                   (window-header-line-height window)
                 (window-tab-line-height window))))
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
          (message "%s" (kill-new (eaf-call-sync "call_function" eaf--buffer-id "get_url")))
        (eaf-call-sync "call_function" eaf--buffer-id "get_url"))
    (user-error "This command can only be called in an EAF buffer!")))

(defun eaf-toggle-fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (eaf-call-async "execute_function" eaf--buffer-id "toggle_fullscreen" (key-description (this-command-keys-vector))))

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
              (eaf-call-async "execute_function" eaf--buffer-id fun (key-description (this-command-keys-vector)))
            (message "%s command can only be called in an EAF buffer!" sym)))
        (format
         "Proxy function to call \"%s\" on the Python side.

Use `eaf-execute-app-cmd' if you want to execute this command programmatically.
Please ONLY use `eaf-bind-key' and use the unprefixed command name (\"%s\")
to edit EAF keybindings!" fun fun)))
    sym))

(defun eaf--gen-keybinding-map (keybinding &optional no-inherit-eaf-mode-map*)
  "Configure the `eaf-mode-map' from KEYBINDING, one of the eaf-.*-keybinding variables."
  (setq eaf-mode-map
        (let ((map (make-sparse-keymap)))
          (unless no-inherit-eaf-mode-map*
            (set-keymap-parent map eaf-mode-map*))
          (cl-loop for (key . fun) in keybinding
                   do (define-key map (kbd key)
                        (cond
                         ;; If command is normal symbol, just call it directly.
                         ((symbolp fun)
                          fun)
                         ;; If command is string and include - , it's elisp function, use `intern' build elisp function from function name.
                         ((string-match "-" fun)
                          (intern fun))
                         ;; If command is not built-in function and not include char '-'
                         ;; it's command in python side, build elisp proxy function to call it.
                         (t
                          (eaf--make-proxy-function fun))
                         ))
                   finally return map)))
  )

(defun eaf--toggle-caret-browsing (caret-status)
  "Toggle caret browsing given CARET-STATUS."
  (if caret-status
      (eaf--gen-keybinding-map eaf-browser-caret-mode-keybinding t)
    (eaf--gen-keybinding-map eaf-browser-keybinding))
  (setq eaf--buffer-map-alist (list (cons t eaf-mode-map))))

(defun eaf--get-app-bindings (app-name)
  "Get the specified APP-NAME keybinding.

Every app has its name and the corresponding
keybinding variable to eaf-app-binding-alist."
  (symbol-value
   (cdr (assoc app-name eaf-app-binding-alist))))

(defun eaf--create-buffer (url app-name args)
  "Create an EAF buffer given URL, APP-NAME, and ARGS."
  (eaf--gen-keybinding-map (eaf--get-app-bindings app-name))
  (let* ((eaf-buffer-name (if (equal (file-name-nondirectory url) "")
                              url
                            (file-name-nondirectory url)))
         (eaf-buffer (generate-new-buffer eaf-buffer-name))
         (url-directory (or (file-name-directory url) url)))
    (with-current-buffer eaf-buffer
      (eaf-mode)
      (when (file-accessible-directory-p url-directory)
        (setq-local default-directory url-directory))
      ;; `eaf-buffer-url' should record full path of url, otherwise `eaf-open' will open duplicate PDF tab for same url.
      (set (make-local-variable 'eaf--buffer-url) url)
      (set (make-local-variable 'eaf--buffer-app-name) app-name)
      (set (make-local-variable 'eaf--buffer-args) args)
      (run-hooks (intern (format "eaf-%s-hook" app-name)))
      (setq mode-name (concat "EAF/" app-name)))
    eaf-buffer))

(defun eaf-monitor-window-size-change (frame)
  "Delay some time and run `eaf-try-adjust-view-with-frame-size' to compare with Emacs FRAME size."
  (when (epc:live-p eaf-epc-process)
    (setq eaf-last-frame-width (frame-pixel-width frame))
    (setq eaf-last-frame-height (frame-pixel-height frame))
    (run-with-timer 1 nil (lambda () (eaf-try-adjust-view-with-frame-size frame)))))

(defun eaf-try-adjust-view-with-frame-size (frame)
  "Update EAF view once Emacs window size of the FRAME is changed."
  (unless (and (equal (frame-pixel-width frame) eaf-last-frame-width)
               (equal (frame-pixel-height frame) eaf-last-frame-height))
    (eaf-monitor-configuration-change)))

(defun eaf--frame-left (frame)
  "Return outer left position"
  (let ((left (frame-parameter frame 'left)))
    (if (listp left) (nth 1 left) left)))

(defun eaf--frame-top (frame)
  "Return outer top position."
  (let ((top (frame-parameter frame 'top)))
    (if (listp top) (nth 1 top) top)))

(defun eaf--frame-internal-height (frame)
  "Height of internal objects.
Including title-bar, menu-bar, offset depends on window system, and border."
  (let ((geometry (frame-geometry frame)))
    (+ (cdr (alist-get 'title-bar-size geometry))
       (cdr (alist-get 'tool-bar-size geometry)))))

(defun eaf--buffer-x-position-adjust (frame)
  "Adjust the x position of EAF buffers for macOS"
  (if (eq system-type 'darwin)
      (eaf--frame-left frame)
    0))

(defun eaf--buffer-y-postion-adjust (frame)
  "Adjust the y position of EAF buffers for macOS"
  (if (eq system-type 'darwin)
      (+ (eaf--frame-top frame) (eaf--frame-internal-height frame))
    0))

(eval-when-compile
  (when (eq system-type 'darwin)
    (defun eaf--mac-focus-change ()
      "Manage Emacs's focus change"
      (cond
       ((string= "Python\n" (shell-command-to-string "app-frontmost --name"))
        (setq eaf--mac-switch-to-python t))

       ((string= "Emacs\n" (shell-command-to-string "app-frontmost --name"))
        (cond
         (eaf--mac-switch-to-python
          (setq eaf--mac-switch-to-python nil))
         ((not eaf--mac-has-focus)
          (run-with-timer 0.1 nil #'eaf--mac-focus-in)
          )
         (eaf--mac-has-focus
          (eaf--mac-focus-out))))
       (t (eaf--mac-focus-out))))

    (defun eaf--mac-replace-eaf-buffers ()
      (dolist (window (window-list))
        (select-window window)
        (when (eq major-mode 'eaf-mode)
          (get-buffer-create "*eaf temp*")
          (switch-to-buffer "*eaf temp*" t))))

    (defun eaf--mac-focus-in ()
      (setq eaf--mac-has-focus t)
      (ignore-errors
        (set-window-configuration (frame-parameter (selected-frame) 'eaf--mac-frame))
        (bury-buffer "*eaf temp*"))
      )

    (defun eaf--mac-focus-out (&optional frame)
      (setq eaf--mac-has-focus nil)
      (set-frame-parameter (or frame (selected-frame)) 'eaf--mac-frame (current-window-configuration))
      (eaf--mac-replace-eaf-buffers))

    (add-function :after after-focus-change-function #'eaf--mac-focus-change)
    (add-to-list 'delete-frame-functions #'eaf--mac-focus-out)
    ))

(defun eaf-monitor-configuration-change (&rest _)
  "EAF function to respond when detecting a window configuration change."
  (when (and eaf--monitor-configuration-p
             (epc:live-p eaf-epc-process))
    (ignore-errors
      (let (view-infos)
        (dolist (frame (frame-list))
          (dolist (window (window-list frame))
            (with-current-buffer (window-buffer window)
              (when (derived-mode-p 'eaf-mode)
                ;; When `eaf-fullscreen-p' is non-nil, and only the EAF window is present, use frame size
                (if (and eaf-fullscreen-p (equal (length (window-list frame)) 1))
                    (push (format "%s:%s:%s:%s:%s:%s"
                                  eaf--buffer-id
                                  (eaf-get-emacs-xid frame)
                                  0 0 (frame-pixel-width frame) (frame-pixel-height frame))
                          view-infos)
                  (let* ((window-allocation (eaf-get-window-allocation window))
                         (window-divider-right-padding (if window-divider-mode window-divider-default-right-width 0))
                         (window-divider-bottom-padding (if window-divider-mode window-divider-default-bottom-width 0))
                         (x (+ (eaf--buffer-x-position-adjust frame) (nth 0 window-allocation)))
                         (y (+ (eaf--buffer-y-postion-adjust frame) (nth 1 window-allocation)))
                         (w (nth 2 window-allocation))
                         (h (nth 3 window-allocation)))
                    (push (format "%s:%s:%s:%s:%s:%s"
                                  eaf--buffer-id
                                  (eaf-get-emacs-xid frame)
                                  x
                                  y
                                  (- w window-divider-right-padding)
                                  (- h window-divider-bottom-padding))
                          view-infos)))))))
        (eaf-call-async "update_views" (mapconcat #'identity view-infos ","))))))

(defun eaf--delete-org-preview-file (org-file)
  "Delete the org-preview file when given ORG-FILE name."
  (let ((org-html-file (concat (file-name-sans-extension org-file) ".html")))
    (when (file-exists-p org-html-file)
      (delete-file org-html-file)
      (message "[EAF] Cleaned org-preview file %s (%s)." org-html-file org-file))))

(defun eaf--org-killed-buffer-clean ()
  "Function cleaning the killed org buffer."
  (dolist (org-killed-buffer eaf-org-killed-file-list)
    (unless (get-file-buffer org-killed-buffer)
      (setq eaf-org-file-list (remove org-killed-buffer eaf-org-file-list))
      (eaf--delete-org-preview-file org-killed-buffer)))
  (setq eaf-org-killed-file-list nil))

(defun eaf--get-eaf-buffers ()
  "A function that return a list of EAF buffers."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer
       (derived-mode-p 'eaf-mode)))
   (buffer-list)))

(defun eaf--monitor-buffer-kill ()
  "A function monitoring when an EAF buffer is killed."
  (ignore-errors
    (eaf-call-async "kill_buffer" eaf--buffer-id))

  ;; Kill eaf process when last eaf buffer closed.
  ;; We need add timer to avoid the last web page kill when terminal is exited.
  (run-at-time
   5 nil
   (lambda ()
     (when (equal (length (eaf--get-eaf-buffers)) 0)
       (eaf--kill-python-process))
     )))

(defun eaf--monitor-emacs-kill ()
  "Function monitoring when Emacs is killed."
  (ignore-errors
    (when eaf-browser-continue-where-left-off
      (let* ((browser-restore-file-path
              (concat eaf-config-location
                      (file-name-as-directory "browser")
                      (file-name-as-directory "history")
                      "restore.txt"))
             (browser-urls ""))
        (write-region
         (dolist (buffer (eaf--get-eaf-buffers) browser-urls)
           (with-current-buffer buffer
             (when (equal eaf--buffer-app-name "browser")
               (setq browser-urls (concat eaf--buffer-url "\n" browser-urls)))))
         nil browser-restore-file-path)))
    (eaf-call-async "kill_emacs")))

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
  "Save org-preview buffer."
  (when (epc:live-p eaf-epc-process)
    (ignore-errors
      ;; eaf-org-file-list?
      (org-html-export-to-html)
      (eaf-call-async "update_buffer_with_url" "app.org-previewer.buffer" (buffer-file-name) "")
      (message "[EAF] Export %s to HTML." (buffer-file-name)))))

(defun eaf-keyboard-quit ()
  "Wrap around `keyboard-quit' and signals a ‘quit’ condition to EAF applications."
  (interactive)
  (eaf-call-async "action_quit" eaf--buffer-id)
  (call-interactively 'keyboard-quit))

(defun eaf-send-key ()
  "Directly send key to EAF Python side."
  (interactive)
  (eaf-call-async "send_key" eaf--buffer-id (key-description (this-command-keys-vector))))

(defun eaf-send-left-key ()
  "Directly send left key to EAF Python side."
  (interactive)
  (eaf-call-async "send_key" eaf--buffer-id "<left>"))

(defun eaf-send-right-key ()
  "Directly send right key to EAF Python side."
  (interactive)
  (eaf-call-async "send_key" eaf--buffer-id "<right>"))

(defun eaf-send-down-key ()
  "Directly send down key to EAF Python side."
  (interactive)
  (eaf-call-async "send_key" eaf--buffer-id "<down>"))

(defun eaf-send-up-key ()
  "Directly send up key to EAF Python side."
  (interactive)
  (eaf-call-async "send_key" eaf--buffer-id "<up>"))

(defun eaf-send-return-key ()
  "Directly send return key to EAF Python side."
  (interactive)
  (eaf-call-async "send_key" eaf--buffer-id "RET"))

(defun eaf-send-key-sequence ()
  "Directly send key sequence to EAF Python side."
  (interactive)
  (eaf-call-async "send_key_sequence" eaf--buffer-id (key-description (this-command-keys-vector))))

(defun eaf-send-ctrl-return-sequence ()
  "Directly send Ctrl-Return key sequence to EAF Python side."
  (interactive)
  (eaf-call-async "send_key_sequence" eaf--buffer-id "C-RET"))

(defun eaf-send-alt-backspace-sequence ()
  "Directly send Alt-Backspace key sequence to EAF Python side."
  (interactive)
  (eaf-call-async "send_key_sequence" eaf--buffer-id "M-<backspace>"))

(defun eaf-send-shift-return-sequence ()
  "Directly send Shift-Return key sequence to EAF Python side."
  (interactive)
  (eaf-call-async "send_key_sequence" eaf--buffer-id "S-RET"))

(defun eaf-send-second-key-sequence ()
  "Send second part of key sequence to terminal."
  (interactive)
  (eaf-call-async "send_key_sequence"
                  eaf--buffer-id
                  (nth 1 (split-string (key-description (this-command-keys-vector))))))

(defun eaf-set (sym val)
  "Similar to `set', but store SYM with VAL in EAF Python side, and return VAL.

For convenience, use the Lisp macro `eaf-setq' instead."
  (setf (map-elt eaf-var-list sym) val)
  (when (epc:live-p eaf-epc-process)
    ;; Update python side variable dynamically.
    (eaf-call-async "update_emacs_var_dict" (eaf-serialization-var-list)))
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
  `(setf (map-elt ,eaf-app-keybinding ,key)
         ,(if (string-match "_" (symbol-name command))
              (symbol-name command)
            `(quote ,command))))

(defun eaf-focus-buffer (focus-buffer-id)
  "Focus the buffer given the FOCUS-BUFFER-ID."
  (catch 'found-eaf
    (eaf-for-each-eaf-buffer
     (when (string= eaf--buffer-id focus-buffer-id)
       (let ((buffer-window (get-buffer-window buffer)))
         (when buffer-window
           (select-window buffer-window)))
       (throw 'found-eaf t)))))

(defun eaf--show-message (format-string)
  "A wrapper around `message' that prepend [EAF/app-name] before FORMAT-STRING."
  (message "[EAF/%s] %s"
           eaf--buffer-app-name
           format-string
           ))

(defun eaf--set-emacs-var (name value eaf-specific)
  "Set Lisp variable NAME with VALUE on the Emacs side.

If EAF-SPECIFIC is true, this is modifying variables in `eaf-var-list'"
  (if (string= eaf-specific "true")
      (eaf-set (intern name) value)
    (set (intern name) value)))

(defun eaf--create-new-browser-buffer (new-window-buffer-id)
  "Function for creating a new browser buffer with the specified NEW-WINDOW-BUFFER-ID."
  (let ((eaf-buffer (generate-new-buffer (concat "Browser Popup Window " new-window-buffer-id))))
    (with-current-buffer eaf-buffer
      (eaf-mode)
      (set (make-local-variable 'eaf--buffer-id) new-window-buffer-id)
      (set (make-local-variable 'eaf--buffer-url) "")
      (set (make-local-variable 'eaf--buffer-app-name) "browser"))
    (switch-to-buffer eaf-buffer)))

(defun eaf-request-kill-buffer (kill-buffer-id)
  "Function for requesting to kill the given buffer with KILL-BUFFER-ID."
  (catch 'found-eaf
    (eaf-for-each-eaf-buffer
     (when (string= eaf--buffer-id kill-buffer-id)
       (kill-buffer buffer)
       (throw 'found-eaf t)))))

(defun eaf--first-start (eaf-epc-port webengine-include-private-codec)
  "Call `eaf--open-internal' upon receiving `start_finish' signal from server.

WEBENGINE-INCLUDE-PRIVATE-CODEC is only useful when app-name is video-player."
  ;; Make EPC process.
  (setq eaf-epc-process (make-epc:manager
                         :server-process eaf-internal-process
                         :commands (cons eaf-internal-process-prog eaf-internal-process-args)
                         :title (mapconcat 'identity (cons eaf-internal-process-prog eaf-internal-process-args) " ")
                         :port eaf-epc-port
                         :connection (epc:connect "localhost" eaf-epc-port)
                         ))
  (epc:init-epc-layer eaf-epc-process)

  ;; If webengine-include-private-codec and app name is "video-player", replace by "js-video-player".
  (setq eaf--webengine-include-private-codec webengine-include-private-codec)
  (let* ((first-buffer-info (pop eaf--active-buffers))
         (first-start-url (nth 0 first-buffer-info))
         (first-start-app-name (nth 1 first-buffer-info))
         (first-start-args (nth 2 first-buffer-info)))
    (when (and (string-equal first-start-app-name "video-player")
               (string-equal eaf--webengine-include-private-codec "True"))
      (setq first-start-app-name "js-video-player"))
    ;; Start first app.
    (eaf--open-internal first-start-url first-start-app-name first-start-args))

  (dolist (buffer-info eaf--active-buffers)
    (eaf--open-internal (nth 0 buffer-info) (nth 1 buffer-info) (nth 2 buffer-info)))
  (setq eaf--active-buffers nil))

(defun eaf--update-buffer-details (buffer-id title url)
  "Function for updating buffer details with its BUFFER-ID, TITLE and URL."
  (when (eaf--called-from-wsl-on-windows-p)
    (eaf-monitor-configuration-change))
  (when (> (length title) 0)
    (catch 'found-eaf
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (with-current-buffer buffer
            (when (and
                   (derived-mode-p 'eaf-mode)
                   (equal eaf--buffer-id buffer-id))
              (setq mode-name (concat "EAF/" eaf--buffer-app-name))
              (setq-local eaf--bookmark-title title)
              (setq-local eaf--buffer-url url)
              (rename-buffer (format eaf-buffer-title-format title) t)
              (eaf--update-modeline-icon)
              (throw 'found-eaf t))))))))

(defun eaf-translate-text (text)
  "Use sdcv to translate selected TEXT."
  (when (featurep 'sdcv)
    (sdcv-search-input+ text)))

(defun eaf--input-message (input-buffer-id interactive-string callback-tag interactive-type initial-content)
  "Handles input message INTERACTIVE-STRING on the Python side given INPUT-BUFFER-ID and CALLBACK-TYPE."
  (let* ((input-message (eaf-read-input (concat "[EAF/" eaf--buffer-app-name "] " interactive-string) interactive-type initial-content)))
    (if input-message
        (eaf-call-async "handle_input_response" input-buffer-id callback-tag input-message)
      (eaf-call-async "cancel_input_response" input-buffer-id callback-tag))))

(defun eaf-read-input (interactive-string interactive-type initial-content)
  "EAF's multi-purpose read-input function which read an INTERACTIVE-STRING with INITIAL-CONTENT, determines the function base on INTERACTIVE-TYPE."
  (condition-case nil
      (cond ((string-equal interactive-type "string")
             (read-string interactive-string initial-content))
            ((string-equal interactive-type "file")
             (expand-file-name (read-file-name interactive-string)))
            ((string-equal interactive-type "yes-or-no")
             (yes-or-no-p interactive-string)))
    (quit nil)))

(defun eaf--open-internal (url app-name args)
  "Open an EAF application internally with URL, APP-NAME and ARGS."
  (let* ((buffer (eaf--create-buffer url app-name args)))
    (with-current-buffer buffer
      (eaf-call-async "new_buffer" eaf--buffer-id
                      (if (eaf--called-from-wsl-on-windows-p)
                          (eaf--translate-wsl-url-to-windows url)
                        url)
                      app-name args)
      (eaf--update-modeline-icon))
    (eaf--display-app-buffer app-name buffer))
  (eaf--post-open-actions url app-name args))

(defun eaf--post-open-actions (url app-name args)
  "The function to run after `eaf--open-internal', taking the same URL, APP-NAME and ARGS."
  (cond ((and args (equal app-name "pdf-viewer"))
         (let ((office-pdf (string-match "office-pdf" args)))
           (when office-pdf
             (with-current-buffer (file-name-nondirectory url)
               (rename-buffer (concat "[Converted] " (substring args 0 (- office-pdf 1))) t)))))))

(defun eaf--update-modeline-icon ()
  "Update modeline icon if used"
  (when (and (ignore-errors (require 'all-the-icons) (featurep 'eaf-all-the-icons)))
    (declare-function eaf-all-the-icons-update-icon "eaf-all-the-icons.el")
    (eaf-all-the-icons-update-icon)))

(defun eaf--markdown-preview-display (buf)
  "Given BUF, split window to show file and previewer."
  (eaf-split-preview-windows
   (buffer-local-value
    'eaf--buffer-url buf))
  (switch-to-buffer buf)
  (other-window +1))

(defun eaf--org-preview-display (buf)
  "Given BUF, split window to show file and previewer."
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
              (default-directory (eaf--non-remote-default-directory))
              (file (concat (temporary-file-directory) (make-temp-name "eaf-mail-") ".html")))
    (with-temp-file file
      (insert html))
    (eaf-open file "browser" "temp_html_file")))

(defun eaf-open-devtool-page ()
  "Use EAF Browser to open the devtools page."
  (delete-other-windows)
  (split-window (selected-window) (/ (* (nth 3 (eaf-get-window-allocation (selected-window))) 2) 3) nil t)
  (other-window 1)
  (eaf-open "about:blank" "browser" "devtools"))

;;;###autoload
(defun eaf-open-browser (url &optional args)
  "Open EAF browser application given a URL and ARGS."
  (interactive "M[EAF/browser] URL: ")
  (eaf-open (eaf-wrap-url url) "browser" args))

(defun eaf-open-url-at-point ()
  "Open URL at current point by EAF browser."
  (interactive)
  (eaf-open-browser (browse-url-url-at-point)))

(defun eaf-browser--duplicate-page-in-new-tab (url)
  "Duplicate a new tab for the dedicated URL."
  (eaf-open (eaf-wrap-url url) "browser" nil t))

(defun eaf-is-valid-web-url (url)
  "Return the same URL if it is valid."
  (when (and url
             ;; URL should not include blank char.
             (< (length (split-string url)) 2)
             ;; Use regexp matching URL.
             (or (and
                  (string-prefix-p "file://" url)
                  (string-suffix-p ".html" url))
                 ;; Normal url address.
                 (string-match "^\\(https?://\\)?[a-z0-9]+\\([-.][a-z0-9]+\\)*.+\\..+[a-z0-9.]\\{1,6\\}\\(:[0-9]{1,5}\\)?\\(/.*\\)?$" url)
                 ;; Localhost url.
                 (string-match "^\\(https?://\\)?\\(localhost\\|127.0.0.1\\):[0-9]+/?" url)))
    url))

(defun eaf-wrap-url (url)
  "Wraps URL with prefix http:// if URL does not include it."
  (if (or (string-prefix-p "http://" url)
          (string-prefix-p "https://" url)
          (string-prefix-p "file://" url)
          (string-prefix-p "chrome://" url))
      url
    (concat "http://" url)))

(defun eaf-goto-left-tab ()
  "Go to left tab when awesome-tab exists."
  (interactive)
  (when (ignore-errors (require 'awesome-tab))
    (awesome-tab-backward-tab)))

(defun eaf-goto-right-tab ()
  "Go to right tab when awesome-tab exists."
  (interactive)
  (when (ignore-errors (require 'awesome-tab))
    (awesome-tab-forward-tab)))

;;;###autoload
(defun eaf-open-browser-in-background (url &optional args)
  "Open browser with the specified URL and optional ARGS in background."
  (setq eaf--monitor-configuration-p nil)
  (let ((save-buffer (current-buffer)))
    (eaf-open-browser url args)
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
         (history-url (eaf-is-valid-web-url (when (string-match "⇰\s\\(.+\\)$" history)
                                              (match-string 1 history)))))
    (cond (history-url (eaf-open-browser history-url))
          ((eaf-is-valid-web-url history) (eaf-open-browser history))
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
                                    (format "[EAF/browser] Select search engine (default %s): " eaf-browser-default-search-engine)
                                    all-search-engine nil t nil nil eaf-browser-default-search-engine))
                               (or search-engine eaf-browser-default-search-engine)))
         (link (or (cdr (assoc real-search-engine
                               eaf-browser-search-engines))
                   (error (format "[EAF/browser] Search engine %s is unknown to EAF!" real-search-engine))))
         (current-symbol (if mark-active
                             (if (eq major-mode 'pdf-view-mode)
                                 (progn
                                   (declare-function pdf-view-active-region-text "pdf-view.el")
                                   (car (pdf-view-active-region-text)))
                               (buffer-substring (region-beginning) (region-end)))
                           (symbol-at-point)))
         (search-url (if search-string
                         (format link search-string)
                       (let ((search-string (read-string (format "[EAF/browser] Search (%s): " current-symbol))))
                         (if (string-blank-p search-string)
                             (format link current-symbol)
                           (format link search-string))))))
    (eaf-open search-url "browser")))

;;;###autoload
(defun eaf-open-demo ()
  "Open EAF demo screen to verify that EAF is working properly."
  (interactive)
  (eaf-open "eaf-demo" "demo"))

(defun eaf-open-vue-demo ()
  "Open EAF vue demo"
  (interactive)
  (eaf-open "eaf-vue-demo" "vue-demo"))

(defun eaf-open-music-player (music-file)
  "Open EAF music player."
  (interactive "fOpen music: ")
  (eaf-open "eaf-music-player" "music-player" music-file))

(defun eaf-open-system-monitor ()
  "Open EAF system monitor."
  (interactive)
  (eaf-open "eaf-system-monitor" "system-monitor"))

;;;###autoload
(defun eaf-open-camera ()
  "Open EAF camera application."
  (interactive)
  (eaf-open "eaf-camera" "camera"))

(defun eaf-ipython-command ()
  (if (eaf--called-from-wsl-on-windows-p)
      "ipython.exe"
    "ipython"))

(defun eaf-open-ipython ()
  "Open ipython in terminal."
  (interactive)
  (if (executable-find (eaf-ipython-command))
      (eaf-terminal-run-command-in-dir
       (eaf-ipython-command)
       (eaf--non-remote-default-directory))
    (message "[EAF/terminal] Please install ipython first.")))

(defun eaf-open-jupyter ()
  "Open jupyter."
  (interactive)
  (if (executable-find (if (eaf--called-from-wsl-on-windows-p)
                           "jupyter-qtconsole.exe"
                         "jupyter-qtconsole"))
      (let* ((data (json-read-from-string (shell-command-to-string (if (eaf--called-from-wsl-on-windows-p)
                                                                       "jupyter.exe kernelspec list --json"
                                                                     "jupyter kernelspec list --json"))))
             (kernel (completing-read "Jupyter Kernels: " (mapcar #'car (alist-get 'kernelspecs data))))
             (args (make-hash-table :test 'equal)))
        (puthash "kernel" kernel args)
        (eaf-open (format "eaf-jupyter-%s" kernel) "jupyter" (json-encode-hash-table args) t))
    (message "[EAF/jupyter] Please install qtconsole first.")))

;;;###autoload
(defun eaf-open-terminal ()
  "Open EAF Terminal, a powerful GUI terminal emulator in Emacs.

The initial directory is `default-directory'.  However, it opens `$HOME'
 when `default-directory' is part of a remote process.

If a buffer of EAF Terminal in `default-directory' exists, switch to the buffer.
To override and open a new terminal regardless, call interactively with prefix arg."
  (interactive)
  (eaf-terminal-run-command-in-dir (eaf--generate-terminal-command) (eaf--non-remote-default-directory) t))

(defun eaf-terminal-run-command-in-dir (command dir &optional always-new)
  "Run COMMAND in terminal in directory DIR.

If ALWAYS-NEW is non-nil, always open a new terminal for the dedicated DIR."
  (let* ((args (make-hash-table :test 'equal))
         (expand-dir (expand-file-name dir)))
    (puthash "command" command args)
    (puthash "directory"
             (if (eaf--called-from-wsl-on-windows-p)
                 (eaf--translate-wsl-url-to-windows expand-dir)
               expand-dir)
             args)
    (eaf-open dir "terminal" (json-encode-hash-table args) always-new)))

(defun eaf--non-remote-default-directory ()
  "Return `default-directory' itself if is not part of remote, otherwise return $HOME."
  (if (or (file-remote-p default-directory)
          (not (file-accessible-directory-p default-directory)))
      (getenv "HOME")
    default-directory))

(defun eaf--generate-terminal-command ()
  (if (or (eaf--called-from-wsl-on-windows-p)
          (eq system-type 'windows-nt))
      "powershell.exe"
    (getenv "SHELL")))

(defun eaf--get-app-for-extension (extension-name)
  "Given the EXTENSION-NAME, loops through `eaf-app-extensions-alist', set and return `app-name'."
  (let ((app-name
         (cl-loop for (app . ext) in eaf-app-extensions-alist
                  if (member extension-name (symbol-value ext))
                  return app)))
    (if (string-equal app-name "video-player")
        ;; Use Browser play video if QWebEngine include private codec.
        (if (string-equal eaf--webengine-include-private-codec "True") "js-video-player" "video-player")
      app-name)))

;;;###autoload
(defun eaf-get-file-name-extension (file)
  "A wrapper around `file-name-extension' that downcases the extension of the FILE."
  (downcase (file-name-extension file)))

(defun eaf--called-from-wsl-on-windows-p ()
  "Check whether eaf is called by Emacs on WSL and is running on Windows."
  (and (eq system-type 'gnu/linux)
       (string-match-p ".exe" eaf-python-command)))

(defun eaf--translate-wsl-url-to-windows (path)
  "Translate from a WSL path to a Windows path'"
  (replace-regexp-in-string "/mnt/\\([a-zA-Z]\\)" "\\1:" path))

;;;###autoload
(defun eaf-open (url &optional app-name args always-new)
  "Open an EAF application with URL, optional APP-NAME and ARGS.

Interactively, a prefix arg replaces ALWAYS-NEW, which means to open a new
 buffer regardless of whether a buffer with existing URL and APP-NAME exists.

By default, `eaf-open' will switch to buffer if corresponding url exists.
`eaf-open' always open new buffer if option OPEN-ALWAYS is non-nil.

When called interactively, URL accepts a file that can be opened by EAF."
  (interactive "F[EAF] EAF Open: ")
  ;; Try to set app-name along with url when calling INTERACTIVELY
  (when (and (not app-name) (file-exists-p url))
    (setq url (expand-file-name url))
    (when (featurep 'recentf)
      (recentf-add-file url))
    (let* ((extension-name (eaf-get-file-name-extension url)))
      ;; Initialize url, app-name and args
      (setq app-name (eaf--get-app-for-extension extension-name))
      (cond
       ((equal app-name "browser")
        (setq url (concat "file://" url)))
       ((equal app-name "office")
        (user-error "Please use `eaf-open-office' instead!"))
       ((equal app-name "markdown-previewer")
        ;; Warning user install java if found PlantUML syntax in markdown file.
        (with-temp-buffer
          (insert-file-contents url)
          (goto-char (point-min))
          (when (search-forward "```puml" nil t)
            (unless (executable-find "java")
              (user-error (format "Have PlantUML code in file '%s', you need to install Java to preview normally." url))
              )))))))
  ;; Now that app-name should hopefully be set
  (unless app-name
    ;; Output error to user if app-name is empty string.
    (user-error (concat (if app-name (concat "[EAF/" app-name "] ") "[EAF] ")
                        (cond
                         ((not (or (string-prefix-p "/" url)
                                   (string-prefix-p "~" url))) "File %s cannot be opened.")
                         ((file-exists-p url) "File %s cannot be opened.")
                         (t "File %s does not exist.")))
                url))
  (unless args (setq args ""))
  (setq always-new (or always-new current-prefix-arg))
  ;; Hooks are only added if not present already...
  (add-hook 'window-size-change-functions #'eaf-monitor-window-size-change)
  (add-hook 'window-configuration-change-hook #'eaf-monitor-configuration-change)
  ;; Open URL with EAF application
  (if (epc:live-p eaf-epc-process)
      (let (exists-eaf-buffer)
        ;; Try to open buffer
        (catch 'found-eaf
          (eaf-for-each-eaf-buffer
           (when (and (string= eaf--buffer-url url)
                      (string= eaf--buffer-app-name app-name))
             (setq exists-eaf-buffer buffer)
             (throw 'found-eaf t))))
        ;; Switch to existing buffer,
        ;; if no match buffer found, call `eaf--open-internal'.
        (if (and exists-eaf-buffer
                 (not always-new))
            (progn
              (eaf--display-app-buffer app-name exists-eaf-buffer)
              (message (concat "[EAF/" app-name "] " "Switch to %s") url))
          (eaf--open-internal url app-name args)
          (message (concat "[EAF/" app-name "] " "Opening %s") url)))
    ;; Record user input, and call `eaf--open-internal' after receive `start_finish' signal from server process.
    (unless eaf--active-buffers
      (push `(,url ,app-name ,args) eaf--active-buffers))
    (eaf-start-process)
    (message (concat "[EAF/" app-name "] " "Opening %s") url)))

(defun eaf--display-app-buffer (app-name buffer)
  "Display specified APP-NAME's app buffer in BUFFER."
  (let ((display-fun (or (cdr (assoc app-name
                                     eaf-app-display-function-alist))
                         #'switch-to-buffer)))
    (funcall display-fun buffer)))

(defun eaf-split-preview-windows (url)
  "Function for spliting preview windows with specified URL."
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

(defun eaf-file-browser-qrcode (dir)
  "Open EAF File Browser application.

Select directory DIR to share file from the smartphone.

Make sure that your smartphone is connected to the same WiFi network as this computer."
  (interactive "D[EAF/file-browser] Specify Destination: ")
  (eaf-open dir "file-browser"))

(defun eaf-edit-buffer-cancel ()
  "Cancel EAF Browser focus text input and closes the buffer."
  (interactive)
  (kill-buffer)
  (delete-window)
  (message "[EAF/%s] Edit cancelled!" eaf--buffer-app-name))

(defun eaf-edit-buffer-confirm ()
  "Confirm input text and send the text to corresponding EAF app."
  (interactive)
  ;; Note: pickup buffer-id from buffer name and not restore buffer-id from buffer local variable.
  ;; Then we can switch edit buffer to any other mode, such as org-mode, to confirm buffer string.
  (cond ((equal eaf-mindmap--current-add-mode "sub")
         (eaf-call-async "update_multiple_sub_nodes"
                         eaf--buffer-id
                         (buffer-string)))
        ((equal eaf-mindmap--current-add-mode "brother")
         (eaf-call-async "update_multiple_brother_nodes"
                         eaf--buffer-id
                         (buffer-string)))
        ((equal eaf-mindmap--current-add-mode "middle")
         (eaf-call-async "update_multiple_middle_nodes"
                         eaf--buffer-id
                         (buffer-string)))
        (t
         (eaf-call-async "update_focus_text"
                         eaf--buffer-id
                         (eaf--encode-string (kill-new (buffer-string))))))
  (kill-buffer)
  (delete-window))

(defun eaf-edit-buffer-switch-to-org-mode ()
  "Switch to `org-mode' to edit table handily."
  (interactive)
  (let ((buffer-app-name eaf--buffer-app-name)
        (buffer-id eaf--buffer-id))
    (org-mode)
    (set (make-local-variable 'eaf--buffer-app-name) buffer-app-name)
    (set (make-local-variable 'eaf--buffer-id) buffer-id)
    (outline-show-all)
    (goto-char (point-min))
    (local-set-key (kbd "C-c C-c") 'eaf-edit-buffer-confirm)
    (local-set-key (kbd "C-c C-k") 'eaf-edit-buffer-cancel)
    (eaf--edit-set-header-line)))

(defalias 'eaf-create-mindmap 'eaf-open-mindmap) ;; compatible

(defun eaf-open-mindmap (file)
  "Open a given Mindmap FILE."
  (interactive "F[EAF/mindmap] Select Mindmap file: ")
  (eaf-open file "mindmap"))

(defun eaf-get-file-md5 (file)
  "Get the MD5 value of a specified FILE."
  (car (split-string (shell-command-to-string (format "md5sum '%s'" (file-truename file))) " ")))

(defun eaf-open-office (file)
  "View Microsoft Office FILE as READ-ONLY PDF."
  (interactive "f[EAF/office] Open Office file as PDF: ")
  (if (executable-find "libreoffice")
      (let* ((file-md5 (eaf-get-file-md5 file))
             (file-name-base (file-name-base file))
             (convert-file (format "/tmp/%s.pdf" file-name-base))
             (pdf-file (format "/tmp/%s.pdf" file-md5)))
        (if (file-exists-p pdf-file)
            (eaf-open pdf-file "pdf-viewer" (concat file-name-base "_office-pdf"))
          (message "Converting %s to PDF format, EAF will start after convert finish." file)
          (make-process
           :name ""
           :buffer " *eaf-open-office*"
           :command (list "libreoffice" "--headless" "--convert-to" "pdf" (file-truename file) "--outdir" "/tmp")
           :sentinel (lambda (_ event)
                       (when (string= (substring event 0 -1) "finished")
                         (rename-file convert-file pdf-file)
                         (eaf-open pdf-file "pdf-viewer" (concat file-name-base "_office-pdf")))))))
    (error "[EAF/office] libreoffice is required convert Office file to PDF!")))

(defun eaf--atomic-edit (buffer-id focus-text)
  "EAF Browser: edit FOCUS-TEXT with Emacs's BUFFER-ID."
  (split-window-below -10)
  (other-window 1)
  (let ((edit-text-buffer (generate-new-buffer (format "eaf-%s-atomic-edit" eaf--buffer-app-name)))
        (buffer-app-name eaf--buffer-app-name))
    (with-current-buffer edit-text-buffer
      (eaf-edit-mode)
      (set (make-local-variable 'eaf--buffer-app-name) buffer-app-name)
      (set (make-local-variable 'eaf--buffer-id) buffer-id))
    (switch-to-buffer edit-text-buffer)
    (setq-local eaf-mindmap--current-add-mode "")
    (eaf--edit-set-header-line)
    (insert focus-text)
    ;; When text line number above
    (when (> (line-number-at-pos) 30)
      (goto-char (point-min)))))

(defun eaf--edit-set-header-line ()
  "Set header line."
  (setq header-line-format
        (substitute-command-keys
         (concat
          "\\<eaf-edit-mode-map>"
          " EAF/" eaf--buffer-app-name " EDIT: "
          "Confirm with `\\[eaf-edit-buffer-confirm]', "
          "Cancel with `\\[eaf-edit-buffer-cancel]'. "
          "Switch to org-mode with `\\[eaf-edit-buffer-switch-to-org-mode]'. "
          ))))

(defun eaf--enter-fullscreen-request ()
  "Entering EAF browser fullscreen use Emacs frame's size."
  (setq-local eaf-fullscreen-p t)
  (eaf-monitor-configuration-change)
  (when (and eaf-browser-fullscreen-move-cursor-corner
             (or (string= eaf--buffer-app-name "browser")
                 (string= eaf--buffer-app-name "js-video-player")))
    (eaf-call-async "execute_function" eaf--buffer-id "move_cursor_to_corner" (key-description (this-command-keys-vector)))))

(defun eaf--exit_fullscreen_request ()
  "Exit EAF browser fullscreen."
  (setq-local eaf-fullscreen-p nil)
  (eaf-monitor-configuration-change))

(defun eaf-browser-send-esc-or-exit-fullscreen ()
  "Escape fullscreen status if browser current is fullscreen.
Otherwise send key 'esc' to browser."
  (interactive)
  (if eaf-fullscreen-p
      (eaf-call-async "execute_function" eaf--buffer-id "exit_fullscreen" "<escape>")
    (eaf-call-async "send_key" eaf--buffer-id "<escape>")))

(defun eaf-browser-is-loading ()
  "Return non-nil if current page is loading."
  (interactive)
  (when (and (string= eaf--buffer-app-name "browser")
             (string= (eaf-call-sync "call_function" eaf--buffer-id "page_is_loading") "True"))))

;; Update and load the theme
(defun eaf-get-theme-mode ()
  (format "%s"(frame-parameter nil 'background-mode)))

(defun eaf-get-theme-background-color ()
  (format "%s"(frame-parameter nil 'background-color)))

(defun eaf-get-theme-foreground-color ()
  (format "%s"(frame-parameter nil 'foreground-color)))

(eaf-setq eaf-emacs-theme-mode (eaf-get-theme-mode))

(eaf-setq eaf-emacs-theme-background-color (eaf-get-theme-background-color))

(eaf-setq eaf-emacs-theme-foreground-color (eaf-get-theme-foreground-color))

(advice-add 'load-theme :around #'eaf-monitor-load-theme)
(defun eaf-monitor-load-theme (orig-fun &optional arg &rest args)
  "Update `eaf-emacs-theme-mode' after execute `load-theme'."
  (apply orig-fun arg args)
  (eaf-setq eaf-emacs-theme-mode (eaf-get-theme-mode))
  (eaf-setq eaf-emacs-theme-background-color (eaf-get-theme-background-color))
  (eaf-setq eaf-emacs-theme-foreground-color (eaf-get-theme-foreground-color)))

(define-minor-mode eaf-pdf-outline-mode
  "EAF pdf outline mode."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "RET") 'eaf-pdf-outline-jump)
            (define-key map (kbd "q") 'quit-window)
            map))

(defun eaf-pdf-outline ()
  "Create PDF outline."
  (interactive)
  (let ((buffer-name (buffer-name (current-buffer)))
        (toc (eaf-call-sync "call_function" eaf--buffer-id "get_toc"))
        (page-number (string-to-number (eaf-call-sync "call_function" eaf--buffer-id "current_page"))))
    ;; Save window configuration before outline.
    (setq eaf-pdf-outline-window-configuration (current-window-configuration))

    ;; Insert outline content.
    (with-current-buffer (get-buffer-create  eaf-pdf-outline-buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert toc)
      (setq toc (mapcar (lambda (line)
                          (string-to-number (car (last (split-string line " ")))))
                        (butlast (split-string (buffer-string) "\n"))))
      (goto-line (seq-count (apply-partially #'>= page-number) toc))
      (set (make-local-variable 'eaf-pdf-outline-original-buffer-name) buffer-name)
      (let ((view-read-only nil))
        (read-only-mode 1))
      (eaf-pdf-outline-mode 1))

    ;; Popup ouline buffer.
    (pop-to-buffer eaf-pdf-outline-buffer-name)))

(defun eaf-pdf-outline-jump ()
  "Jump into specific page."
  (interactive)
  (let* ((line (thing-at-point 'line))
         (page-num (replace-regexp-in-string "\n" "" (car (last (s-split " " line))))))
    ;; Jump to page.
    (switch-to-buffer-other-window eaf-pdf-outline-original-buffer-name)
    (eaf-call-sync "call_function_with_args" eaf--buffer-id "jump_to_page_with_num" (format "%s" page-num))

    ;; Restore window configuration before outline operation.
    (when eaf-pdf-outline-window-configuration
      (set-window-configuration eaf-pdf-outline-window-configuration)
      (setq eaf-pdf-outline-window-configuration nil))))

(defun eaf-pdf-get-annots (page)
  "Return a map of annotations on PAGE.

The key is the annot id on PAGE."
  (eaf-call-sync "call_function_with_args" eaf--buffer-id "get_annots" (format "%s" page)))

(defun eaf-pdf-jump-to-annot (annot)
  "Jump to specifical pdf annot."
  (let ((rect (gethash "rect" annot))
        (page (gethash "page" annot)))
    (eaf-call-sync "call_function_with_args" eaf--buffer-id "jump_to_rect" (format "%s" page) rect)))

(defun eaf--get-current-desktop-name ()
  "Get current desktop name by `wmctrl'."
  (if (string-empty-p eaf-wm-name)
      (if (executable-find "wmctrl")
          ;; Get desktop name by command `wmctrl -m'.
          (cl-second (split-string (cl-first (split-string (shell-command-to-string "wmctrl -m") "\n")) ": "))
        ;; Otherwise notify user and return emptry string.
        (message "You need install wmctrl to get the name of desktop.")
        "")
    eaf-wm-name))

(defun eaf--activate-emacs-win32-window()
  "Use vbs activate Emacs win32 window."
  (let* ((activate-window-file-path
          (concat eaf-config-location "activate-window.vbs"))
         (activate-window-file-exists (file-exists-p activate-window-file-path)))
    (unless activate-window-file-exists
      (with-temp-file activate-window-file-path
        (insert "set WshShell = CreateObject(\"WScript.Shell\")\nWshShell.AppActivate Wscript.Arguments(0)")))
    (shell-command-to-string (format "cscript %s %s" activate-window-file-path (emacs-pid)))))

(defun eaf--activate-emacs-wsl-window()
  "Activate Emacs window running on Wsl."
  (eaf-call-async "activate_emacs_wsl_window" (frame-parameter nil 'name)))

(defun eaf--activate-emacs-linux-window (&optional buffer_id)
  "Activate Emacs window by `wmctrl'."
  (if (member (eaf--get-current-desktop-name) eaf-wm-focus-fix-wms)
      ;; When switch app focus in WM, such as, i3 or qtile.
      ;; Emacs window cannot get the focus normally if mouse in EAF buffer area.
      ;;
      ;; So we move mouse to frame bottom of Emacs, to make EAF receive input event.
      (eaf-call-async "execute_function" (or eaf--buffer-id buffer_id) "move_cursor_to_corner" (key-description (this-command-keys-vector)))

    ;; When press Alt + Tab in DE, such as KDE.
    ;; Emacs window cannot get the focus normally if mouse in EAF buffer area.
    ;;
    ;; So we use wmctrl activate on Emacs window after Alt + Tab operation.
    (if (executable-find "wmctrl")
        (shell-command-to-string (format "wmctrl -i -a $(wmctrl -lp | awk -vpid=$PID '$3==%s {print $1; exit}')" (emacs-pid)))
      (message "Please install wmctrl to active Emacs window."))))

(defun eaf--activate-emacs-mac-window()
  "Activate Emacs macOS window."
  (shell-command-to-string "open -a emacs"))

(defun eaf-activate-emacs-window(&optional buffer_id)
  "Activate Emacs window."
  (cond
   ((eaf--called-from-wsl-on-windows-p)
    (eaf--activate-emacs-wsl-window))
   ((memq system-type '(cygwin windows-nt ms-dos))
    (eaf--activate-emacs-win32-window))
   ((eq system-type 'darwin)
    (eaf--activate-emacs-mac-window))
   ((eq system-type 'gnu/linux)
    (eaf--activate-emacs-linux-window buffer_id))))

(defun eaf-elfeed-open-url ()
  "Display the currently selected item in an eaf buffer."
  (interactive)
  (if (featurep 'elfeed)
      (let ((entry (elfeed-search-selected :ignore-region)))
        (require 'elfeed-show)
        (when (elfeed-entry-p entry)
          ;; Move to next feed item.
          (elfeed-untag entry 'unread)
          (elfeed-search-update-entry entry)
          (unless elfeed-search-remain-on-entry (forward-line))

          ;; Open elfeed item in other window,
          ;; and scroll EAF browser content by command `scroll-other-window'.
          (delete-other-windows)
          (pcase eaf-elfeed-split-direction
            ("below"
             (split-window-no-error nil 30 'up)
             (eaf--select-window-by-direction "down")
             (eaf-open-browser (elfeed-entry-link entry))
             (eaf--select-window-by-direction "up"))
            ("right"
             (split-window-no-error nil 60 'right)
             (eaf--select-window-by-direction "right")
             (eaf-open-browser (elfeed-entry-link entry))
             (eaf--select-window-by-direction "left")))
          ))
    (message "Please install elfeed first.")))

(defun eaf--select-window-by-direction (direction)
  "Select the most on the side according to the direction."
  (ignore-errors
    (dotimes (_ 50)
      (pcase direction
        ("left" (windmove-left))
        ("right" (windmove-right))
        ("up" (windmove-up))
        ("below" (windmove-down))
        ))))

(defun eaf--change-default-directory (directory)
  "Change default directory to DIRECTORY."
  (when (file-accessible-directory-p (or (file-name-directory directory) directory))
    (setq-local default-directory directory)))

;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eaf-get-view-info ()
  (let* ((window-allocation (eaf-get-window-allocation (selected-window)))
         (x (nth 0 window-allocation))
         (y (nth 1 window-allocation))
         (w (nth 2 window-allocation))
         (h (nth 3 window-allocation)))
    (format "%s:%s:%s:%s:%s" eaf--buffer-id x y w h)))

(defun eaf-generate-keymap-doc ()
  "This command use for generate keybindings document Wiki."
  (interactive)
  (let ((vars (list 'eaf-browser-keybinding
                    'eaf-browser-caret-mode-keybinding
                    'eaf-pdf-viewer-keybinding
                    'eaf-video-player-keybinding
                    'eaf-js-video-player-keybinding
                    'eaf-image-viewer-keybinding
                    'eaf-music-player-keybinding
                    'eaf-system-monitor-keybinding
                    'eaf-terminal-keybinding
                    'eaf-camera-keybinding
                    'eaf-mindmap-keybinding
                    'eaf-jupyter-keybinding
                    )))
    (erase-buffer)
    (insert "**** Entire document automatically generated by command =eaf-generate-keymap-doc=.\n\n")
    (insert "* Overview
  Each EAF App has its own set of keybindings. Their default bindings are listed below. You can also see this list by executing =(describe-mode)= or =C-h m= within an EAF buffer.

  You can customize them very easily with the =eaf-bind-key= function: find the corresponding *Keybinding Variable*, and add the something similar to the following to =.emacs=
  #+BEGIN_SRC emacs-lisp
    (eaf-bind-key scroll_up \"C-n\" eaf-pdf-viewer-keybinding)
  #+END_SRC
  To *unbind* an existing keybinding, use the following:
  #+begin_src emacs-lisp
    (eaf-bind-key nil \"C-n\" eaf-pdf-viewer-keybinding)
  #+end_src

* Global keybindings
  | Key   | Event                       |
  |-------+-----------------------------|
  | C-h m | eaf-describe-bindings       |
  | C-c b | eaf-open-bookmark           |
  | C-c e | eaf-open-external           |
  | C-c i | eaf-import-chrome-bookmarks |
  | M-/   | eaf-get-path-or-url         |
  | M-'   | eaf-toggle-fullscreen       |
  | M-[   | eaf-share-path-or-url       |

* Browser Edit Mode
  | Key     | Event                              |
  |---------+------------------------------------|
  | C-c C-c | eaf-edit-buffer-confirm            |
  | C-c C-k | eaf-edit-buffer-cancel             |
  | C-c C-t | eaf-edit-buffer-switch-to-org-mode |

")
    (dolist (var vars)
      (insert (format "* %s\n" (get var 'variable-documentation)))
      (insert (format "  *Keybinding Variable*: =%s=\n" (symbol-name var)))
      (insert "| Key | Event |\n")
      (insert "|-----+------|\n")
      ;; NOTE: `standard-value' use for fetch origin value of keybinding variable.
      ;; Otherwise, developer's personal config will dirty document.
      (dolist (element (eval (car (get var 'standard-value))))
        (insert (format "| %s | %s |\n" (car element) (cdr element))))
      (insert "\n"))))

(defun eaf--browser-export-text (buffer-name html-text)
  (let ((eaf-export-text-buffer (get-buffer-create buffer-name)))
    (with-current-buffer eaf-export-text-buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert html-text)
      (goto-char (point-min))
      (read-only-mode 1))
    (switch-to-buffer eaf-export-text-buffer)
    ))

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
        (eaf-call-async "scroll_other_buffer" (eaf-get-view-info) "up"
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
        (eaf-call-async "scroll_other_buffer" (eaf-get-view-info) "down"
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
        (eaf-call-async "scroll_other_buffer" (eaf-get-view-info)
                        (if (string-equal direction "up") "up" "down")
                        (if line "line" "page"))
        (other-window -1))
    (other-window -1)
    (apply orig-fun direction line args)))

(defun eaf--buffer-file-p ()
  "Determine if the file opened at the current buffer be opened by EAF."
  (let ((ext (when (and buffer-file-name
                        (file-exists-p buffer-file-name))
               (file-name-extension buffer-file-name))))
    (and ext
         (member (downcase ext) (append
                                 eaf-pdf-extension-list
                                 eaf-markdown-extension-list
                                 eaf-image-extension-list
                                 eaf-video-extension-list
                                 eaf-org-extension-list
                                 eaf-mindmap-extension-list
                                 eaf-office-extension-list)))))

(defun eaf-open-this-buffer ()
  "Try to open the current buffer using EAF, if possible."
  (interactive)
  (if (eaf--buffer-file-p)
      (eaf-open buffer-file-name)
    (user-error "[EAF] Current buffer is not supported by EAF!")))

(defun eaf--find-file-ext-p (ext)
  "Determine if file extension EXT can be opened by EAF directly by `find-file'.

You can configure a blacklist using `eaf-find-file-ext-blacklist'"
  (and ext
       (member (downcase ext) (append
                               eaf-pdf-extension-list
                               eaf-video-extension-list
                               eaf-image-extension-list
                               eaf-mindmap-extension-list))
       (not (member ext eaf-find-file-ext-blacklist))))

;; Make EAF as default app for supported extensions.
;; Use `eaf-open' in `find-file'
(defun eaf--find-file-advisor (orig-fn file &rest args)
  "Advisor of `find-file' that opens EAF supported file using EAF.

It currently identifies PDF, videos, images, and mindmap file extensions."
  (let ((fn (if (commandp 'eaf-open)
                #'(lambda (file)
                    (eaf-open file))
              orig-fn))
        (ext (file-name-extension file)))
    (if (eaf--find-file-ext-p ext)
        (apply fn file nil)
      (apply orig-fn file args))))
(advice-add #'find-file :around #'eaf--find-file-advisor)

;; Use `eaf-open' in `dired-find-file' and `dired-find-alternate-file'
(defun eaf--dired-find-file-advisor (orig-fn)
  "Advisor of `dired-find-file' and `dired-find-alternate-file' that opens EAF supported file using EAF.

It currently identifies PDF, videos, images, and mindmap file extensions."
  (dolist (file (dired-get-marked-files))
    (let ((fn (if (commandp 'eaf-open)
                  #'(lambda (file)
                      (eaf-open file))
                orig-fn))
          (ext (file-name-extension file)))
      (if (eaf--find-file-ext-p ext)
          (apply fn file nil)
        (funcall-interactively orig-fn)))))
(advice-add #'dired-find-file :around #'eaf--dired-find-file-advisor)
(advice-add #'dired-find-alternate-file :around #'eaf--dired-find-file-advisor)

(provide 'eaf)

;;; eaf.el ends here
