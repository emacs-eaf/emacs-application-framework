;;; eaf.el --- Emacs application framework  -*- lexical-binding: t; -*-

;; Filename: eaf.el
;; Description: Emacs application framework
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: Fri Apr  8 12:23:33 2022 (-0400)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/emacs-eaf/emacs-application-framework
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
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'bookmark)

(declare-function straight--symlink-recursively "straight")

(define-obsolete-function-alias 'eaf-setq 'setq "Version 0.5, Commit d8abd23"
  "See https://github.com/emacs-eaf/emacs-application-framework/issues/734.
for more information.

In summary, the EAF's python applications can now access users' customized
variables with the get_emacs_var module, eliminating the need to store
them in `eaf-var-list' and thus, `eaf-setq' is no longer necessary.")

(defvar eaf-app-binding-alist '()
  "Mapping app names to keybinding variables.

Any new app should add the its name and the corresponding
keybinding variable to this list.")

(defvar eaf-app-module-path-alist '())

(defvar eaf-app-bookmark-handlers-alist '()
  "Mapping app names to bookmark handler functions.

A bookmark handler function is used as
`bookmark-make-record-function' and should follow its spec.")

(defvar eaf-app-bookmark-restore-alist '())


(defvar eaf-preview-display-function-alist '()
  "Preview display function.")

(defvar eaf-app-extensions-alist '()
  "Mapping app names to extension list variables.

A new app can use this to configure extensions which should
handled by it.")

(defvar eaf-app-hook-alist '()
  "Application running hook.")

(defvar eaf-build-dir (file-name-directory (locate-library "eaf")))
(defvar eaf-source-dir (file-name-directory (file-truename (concat eaf-build-dir "eaf.el"))))

;; Helper functions for generating the defcustom entry for the list of apps
(defun eaf--alist-to-defcustom-const (entry)
  "Map an alist for an app to an entry for the defcustom set"
  `(const :tag ,(cdr (assoc 'name entry)) ,(car entry)))

(defun eaf--json-to-defcustom-set ()
  "Generate the 'set choice for the defcustom entry"
  (let ((apps-alist
         (with-temp-buffer
           (insert-file-contents (concat eaf-build-dir "applications.json"))
           (goto-char 0)
           (json-parse-buffer :object-type 'alist))))
    (cons 'set (mapcar 'eaf--alist-to-defcustom-const (cdr apps-alist)))))

(defcustom eaf-apps-to-install nil
  "List of applications to install"
  :group 'eaf
  :type (eaf--json-to-defcustom-set))

(defun eaf-add-subdirs-to-load-path (search-dir)
  (interactive)
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             (cl-remove-if
              #'(lambda (subdir)
                  (or
                   (not (file-directory-p (concat dir subdir)))
                   (member subdir '("." ".."
                                    "dist" "node_modules" "__pycache__"
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github"))))
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))
          (add-to-list 'load-path subdir-path t))

        (eaf-add-subdirs-to-load-path subdir-path)))))

;; Add EAF app directories where .el exists to `load-path'.
(eaf-add-subdirs-to-load-path eaf-build-dir)

(require 'eaf-epc)

(defgroup eaf nil
  "Emacs Application Framework."
  :group 'applications)

(defcustom eaf-mode-hook '()
  "EAF mode hook."
  :type 'hook)

(defcustom eaf-mode-line-format mode-line-format
  "`mode-line-format' used by eaf-mode.")

(defcustom eaf-frame-title-format frame-title-format
  "`frame-title-format' used by eaf-mode.")

(defvar eaf-mode-map*
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-h m") #'eaf-describe-bindings)
    (define-key map (kbd "C-o") #'eaf-duplicate-current-buffer)
    (define-key map [remap describe-bindings] #'eaf-describe-bindings)
    (define-key map (kbd "C-c b") #'eaf-open-bookmark)
    (define-key map (kbd "C-c i") #'eaf-import-chrome-bookmarks)
    (define-key map (kbd "C-c e") #'eaf-open-external)
    (define-key map (kbd "C-h k") #'describe-key)
    (define-key map (kbd "C-h v") #'describe-variable)
    (define-key map (kbd "C-h f") #'describe-function)
    (define-key map (kbd "C-h V") #'apropos-variable)
    (define-key map (kbd "C-h F") #'apropos-function)
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

(defvar eaf-server nil
  "The EAF Server.")

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
  ;; Let eaf can set its mode-line and frame-title.
  (setq-local mode-line-format eaf-mode-line-format)
  (setq-local frame-title-format eaf-frame-title-format)
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

(defun eaf--start-epc-server ()
  "Function to start the EPC server."
  (unless eaf-server
    (setq eaf-server
          (eaf-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (eaf-epc-define-method mngr 'eval-in-emacs 'eaf--eval-in-emacs)
               (eaf-epc-define-method mngr 'get-emacs-func-result 'eaf--get-emacs-func-result)
               (eaf-epc-define-method mngr 'get-emacs-var 'eaf--get-emacs-var)
               (eaf-epc-define-method mngr 'get-emacs-vars 'eaf--get-emacs-vars)
               ))))
    (if eaf-server
        (setq eaf-server-port (process-contact eaf-server :service))
      (error "[EAF] eaf-server failed to start")))
  eaf-server)

(defun eaf--get-emacs-func-result (sexp-string)
  (eval (read sexp-string)))

(defun eaf--eval-in-emacs (sexp-string)
  (eval (read sexp-string))
  ;; Return nil to avoid epc error `Got too many arguments in the reply'.
  nil)

(defun eaf--get-emacs-var (var-name)
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun eaf--get-emacs-vars (&rest vars)
  (mapcar #'eaf--get-emacs-var vars))

(defun get-emacs-face-foregrounds (&rest faces)
  (mapcar #'(lambda (face-name) (eaf-color-name-to-hex (face-attribute (intern face-name) :foreground nil 'default))) faces))

(defun eaf-color-int-to-hex (int)
  (substring (format (concat "%0" (int-to-string 4) "X") int) (- 2)))

(defun eaf-color-name-to-hex (color)
  (let ((components (x-color-values color)))
    (concat "#"
            (eaf-color-int-to-hex (nth 0 components))
            (eaf-color-int-to-hex (nth 1 components))
            (eaf-color-int-to-hex (nth 2 components)))))

(defvar eaf-epc-process nil)

(defvar eaf-internal-process nil)
(defvar eaf-internal-process-prog nil)
(defvar eaf-internal-process-args nil)

(defvar eaf--first-start-app-buffers nil
  "Contains a list of '(buffer-url buffer-app-name buffer-args).")

(defvar eaf-last-frame-width 0)

(defvar eaf-last-frame-height 0)

(defcustom eaf-name "*eaf*"
  "Name of EAF buffer."
  :type 'string)

(defcustom eaf-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python interpreter used to run eaf.py."
  :type 'string)

(defcustom eaf-config-location (expand-file-name (locate-user-emacs-file "eaf/"))
  "Directory where eaf will store configuration files."
  :type 'directory)

(defcustom eaf-marker-letters "ASDFHJKLWEOPCNM"
  ""
  :type 'string)

(defcustom eaf-marker-quit-keys " "
  ""
  :type 'string)

(defcustom eaf-marker-fontsize 11.5
  "Fontsize (px) of marker."
  :type 'number)

(defcustom eaf-buffer-background-color "#000000"
  ""
  :type 'string)

(defcustom eaf-webengine-show-hover-link nil
  ""
  :type 'boolean)

(defcustom eaf-find-file-ext-blacklist '("md" "org" "html" "htm" "epub")
  "A blacklist of extensions to avoid when opening `find-file' file using EAF."
  :type 'cons)

(defcustom eaf-proxy-host ""
  "Proxy Host used by EAF Browser."
  :type 'string)

(defcustom eaf-proxy-port ""
  "Proxy Port used by EAF Browser."
  :type 'string)

(defcustom eaf-proxy-type ""
  "Proxy Type used by EAF Browser.  The value is either \"http\" or \"socks5\"."
  :type 'string)

(defcustom eaf-webengine-default-zoom 1.0
  "Set the default zoom factor for EAF Browser."
  :type 'float)

(defcustom eaf-webengine-zoom-step 0.1
  "Set the zoom step factor for EAF Browser."
  :type 'float)

(defcustom eaf-webengine-scroll-step 400
  "Set the scroll step for EAF Browser, increase/decrease for bigger/smaller steps."
  :type 'float)

(defcustom eaf-webengine-pc-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/106.0.0.0 Safari/537.36"
  "Simulate a PC User-Agent for EAF Browser."
  :type 'string)

(defcustom eaf-webengine-phone-user-agent "Mozilla/5.0 (iPhone; CPU iPhone OS 13_2_3 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/13.0.3 Mobile/15E148 Safari/604.1"
  "Simulate a Phone User-Agent for EAF Browser."
  :type 'string)

(defcustom eaf-webengine-font-family ""
  "Set font family for EAF Browser."
  :type 'string)

(defcustom eaf-webengine-font-size 16
  "Set font size for EAF Browser."
  :type 'integer)

(defcustom eaf-webengine-fixed-font-family ""
  "Set fixed font family for EAF Browser."
  :type 'string)

(defcustom eaf-webengine-fixed-font-size 16
  "Set fixed font size for EAF Browser."
  :type 'integer)

(defcustom eaf-webengine-serif-font-family ""
  "Set serif font family for EAF Browser."
  :type 'string)

(defcustom eaf-webengine-enable-plugin t
  "If non-nil, enable QtWebEngine plugins for EAF Browser."
  :type 'boolean)

(defcustom eaf-webengine-enable-javascript t
  "If non-nil, enable javascript for EAF Browser."
  :type 'boolean)

(defcustom eaf-webengine-enable-javascript-access-clipboard t
  "If non-nil, enable javascript access user clipboard for EAF Browser."
  :type 'boolean)

(defcustom eaf-webengine-enable-scrollbar nil
  "If non-nil, enable scroll bar for EAF Browser."
  :type 'boolean)

(defcustom eaf-webengine-unknown-url-scheme-policy "AllowUnknownUrlSchemesFromUserInteraction"
  "Allowed options: DisallowUnknownUrlSchemes, AllowUnknownUrlSchemesFromUserInteraction, or AllowAllUnknownUrlSchemes."
  :type 'string)

(defcustom eaf-webengine-download-path "~/Downloads"
  "Set the download path for EAF Browser."
  :type 'string)

(defcustom eaf-enable-debug nil
  "If you got segfault error, please turn this option.
Then EAF will start by gdb, please send new issue with `*eaf*' buffer content when next crash."
  :type 'boolean)

(defcustom eaf-kill-process-after-last-buffer-closed nil
  "Kill eaf process when last eaf buffer closed, default is nil.

If you don't want EAF process exist when all EAF buffer closed, turn on this option.

Turn off this option will improve EAF new page creation speed."
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
    "Xfwm4"                             ;Xfce
    "wlroots wm" ;hyprland(may work in other Wayland compositors based on wlroots)
    )
  "Set mouse cursor to frame bottom in these wms, to make EAF receive input event.

EAF confirms that the desktop environment or window manager you can work includes:
KDE, Gnome2, Gnome3, Mate, Xfce, LXDE, Sway, i3, QTile, Xpra, EXWM.

In which, KDE, Gnome2, Gnome3, Mate, Xfce, LXDE, Sway works well default,
i3, QTile, Xpra, EXWM need move cursor to corner to get focus of Window manager.

If your window manager can't receive input event, you can try add `NAME' of command `wmctrl -m' to this list.

Please send PR if it works.
Please fill an issue if it still doesn't work."
  :type 'list)

(defcustom eaf-start-python-process-when-require t
  "Start EAF python process when require `eaf', default is turn on.

Turn on this option will improve start speed."
  :type 'boolean)

(defcustom eaf-byte-compile-apps nil
  "If this option turn on, EAF will byte-compile elisp code.")

(defcustom eaf-clean-duplicate-buffers t
  "Clean duplicate file manager buffers."
  :type 'boolean)

(defcustom eaf-goto-right-after-close-buffer nil
  "EAF will switch to right buffer after close current EAF buffer if this option is enable.

And you need re-implement `eaf-goto-right-tab' self."
  :type 'boolean)

(defcustom eaf-duplicate-buffer-survival-time 60
  "If file manager buffer not show in current frame, and existence time exceeds than 60 seconds,
EAF will remove the duplicate file manager buffer."
  :type 'integer)

(defvar eaf--monitor-configuration-p t
  "When this variable is non-nil, `eaf-monitor-configuration-change' executes.
This variable is used to open buffer in backend and avoid graphics blink.

EAF call python method `new_buffer' to create EAF application buffer.
EAF call python method `update_views' to create EAF application view.

Python process only create application view when Emacs window or buffer state change.")

(defvar eaf-fullscreen-p nil
  "When non-nil, EAF will intelligently hide modeline as necessray.")

(defvar eaf-buffer-title-format "%s")

(defvar-local eaf--bookmark-title nil)

(defmacro eaf-for-each-eaf-buffer (&rest body)
  "A syntactic sugar to loop through each EAF buffer and evaluat BODY.

Within BODY, `buffer' can be used to"
  `(dolist (buffer (eaf--get-eaf-buffers))
     (with-current-buffer buffer
       ,@body)))

(defun eaf--bookmark-make-record ()
  "Create a EAF bookmark.

The bookmark will try to recreate EAF buffer session.
For now only EAF browser app is supported."
  (let ((handler (cdr
                  (assoc eaf--buffer-app-name
                         eaf-app-bookmark-handlers-alist))))
    (when handler
      (funcall handler))))

(defun eaf--bookmark-restore (bookmark)
  "Restore EAF buffer according to BOOKMARK."
  (let ((app (cdr (assq 'eaf-app bookmark))))
    (funcall (cdr (assoc app eaf-app-bookmark-restore-alist)) bookmark)))

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

(defvar eaf--existing-bookmarks nil
  "Existing bookmarks in Emacs.
A hashtable, key is url and value is title.")

(defun eaf--load-existing-bookmarks()
  "Load existing bookmarks."
  (let ((bookmarks (make-hash-table :test 'equal)))
    (dolist (bm bookmark-alist)
      (let* ((name (car bm))
             (file (bookmark-get-filename name)))
        (puthash file name bookmarks)))
    bookmarks))

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
  (eaf-deferred-chain
    (eaf-epc-call-deferred eaf-epc-process (read method) args)))

(defun eaf-call-sync (method &rest args)
  "Call Python EPC function METHOD and ARGS synchronously."
  (eaf-epc-call-sync eaf-epc-process (read method) args))

(defun eaf--called-from-wsl-on-windows-p ()
  "Check whether eaf is called by Emacs on WSL and is running on Windows."
  (and (eq system-type 'gnu/linux)
       (string-match-p ".exe" eaf-python-command)))

(defun eaf-get-emacs-xid (frame)
  "Get Emacs FRAME xid."
  (if (eaf--called-from-wsl-on-windows-p)
      (eaf-call-sync "get_emacs_wsl_window_id")
    (frame-parameter frame 'window-id)))

(defun eaf--build-process-environment ()
  ;; Turn on DEBUG info when `eaf-enable-debug' is non-nil.
  (let ((environments (seq-filter
                       (lambda (var)
                         (and (not (string-match-p "QT_SCALE_FACTOR" var))
                              (not (string-match-p "QT_SCREEN_SCALE_FACTOR" var))))
                       process-environment)))
    (when eaf-enable-debug
      (add-to-list 'environments "QT_DEBUG_PLUGINS=1" t))

    (unless (eq system-type 'darwin)
      (add-to-list 'environments
                   (cond
                    ((eaf-emacs-running-in-wayland-native)
                     ;; Wayland native need to set QT_AUTO_SCREEN_SCALE_FACTOR=1
                     ;; otherwise Qt window only have half of screen.
                     "QT_AUTO_SCREEN_SCALE_FACTOR=1")
                    (t
                     ;; XWayland need to set QT_AUTO_SCREEN_SCALE_FACTOR=0
                     ;; otherwise Qt which explicitly force high DPI enabling get scaled TWICE.
                     "QT_AUTO_SCREEN_SCALE_FACTOR=0"))
                   t)

      (add-to-list 'environments "QT_FONT_DPI=96" t)

      ;; Make sure EAF application scale support 4k screen.
      (add-to-list 'environments "QT_SCALE_FACTOR=1" t)

      ;; Use XCB for input event transfer.
      ;; Only enable this option on Linux platform.
      (when (and (eq system-type 'gnu/linux)
                 (not (eaf-emacs-running-in-wayland-native)))
        (add-to-list 'environments "QT_QPA_PLATFORM=xcb" t)))
    environments))

(defvar eaf-start-process-hook nil)

(defun eaf-start-process ()
  "Start EAF process if it isn't started."
  (unless (eaf-epc-live-p eaf-epc-process)
    ;; start epc server and set `eaf-server-port'
    (eaf--start-epc-server)
    (let* ((eaf-args (append
                      (list eaf-python-file)
                      (eaf-get-render-size)
                      (list (number-to-string eaf-server-port))
                      ))
           environments)

      ;; Folow system DPI.
      (setq environments (eaf--build-process-environment))

      ;; Set process arguments.
      (if eaf-enable-debug
          (progn
            (setq eaf-internal-process-prog "gdb")
            (setq eaf-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" eaf-python-command) eaf-args)))
        (setq eaf-internal-process-prog eaf-python-command)
        (setq eaf-internal-process-args eaf-args))

      ;; Start python process.
      (let ((process-connection-type (not (eaf--called-from-wsl-on-windows-p)))
            (process-environment environments))
        (setq eaf-internal-process (apply 'start-process eaf-name eaf-name eaf-internal-process-prog eaf-internal-process-args)))
      (set-process-query-on-exit-flag eaf-internal-process nil)))

  ;; Run start process hooks.
  (run-hooks 'eaf-start-process-hook))

(run-with-idle-timer
 1 nil
 #'(lambda ()
     ;; Start EAF python process when load `eaf'.
     ;; It will improve start speed.
     (when eaf-start-python-process-when-require
       (eaf-start-process))))

(defvar eaf-stop-process-hook nil)

(defun eaf-kill-process (&optional restart)
  "Stop EAF process and kill all EAF buffers.

If RESTART is non-nil, cached URL and app-name will not be cleared."
  (interactive)

  ;; Run stop process hooks.
  (run-hooks 'eaf-stop-process-hook)

  (unless restart
    ;; Clear active buffers
    (setq eaf--first-start-app-buffers nil)
    ;; Remove all EAF related hooks since the EAF process is stopped.
    (remove-hook 'kill-buffer-hook #'eaf--monitor-buffer-kill)
    (remove-hook 'kill-emacs-hook #'eaf--monitor-emacs-kill)
    (remove-hook 'after-save-hook #'eaf--org-preview-monitor-buffer-save)
    (remove-hook 'kill-buffer-hook #'eaf--org-preview-monitor-kill)
    (remove-hook 'window-size-change-functions #'eaf-monitor-window-size-change)
    (remove-hook 'window-configuration-change-hook #'eaf-monitor-configuration-change))

  ;; Set `eaf-fullscreen-p'.
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

(defalias 'eaf-stop-process #'eaf-kill-process)

(defun eaf--kill-python-process ()
  "Kill EAF background python process."
  (interactive)
  (when (eaf-epc-live-p eaf-epc-process)
    ;; Cleanup before exit EAF server process.
    (eaf-call-async "cleanup")
    ;; Delete EAF server process.
    (eaf-epc-stop-epc eaf-epc-process)
    ;; Kill *eaf* buffer.
    (when (get-buffer eaf-name)
      (kill-buffer eaf-name))
    (message "[EAF] Process terminated.")))

(defun eaf--kill-devtools-buffers ()
  (dolist (buffer (eaf--get-eaf-buffers))
    (with-current-buffer buffer
      (when (string-prefix-p "DevTools - " (buffer-name buffer))
        (kill-buffer buffer)))))

(defun eaf-restart-process ()
  "Stop and restart EAF process."
  (interactive)
  ;; We need kill debug page first.
  (eaf--kill-devtools-buffers)

  ;; Record reset buffers to `eaf--first-start-app-buffers'.
  (setq eaf--first-start-app-buffers nil)
  (eaf-for-each-eaf-buffer
   (push `(,eaf--buffer-url ,eaf--buffer-app-name ,eaf--buffer-args) eaf--first-start-app-buffers))

  ;; Stop EAF process.
  (eaf-kill-process t)

  ;; Start EAF process, EAF will restore page in `eaf--first-start-app-buffers'.
  (eaf-start-process))

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

(defun eaf-copy-to-clipboard (string)
  (if (version< "29.0" emacs-version)
      (progn
        (kill-new string)
        string)
    (kill-new string)))

(defun eaf-get-path-or-url ()
  "Get the current file path or web URL.

When called interactively, copy to ‘kill-ring’."
  (interactive)
  (if (derived-mode-p 'eaf-mode)
      (if (called-interactively-p 'any)
          (message "%s" (eaf-copy-to-clipboard (eaf-call-sync "execute_function" eaf--buffer-id "get_url")))
        (eaf-call-sync "execute_function" eaf--buffer-id "get_url"))
    (user-error "This command can only be called in an EAF buffer!")))

(defun eaf-toggle-fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (eaf-call-async "eval_function" eaf--buffer-id "toggle_fullscreen" (key-description (this-command-keys-vector))))

(defun eaf--enter-fullscreen-request ()
  "Entering fullscreen use Emacs frame's size."
  (message "[EAF] Enter fullscreen")
  (setq-local eaf-fullscreen-p t)
  (eaf-monitor-configuration-change)
  (when (and eaf-browser-fullscreen-move-cursor-corner
             (string= eaf--buffer-app-name "browser"))
    (eaf-call-async "eval_function" eaf--buffer-id "move_cursor_to_corner" (key-description (this-command-keys-vector)))))

(defun eaf--exit_fullscreen_request ()
  "Exit fullscreen."
  (message "[EAF] Exit fullscreen")
  (setq-local eaf-fullscreen-p nil)
  (eaf-monitor-configuration-change))

(defun eaf--make-py-proxy-function (fun)
  "Define elisp command which can call Python function string FUN."
  (let ((sym (intern (format "eaf-py-proxy-%s" fun))))
    (unless (fboundp sym)
      (defalias sym
        (lambda nil
          (interactive)
          ;; Ensure this is only called from EAF buffer
          (if (derived-mode-p 'eaf-mode)
              (eaf-call-async "eval_function" eaf--buffer-id fun (key-description (this-command-keys-vector)))
            (message "%s command can only be called in an EAF buffer!" sym)))
        (format
         "Proxy function to call \"%s\" on the Python side.

Use `eaf-execute-app-cmd' if you want to execute this command programmatically.
Please ONLY use `eaf-bind-key' and use the unprefixed command name (\"%s\")
to edit EAF keybindings!" fun fun)))
    sym))

(defun eaf--make-js-proxy-function (fun &optional args)
  "Define elisp command which can call JavaScript function string FUN."
  (let ((sym (intern (format "eaf-js-proxy-%s" fun))))
    (unless (fboundp sym)
      (defalias sym
        (lambda nil
          (interactive)
          (unless args
            (setq args ""))
          ;; Ensure this is only called from EAF buffer
          (when (derived-mode-p 'eaf-mode)
            (eaf-call-async "eval_js_function" eaf--buffer-id (string-trim-left fun "js_") args)
            ))
        (format
         "Proxy function to call \"%s\" on the JavaScript side.

Please ONLY use `eaf-bind-key' and use the unprefixed command name (\"%s\")
to edit EAF keybindings!" fun fun)))
    sym))

(defun eaf--gen-keybinding-map (keybinding &optional no-inherit-eaf-mode-map*)
  "Configure the `eaf-mode-map' from KEYBINDING, one of the eaf-.*-keybinding variables."
  (setq eaf-mode-map
        (let ((map (make-sparse-keymap)))
          (unless no-inherit-eaf-mode-map*
            (set-keymap-parent map eaf-mode-map*))
          (cl-loop for (key . fun) in (reverse keybinding)
                   do (define-key map (kbd key)
                                  (cond
                                   ;; If command is normal symbol, just call it directly.
                                   ((symbolp fun)
                                    fun)

                                   ;; If command is string and include - , it's elisp function, use `intern' build elisp function from function name.
                                   ((string-match "-" fun)
                                    (intern fun))

                                   ;; If command prefix with js_, call JavaScript function directly.
                                   ((string-prefix-p "js_" fun)
                                    (eaf--make-js-proxy-function fun))

                                   ;; If command is not built-in function and not include char '-'
                                   ;; it's command in python side, build elisp proxy function to call it.
                                   (t
                                    (eaf--make-py-proxy-function fun))
                                   ))
                   finally return map))))

(defun eaf--get-app-bindings (app-name)
  "Get the specified APP-NAME keybinding.

Every app has its name and the corresponding
keybinding variable to eaf-app-binding-alist."
  (symbol-value
   (cdr (assoc app-name eaf-app-binding-alist))))

(defun eaf--get-app-module-path (app-name)
  (symbol-value
   (cdr (assoc app-name eaf-app-module-path-alist))))

(defun eaf--get-app-hook (app-name)
  (funcall
   (cdr (assoc app-name eaf-app-hook-alist))))

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

      ;; Don't promt user when exist EAF python process.
      (setq-local confirm-kill-processes nil)

      (when (file-accessible-directory-p url-directory)
        (setq-local default-directory url-directory)

        ;; NOTE:
        ;; Don't set buffer `buffer-file-name' here.
        ;; Otherwise, markdown or org previewer will switch to EAF preview buffer instead open real file.
        )
      ;; `eaf-buffer-url' should record full path of url, otherwise `eaf-open' will open duplicate PDF tab for same url.
      (set (make-local-variable 'eaf--buffer-url) url)
      (set (make-local-variable 'eaf--buffer-app-name) app-name)
      (set (make-local-variable 'eaf--buffer-args) args)
      (run-hooks (intern (format "eaf-%s-hook" app-name)))
      (setq mode-name (concat "EAF/" app-name)))
    eaf-buffer))

(defun eaf-monitor-window-size-change (frame)
  "Delay some time and run `eaf-try-adjust-view-with-frame-size' to compare with Emacs FRAME size."
  (when (eaf-epc-live-p eaf-epc-process)
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

(defun eaf--buffer-y-position-adjust (frame)
  "Adjust the y position of EAF buffers for macOS"
  (if (eq system-type 'darwin)
      (+ (eaf--frame-top frame) (eaf--frame-internal-height frame))
    0))

(defun eaf-emacs-not-use-reparent-technology ()
  "When Emacs running in macOS、Wayland native or terminal environment,
we can't use 'cross-process reparent' technicality like we does in X11, XWayland or Windows.

In this situation, we use 'stay on top' technicality that show EAF window when Emacs get focus, hide EAF window when Emacs lost focus.

'Stay on top' technicality is not perfect like 'cross-process reparent' technicality,
provide at least one way to let everyone experience EAF. ;)"
  (or (eq system-type 'darwin)              ;macOS
      (eaf-emacs-running-in-wayland-native) ;Wayland native
      (not (display-graphic-p))             ;Terminal emulator
      ))

(defun eaf-emacs-running-in-wayland-native ()
  (eq window-system 'pgtk))

(eval-when-compile
  (when (eaf-emacs-not-use-reparent-technology)
    (defvar eaf--topmost-switch-to-python nil
      "Record if Emacs should switch to Python process.")

    (defun eaf--topmost-focus-change ()
      "Manage Emacs's focus change."
      (let* ((front (cond ((eq system-type 'darwin)
                           (shell-command-to-string "app-frontmost --name"))
                          ((string-equal (getenv "XDG_CURRENT_DESKTOP") "sway")
                           (if (executable-find "jshon")
                               (shell-command-to-string (concat eaf-build-dir "swaymsg-treefetch/swaymsg-focusfetcher.sh"))
                             (message "Please install jshon for swaywm support.")))
                          ((string-equal (getenv "XDG_CURRENT_DESKTOP") "Hyprland")
                           (gethash "class" (json-parse-string (shell-command-to-string "hyprctl -j activewindow"))))
                          (t
                           (require 'dbus)
                           (dbus-call-method :session "org.gnome.Shell" "/org/eaf/wayland" "org.eaf.wayland" "get_active_window" :timeout 1000))))
             (front-app-name (string-trim front)))
        (cond
         ((member front-app-name (list "Python" "python3"))
          (setq eaf--topmost-switch-to-python t))
         ((or (string-equal (string-replace "." "-" front)
                            eaf--emacs-program-name)
              (string-match-p (regexp-quote front-app-name)
                              eaf--emacs-program-name))
          (if eaf--topmost-switch-to-python
              (setq eaf--topmost-switch-to-python nil)
            (run-with-timer 0.1 nil #'eaf--topmost-focus-update)))
         (t (eaf--topmost-focus-out)))))

    (defun eaf--topmost-focus-update ()
      "Hide all eaf buffers, and then display new eaf buffers at front."
      (eaf--topmost-focus-out)
      (when (frame-focus-state)
        (dolist (window (window-list (selected-frame)))
          (with-current-buffer (window-buffer window)
            (when (derived-mode-p 'eaf-mode)
              (eaf-call-async "show_buffer_view" eaf--buffer-id))))))

    (defun eaf--topmost-focus-out ()
      "Prepare the screenshot and hide all eaf buffers."
      (dolist (frame (frame-list))
        (dolist (window (window-list frame))
          (with-current-buffer (window-buffer window)
            (when (derived-mode-p 'eaf-mode)
              (eaf--clip-image window)
              (eaf-call-sync "hide_buffer_view" eaf--buffer-id))))))

    (defun eaf--clip-image (window)
      "Clip the image of the qwidget."
      (eaf-call-sync "clip_buffer" eaf--buffer-id)
      (eaf--display-image window))

    (defun eaf--display-image (window)
      "Display the image of qwidget in eaf buffer."
      (let ((image-path (concat eaf-config-location eaf--buffer-id ".jpeg")))
        (when (file-exists-p image-path)
          (clear-image-cache)
          (erase-buffer)
          (insert-image (create-image image-path 'jpeg nil
                                      :width (window-pixel-width window)
                                      :height (window-pixel-height window))))))

    (defun eaf--topmost-delete-frame-handler (frame)
      (eaf--topmost-focus-out))

    (defun eaf--topmost-clear-images-cache-handler ()
      "Clear all images when quitting Emacs."
      (when (file-exists-p eaf-config-location)
        (shell-command-to-string (concat "rm " eaf-config-location "*.jpeg"))))

    (add-hook 'kill-emacs-hook #'eaf--topmost-clear-images-cache-handler)

    (add-hook 'eaf-start-process-hook
              (lambda ()
                (add-function :after after-focus-change-function #'eaf--topmost-focus-change)
                (add-to-list 'move-frame-functions #'eaf-monitor-configuration-change)))

    (add-hook 'eaf-stop-process-hook
              (lambda ()
                (remove-function after-focus-change-function #'eaf--topmost-focus-change)
                (remove-hook 'move-frame-functions #'eaf-monitor-configuration-change)))

    (add-to-list 'delete-frame-functions #'eaf--topmost-delete-frame-handler)))

(defun eaf-monitor-configuration-change (&rest _)
  "EAF function to respond when detecting a window configuration change."
  (when (and eaf--monitor-configuration-p
             (eaf-epc-live-p eaf-epc-process))
    (ignore-errors
      (let (view-infos)
        (dolist (frame (frame-list))
          (dolist (window (window-list frame))
            (with-current-buffer (window-buffer window)
              (when (derived-mode-p 'eaf-mode)
                ;; When `eaf-fullscreen-p' is non-nil, and only the EAF window is present, use frame size
                (if (and eaf-fullscreen-p
                         (equal (length (cl-remove-if #'window-dedicated-p (window-list frame))) 1))
                    (push (format "%s:%s:%s:%s:%s:%s"
                                  eaf--buffer-id
                                  (eaf-get-emacs-xid frame)
                                  0 0 (frame-pixel-width frame) (frame-pixel-height frame))
                          view-infos)
                  (let* ((window-allocation (eaf-get-window-allocation window))
                         (window-divider-right-padding (if window-divider-mode window-divider-default-right-width 0))
                         (window-divider-bottom-padding (if window-divider-mode window-divider-default-bottom-width 0))
                         (titlebar-height (eaf--get-titlebar-height))
                         (frame-coordinate (eaf--get-frame-coordinate))
                         (frame-x (car frame-coordinate))
                         (frame-y (cadr frame-coordinate))
                         (x (+ (eaf--buffer-x-position-adjust frame) (nth 0 window-allocation)))
                         (y (+ (eaf--buffer-y-position-adjust frame) (nth 1 window-allocation)))
                         (w (nth 2 window-allocation))
                         (h (nth 3 window-allocation)))
                    (push (format "%s:%s:%s:%s:%s:%s"
                                  eaf--buffer-id
                                  (eaf-get-emacs-xid frame)
                                  (+ x frame-x)
                                  (+ y titlebar-height frame-y)
                                  (- w window-divider-right-padding)
                                  (- h window-divider-bottom-padding))
                          view-infos)))))))
        (eaf-call-async "update_views" (mapconcat #'identity view-infos ","))))))

(defun eaf--split-number (string)
  (mapcar #'string-to-number (string-split string)))

(defun eaf--get-frame-coordinate ()
  "We need fetch Emacs coordinate to adjust coordinate of EAF if it running on system not support cross-process reparent technology.

Such as, wayland native, macOS etc."
  (cond ((string-equal (getenv "XDG_CURRENT_DESKTOP") "sway")
         (eaf--split-number (shell-command-to-string (concat eaf-build-dir "swaymsg-treefetch/swaymsg-rectfetcher.sh emacs"))))
        ((string-equal (getenv "XDG_CURRENT_DESKTOP") "Hyprland")
         (let ((clients (json-parse-string (shell-command-to-string "hyprctl -j clients")))
               (coordinate))
           (dotimes (i (length clients))
             (when (equal (gethash "pid" (aref clients i)) (emacs-pid))
               (setq coordinate (gethash "at" (aref clients i)))))
           (list (aref coordinate 0) (aref coordinate 1))))
        ((eaf-emacs-running-in-wayland-native)
         (require 'dbus)
         (let* ((coordinate (eaf--split-number
                             (dbus-call-method :session "org.gnome.Shell" "/org/eaf/wayland" "org.eaf.wayland" "get_emacs_window_coordinate" :timeout 1000)
                             ","))
                ;; HiDPI need except by `frame-scale-factor'.
                (frame-x (truncate (/ (car coordinate) (frame-scale-factor))))
                (frame-y (truncate (/ (cadr coordinate) (frame-scale-factor)))))
           (list frame-x frame-y)))
        (t
         (list 0 0))))

(defun eaf--get-titlebar-height ()
  "We need fetch height of window titlebar to adjust y coordinate of EAF when Emacs is not fullscreen."
  (cond ((eaf-emacs-running-in-wayland-native)
         (let ((is-fullscreen-p (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))))
           (if is-fullscreen-p
               0
             ;; `32' is titlebar of Gnome3, we need change this value in other environment.
             (cond ((string-equal (getenv "XDG_CURRENT_DESKTOP") "Hyprland")
                    0)
                   (t
                    32)))))
        (t
         0)))

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
  (when eaf-kill-process-after-last-buffer-closed
    (run-at-time
     5 nil
     (lambda ()
       (when (equal (length (eaf--get-eaf-buffers)) 0)
         (eaf--kill-python-process))
       ))))

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

(defun eaf-keyboard-quit ()
  "Wrap around `keyboard-quit' and signals a ‘quit’ condition to EAF applications."
  (interactive)
  (eaf-call-async "action_quit" eaf--buffer-id)
  (call-interactively 'keyboard-quit))

(defun eaf-send-key ()
  "Directly send key to EAF Python side."
  (interactive)
  (eaf-call-async "send_key" eaf--buffer-id (key-description (this-command-keys-vector))))

(defun eaf-send-key-sequence ()
  "Directly send key sequence to EAF Python side."
  (interactive)
  (eaf-call-async "send_key_sequence" eaf--buffer-id (key-description (this-command-keys-vector))))

(defmacro eaf-create-send-key-function (key &optional value)
  (let ((send-key-function (intern (format "eaf-send-%s-key" key))))
    `(defun ,send-key-function()
       (interactive)
       (eaf-call-async "send_key" eaf--buffer-id (or ,value (format "<%s>" ,key))))))

(defmacro eaf-create-send-sequence-function (key value)
  (let ((send-key-sequence-function (intern (format "eaf-send-%s-sequence" key))))
    `(defun ,send-key-sequence-function()
       (interactive)
       (eaf-call-async "send_key_sequence" eaf--buffer-id ,value))))

(eaf-create-send-key-function "left")
(eaf-create-send-key-function "right")
(eaf-create-send-key-function "down")
(eaf-create-send-key-function "up")
(eaf-create-send-key-function "escape")
(eaf-create-send-key-function "return" "RET")

(eaf-create-send-sequence-function "ctrl-return" "C-RET")
(eaf-create-send-sequence-function "ctrl-delete" "C-<delete>")
(eaf-create-send-sequence-function "ctrl-backspace" "C-<backspace>")
(eaf-create-send-sequence-function "ctrl-left" "C-<left>")
(eaf-create-send-sequence-function "ctrl-right" "C-<right>")
(eaf-create-send-sequence-function "alt-backspace" "M-<backspace>")
(eaf-create-send-sequence-function "shift-return" "S-RET")

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

(defun eaf-get-buffer (buffer-id)
  "Find the buffer given the BUFFER-ID."
  (catch 'found-eaf
    (eaf-for-each-eaf-buffer
     (when (string= eaf--buffer-id buffer-id)
       (throw 'found-eaf buffer))
     nil)))

(defun eaf-get-window-size-by-buffer-id (buffer-id)
  (let ((buffer (eaf-get-buffer buffer-id)))
    (when buffer
      (eaf-get-window-allocation (get-buffer-window buffer)))))

(defun eaf-focus-buffer (buffer-id)
  "Focus the buffer given the BUFFER-ID."
  (let* ((buffer (eaf-get-buffer buffer-id))
         (window (if buffer (get-buffer-window buffer 'visible) nil)))
    (when window (select-window window) t)))

(defvar-local eaf-buffer-input-focus nil)
(defun eaf-update-focus-state (buffer-id state)
  (let ((buffer (eaf-get-buffer buffer-id)))
    (when buffer
      (with-current-buffer buffer
        (setq-local eaf-buffer-input-focus state)))))

(defun eaf--show-message (format-string eaf-prefix logging)
  "A wrapper around `message' that prepend [EAF/app-name] before FORMAT-STRING."
  (let* ((eaf-prefix (if (equal eaf-prefix "TRUE") t nil))
         (logging (if (equal logging "TRUE") t nil))
         (fmt (cond ((not eaf-prefix) "%s")
                    (eaf--buffer-app-name (concat "[EAF/" eaf--buffer-app-name "] %s"))
                    (t "[EAF] %s"))))
    (if logging (message fmt format-string)
      (let ((message-log-max nil))
        (message fmt format-string)))))

(defun eaf--clear-message ()
  "Clear Emacs' echo area ."
  (message nil))

(defun eaf--set-emacs-var (name value)
  "Set Lisp variable NAME with VALUE on the Emacs side."
  (set (intern name) value))

(defun eaf-request-kill-buffer (buffer-id)
  "Function for requesting to kill the given buffer with BUFFER-ID."
  (let* ((buffer (eaf-get-buffer buffer-id)))
    (when buffer
      (kill-buffer buffer)

      (when eaf-goto-right-after-close-buffer
        (eaf-goto-right-tab)))))

(defun eaf--first-start (eaf-epc-port)
  "Call `eaf--open-internal' upon receiving `start_finish' signal from server.

WEBENGINE-INCLUDE-PRIVATE-CODEC is only useful when app-name is video-player."
  ;; Make EPC process.
  (setq eaf-epc-process (make-eaf-epc-manager
                         :server-process eaf-internal-process
                         :commands (cons eaf-internal-process-prog eaf-internal-process-args)
                         :title (mapconcat 'identity (cons eaf-internal-process-prog eaf-internal-process-args) " ")
                         :port eaf-epc-port
                         :connection (eaf-epc-connect "localhost" eaf-epc-port)
                         ))
  (eaf-epc-init-epc-layer eaf-epc-process)

  (dolist (buffer-info eaf--first-start-app-buffers)
    (eaf--open-internal (nth 0 buffer-info) (nth 1 buffer-info) (nth 2 buffer-info)))
  (setq eaf--first-start-app-buffers nil))

(defun eaf--update-buffer-details (buffer-id title url)
  "Function for updating buffer details with its BUFFER-ID, TITLE and URL."
  (when (eaf--called-from-wsl-on-windows-p)
    (eaf-monitor-configuration-change))
  (when (and (> (length title) 0)
             ;; NOTE: When run eaf-browser, this function will be
             ;; called twice, buffer name will be changed twice too,
             ;; To prevent mode-line flickr, we do not change buffer
             ;; name when when title is equal url.
             (not (equal title url)))
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

(defvar eaf-search-input-active-p nil)
(defvar eaf-search-input-buffer-id nil)
(defvar eaf-search-input-callback-tag nil)

(defun eaf--input-message (input-buffer-id interactive-string callback-tag interactive-type initial-content completion-list)
  "Handles input message INTERACTIVE-STRING on the Python side given INPUT-BUFFER-ID and CALLBACK-TYPE."
  (when (string-equal interactive-type "search")
    (setq eaf-search-input-active-p t)
    (setq eaf-search-input-buffer-id input-buffer-id)
    (setq eaf-search-input-callback-tag callback-tag))

  (let* ((input-message (eaf-read-input (concat "[EAF/" eaf--buffer-app-name "] " interactive-string) interactive-type initial-content completion-list)))
    (if input-message
        (eaf-call-async "handle_input_response" input-buffer-id callback-tag input-message)
      (eaf-call-async "cancel_input_response" input-buffer-id callback-tag))
    (setq eaf-search-input-active-p nil)))

(defun eaf-read-input (interactive-string interactive-type initial-content completion-list)
  "EAF's multi-purpose read-input function which read an INTERACTIVE-STRING with INITIAL-CONTENT, determines the function base on INTERACTIVE-TYPE."
  (condition-case nil
      (cond ((or (string-equal interactive-type "string")
                 (string-equal interactive-type "marker")
                 (string-equal interactive-type "search"))
             (read-string interactive-string initial-content))
            ((string-equal interactive-type "file")
             (expand-file-name (read-file-name interactive-string initial-content)))
            ((string-equal interactive-type "directory")
             (expand-file-name (read-directory-name interactive-string initial-content)))
            ((string-equal interactive-type "yes-or-no")
             (yes-or-no-p interactive-string))
            ((string-equal interactive-type "list")
             (completing-read interactive-string completion-list)))
    (quit nil)))

(defun eaf--open-internal (url app-name args)
  "Open an EAF application internally with URL, APP-NAME and ARGS."
  (let* ((buffer (eaf--create-buffer url app-name args)))
    (eaf--open-new-buffer buffer)))

(defun eaf--open-new-buffer (buffer)
  (with-current-buffer buffer
    (eaf-call-async "new_buffer"
                    eaf--buffer-id
                    (if (eaf--called-from-wsl-on-windows-p)
                        (eaf--translate-wsl-url-to-windows eaf--buffer-url)
                      eaf--buffer-url)
                    (eaf--get-app-module-path eaf--buffer-app-name)
                    eaf--buffer-args)

    ;; Run application's hook.
    (let ((app-hook (assoc eaf--buffer-app-name eaf-app-hook-alist)))
      (when app-hook
        (funcall (cdr app-hook))))

    (eaf--update-modeline-icon)
    (eaf--preview-display-buffer eaf--buffer-app-name buffer)))

(defun eaf--rebuild-buffer ()
  (when (derived-mode-p 'eaf-mode)
    (eaf-restart-process)
    (eaf--open-new-buffer (current-buffer))
    (eaf-monitor-configuration-change)))

(defun eaf--update-modeline-icon ()
  "Update modeline icon if used"
  (when (and (ignore-errors (require 'all-the-icons)) (featurep 'eaf-all-the-icons))
    (declare-function eaf-all-the-icons-update-icon "eaf-all-the-icons.el")
    (eaf-all-the-icons-update-icon)))

(defun eaf-goto-left-tab ()
  "Go to left tab, you need re-implement this interface yourself."
  (interactive)
  (message "Go to left tab, you need re-implement eaf-goto-left-tab yourself."))

(defun eaf-goto-right-tab ()
  "Go to right tab, you need re-implement this interface yourself."
  (interactive)
  (message "Go to right tab, you need re-implement eaf-goto-right-tab yourself."))

(defun eaf-translate-text (text)
  "Translate selected TEXT."
  (message "You need re-implement eaf-translate-text yourself."))

(defun eaf--non-remote-default-directory ()
  "Return `default-directory' itself if is not part of remote, otherwise return $HOME."
  (if (or (file-remote-p default-directory)
          (not (file-accessible-directory-p default-directory)))
      (getenv "HOME")
    default-directory))

(defun eaf--get-app-for-extension (url)
  "Given the EXTENSION-NAME, loops through `eaf-app-extensions-alist', set and return `app-name'."
  (let ((extension-name (eaf-get-file-name-extension url))
        apps)
    (dolist (app-extension eaf-app-extensions-alist)
      (when (member extension-name (symbol-value (cdr app-extension)))
        (add-to-list 'apps (car app-extension))))
    (if (length= apps 1)
        (car apps)
      (completing-read (format "Which app to open %s: " url) apps))))

;;;###autoload
(defun eaf-get-file-name-extension (file)
  "A wrapper around `file-name-extension' that downcases the extension of the FILE."
  (downcase (file-name-extension file)))

(defun eaf--translate-wsl-url-to-windows (path)
  "Translate from a WSL PATH to a Windows path."
  (replace-regexp-in-string "/mnt/\\([a-zA-Z]\\)" "\\1:" path))

;;;###autoload
(defun eaf-open (url &optional app-name args always-new)
  "Open an EAF application with URL, optional APP-NAME and ARGS.

Interactively, a prefix arg replaces ALWAYS-NEW, which means to open a new
 buffer regardless of whether a buffer with existing URL and APP-NAME exists.

By default, `eaf-open' will switch to buffer if corresponding url exists.
`eaf-open' always open new buffer if option OPEN-ALWAYS is non-nil.

When called interactively, URL accepts a file that can be opened by EAF."
  (interactive "G[EAF] EAF Open: ")
  ;; Try to set app-name along with url when calling INTERACTIVELY
  (when (and (not app-name) (file-exists-p url))
    ;; Expand file name.
    (setq url (expand-file-name url))

    ;; Add recentf list.
    (when (featurep 'recentf)
      (recentf-add-file url))

    ;; Adjust before open.
    (if (file-directory-p url)
        (setq app-name "file-manager")
      ;; Initialize url, app-name and args
      (setq app-name (eaf--get-app-for-extension url))
      (cond
       ((equal app-name "browser")
        (setq url (concat "file://" url)))
       )))

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

  ;; Init arguments.
  (unless args (setq args ""))
  (setq always-new (or always-new current-prefix-arg))

  ;; Hooks are only added if not present already...
  (add-hook 'window-size-change-functions #'eaf-monitor-window-size-change)
  (add-hook 'window-configuration-change-hook #'eaf-monitor-configuration-change)

  ;; Open URL with EAF application
  (if (eaf-epc-live-p eaf-epc-process)
      (let (exists-eaf-buffer)
        ;; Try to open buffer           ; ;
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
              (eaf--preview-display-buffer app-name exists-eaf-buffer)
              (message (concat "[EAF/" app-name "] " "Switch to %s") url))
          (eaf--open-internal url app-name args)
          (message (concat "[EAF/" app-name "] " "Opening %s") url)))

    ;; Record user input, and call `eaf--open-internal' after receive `start_finish' signal from server process.
    (unless eaf--first-start-app-buffers
      (push `(,url ,app-name ,args) eaf--first-start-app-buffers))

    ;; Start EAF process.
    (eaf-start-process)
    (message (concat "[EAF/" app-name "] " "Opening %s") url)))

(defun eaf-duplicate-current-buffer ()
  "Duplicate the current EAF buffer in a new buffer.

So multiple EAF buffers visiting the same file do not sync with each other."
  (interactive)
  (when (derived-mode-p 'eaf-mode)
    (delete-other-windows)
    (split-window-horizontally)
    (other-window +1)
    (eaf-open eaf--buffer-url eaf--buffer-app-name eaf--buffer-args t)))

(defun eaf--preview-display-buffer (app-name buffer)
  "Display specified APP-NAME's app buffer in BUFFER."
  (let ((display-fun (or (cdr (assoc app-name
                                     eaf-preview-display-function-alist))
                         #'switch-to-buffer)))
    (funcall display-fun buffer)))

(defun eaf-split-preview-windows (url)
  "Function for spliting preview windows with specified URL."
  (delete-other-windows)
  (find-file url)
  (split-window-horizontally)
  (other-window +1))

(defvar-local eaf-edit-confirm-action nil)

(defvar eaf-edit-confirm-function-alist '())

(defun eaf-edit-buffer-confirm ()
  "Confirm input text and send the text to corresponding EAF app."
  (interactive)
  ;; Note: pickup buffer-id from buffer name and not restore buffer-id from buffer local variable.
  ;; Then we can switch edit buffer to any other mode, such as org-mode, to confirm buffer string.

  ;; Do confirm action.
  (let ((confirm-function (cdr (assoc eaf-edit-confirm-action eaf-edit-confirm-function-alist))))
    (if confirm-function
        (funcal confirm-function)
      (eaf-call-async "set_focus_text" eaf--buffer-id (eaf--encode-string (eaf-copy-to-clipboard (buffer-string))))))

  ;; Close confirm window.
  (kill-buffer)
  (delete-window))

;; Update and load the theme
(defun eaf-get-theme-mode ()
  (format "%s" (frame-parameter nil 'background-mode)))

(defun eaf-get-theme-background-color ()
  (format "%s" (frame-parameter nil 'background-color)))

(defun eaf-get-theme-foreground-color ()
  (format "%s" (frame-parameter nil 'foreground-color)))

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
  "Activate Emacs win32 window."
  (eaf-call-async "activate_emacs_win32_window" (frame-parameter nil 'name)))

(defvar eaf--emacs-program-name
  (string-trim
   (string-replace "." "-" (alist-get 'comm (process-attributes (emacs-pid)))))
  "Name of Emacs.")

(defun eaf--activate-emacs-linux-window (&optional buffer_id)
  "Activate Emacs window by `wmctrl'."
  (let ((system-configuration-arguments (split-string system-configuration-features)))
    (if (or (member "LUCID" system-configuration-arguments)
            (member "ATHENA" system-configuration-arguments))
        (message "Please compile emacs use option --with-x-toolkit=gtk3, otherwise EAF can't focus emacs window expected.")
      (if (member (eaf--get-current-desktop-name) eaf-wm-focus-fix-wms)
          ;; When switch app focus in WM, such as, i3 or qtile.
          ;; Emacs window cannot get the focus normally if mouse in EAF buffer area.
          ;;
          ;; So we move mouse to frame bottom of Emacs, to make EAF receive input event.
          (cond ((string-equal (getenv "XDG_CURRENT_DESKTOP") "Hyprland")
                 (shell-command-to-string (concat "hyprctl dispatch focuswindow " eaf--emacs-program-name)))
                (t
                 (eaf-call-async "eval_function" (or eaf--buffer-id buffer_id) "move_cursor_to_corner" (key-description (this-command-keys-vector)))))

        ;; Activate the window by `wmctrl' when possible
        (if (executable-find "wmctrl")
            (shell-command-to-string (format "wmctrl -i -a $(wmctrl -lp | awk -vpid=$PID '$3==%s {print $1; exit}')" (emacs-pid)))
          (message "Please install wmctrl to active Emacs window."))))))

(defun eaf--activate-emacs-mac-window()
  "Activate Emacs macOS window."
  (shell-command-to-string "open -a emacs"))

(defun eaf-activate-emacs-window(&optional buffer_id)
  "Activate Emacs window."
  (cond
   ((or (memq system-type '(cygwin windows-nt ms-dos))
        (eaf--called-from-wsl-on-windows-p))
    (eaf--activate-emacs-win32-window))
   ((eq system-type 'darwin)
    (eaf--activate-emacs-mac-window))
   ((or (eq system-type 'gnu/linux)
        (eq system-type 'berkeley-unix))
    (eaf--activate-emacs-linux-window buffer_id))))

(defun eaf--change-default-directory (buffer-id directory)
  "Change default directory to DIRECTORY."
  (let ((buffer (eaf-get-buffer buffer-id)))
    (when buffer
      (with-current-buffer buffer
        (when (file-accessible-directory-p (or (file-name-directory directory) directory))
          (setq-local default-directory directory))))))

;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eaf-generate-keymap-doc (var)
  "This command use for generate keybindings document Wiki."
  (interactive "vEAF keybinding variable: ")
  (insert (format "### %s\n\n" (get var 'variable-documentation)))
  (insert "| Key   | Event   |\n")
  (insert "| :---- | :------ |\n")
  ;; NOTE: `standard-value' use for fetch origin value of keybinding variable.
  ;; Otherwise, developer's personal config will dirty document.
  (dolist (element (eval (car (get var 'standard-value))))
    (insert (format "| `%s` | %s |\n" (car element) (cdr element))))
  (insert "\n"))

(defun eaf--match-app-extension-p (ext)
  (cl-loop for app-extensions-alist in eaf-app-extensions-alist
           for extensions = (eval (cdr app-extensions-alist))
           when (member ext extensions)
           return t))

(defun eaf--buffer-file-p ()
  "Determine if the file opened at the current buffer be opened by EAF."
  (let ((ext (when (and buffer-file-name
                        (file-exists-p buffer-file-name))
               (file-name-extension buffer-file-name))))
    (and ext (eaf--match-app-extension-p (downcase ext)))))

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
       (eaf--match-app-extension-p (downcase ext))
       (not (member ext eaf-find-file-ext-blacklist))))

(defun eaf-next-buffer-same-app ()
  "Switch to the next buffer of the same EAF app with the current buffer."
  (interactive)
  (let ((origin-buff (current-buffer)) (app-name mode-name)
        (new-buff nil) (finished nil))
    (while (not finished)
      (setq new-buff (next-buffer))
      (when (or (equal new-buff origin-buff) (equal app-name mode-name))
        (setq finished t)))))

(defun eaf-previous-buffer-same-app ()
  "Switch to the previous buffer of the same EAF app with the current buffer."
  (interactive)
  (let ((origin-buff (current-buffer)) (app-name mode-name)
        (new-buff nil) (finished nil))
    (while (not finished)
      (setq new-buff (previous-buffer))
      (when (or (equal new-buff origin-buff) (equal app-name mode-name))
        (setq finished t)))))

(defun eaf-share-path-or-url ()
  "Share the current file path or web URL as QRCode."
  (interactive)
  (if (ignore-errors (require 'eaf-airshare))
      (eaf-open (eaf-get-path-or-url) "airshare")
    (message "You should install EAF application 'airshare' first.")))

(defun eaf-open-devtool-page ()
  "Use EAF Browser to open the devtools page."
  (delete-other-windows)
  (split-window (selected-window) (round (* (nth 3 (eaf-get-window-allocation (selected-window))) 0.618)) nil t)
  (other-window 1)
  (eaf-open "about:blank" "browser" "devtools"))

;;;;;;;;;;;;;;;;;;;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: In the code below we should use `save-selected-window' (or even
;; better `with-selected-window') rather than (other-window +1) followed by
;; (other-window -1) since this is not always a no-op.
(defun eaf--scroll-other-window (orig-fun &optional arg &rest args)
  "When next buffer is `eaf-mode', do `eaf-scroll-up-or-next-page'."
  (other-window +1)
  (if (derived-mode-p 'eaf-mode)
      (progn
        (eaf-call-async "scroll_other_buffer" eaf--buffer-id "up"
                        (if arg "line" "page"))
        (other-window -1))
    (other-window -1)
    (apply orig-fun arg args)))
(advice-add 'scroll-other-window :around #'eaf--scroll-other-window)

(defun eaf--scroll-other-window-down (orig-fun &optional arg &rest args)
  "When next buffer is `eaf-mode', do `eaf-scroll-down-or-previous-page'."
  (other-window +1)
  (if (derived-mode-p 'eaf-mode)
      (progn
        (eaf-call-async "scroll_other_buffer" eaf--buffer-id "down"
                        (if arg "line" "page"))
        (other-window -1))
    (other-window -1)
    (apply orig-fun arg args)))
(advice-add 'scroll-other-window-down :around #'eaf--scroll-other-window-down)

(defun eaf--watch-other-window-internal (orig-fun &optional direction line
                                                  &rest args)
  "When next buffer is `eaf-mode', do `eaf-watch-other-window'."
  (other-window +1)
  (if (derived-mode-p 'eaf-mode)
      (progn
        (eaf-call-async "scroll_other_buffer" eaf--buffer-id
                        (if (string-equal direction "up") "up" "down")
                        (if line "line" "page"))
        (other-window -1))
    (other-window -1)
    (apply orig-fun direction line args)))
(advice-add 'watch-other-window-internal :around
            #'eaf--watch-other-window-internal)

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

(defun eaf--load-theme (&rest _ignores)
  (eaf-for-each-eaf-buffer
   (eaf-call-async "eval_function" eaf--buffer-id "update_theme" (key-description (this-command-keys-vector)))))
(advice-add #'load-theme :after #'eaf--load-theme)

(defun eaf-ocr-buffer ()
  (interactive)
  (eaf-call-async "ocr_buffer" eaf--buffer-id))

(defun eaf-ocr-buffer-record (result)
  (kill-new result)
  (message "[EAF] Have paste OCR string to kill-ring."))

;;;###autoload
(defun eaf-install-and-update (&rest apps)
  "Interactively run `install-eaf.py' to install/update EAF apps.

For a full `install-eaf.py' experience, refer to `--help' and run in a terminal."
  (interactive)
  (let* ((default-directory eaf-source-dir)
         (output-buffer (generate-new-buffer "*EAF installation*"))
         (apps (or apps eaf-apps-to-install))
         (proc
          (progn
            (async-shell-command (concat
                                  eaf-python-command " install-eaf.py"
                                  (when (and apps (> (length apps) 0)) " --install ")
                                  (mapconcat 'symbol-name apps " "))
                                 output-buffer)
            (get-buffer-process output-buffer))))
    (if (process-live-p proc)
        (set-process-sentinel proc #'eaf--post-install-sentinel)
      (eaf--post-install))))

(defun eaf--post-install-sentinel (process string-signal)
  (when (memq (process-status process) '(exit signal))
    (message "Running post install")
    (eaf--post-install)
    (shell-command-sentinel process string-signal)))

(defun eaf--symlink-directory (old new)
  (if (fboundp 'straight--symlink-recursively)
      (straight--symlink-recursively old new)
    (make-symbolic-link old new)))

(defun eaf--post-install ()
  ;; If user use Straight package manager, then `eaf-source-dir' is not equal `eaf-build-dir'
  (when (not (string= eaf-source-dir eaf-build-dir))
    (message "Symlinking app directory")
    (eaf--symlink-directory
     (expand-file-name "app" eaf-source-dir)
     (expand-file-name "app" eaf-build-dir)))

  (when eaf-byte-compile-apps
    (message "Byte-compiling")
    (byte-recompile-directory eaf-build-dir 0))

  (message "Updating load path")
  (eaf-add-subdirs-to-load-path eaf-build-dir)
  (message "Done"))

(defvar eaf--last-visit-buffer nil)

(defun eaf-monitor-window-buffer-change ()
  ;; We record last visit time for EAF buffer.
  (when (derived-mode-p 'eaf-mode)
    (setq-local eaf--last-visit-time (current-time)))

  ;; Clean file manager buffer when buffer or window changed.
  (unless (eq (current-buffer)
              eaf--last-visit-buffer)
    (when eaf-clean-duplicate-buffers
      (eaf-clean-file-manager-buffers)))

  (unless (or (minibufferp)
              (string-equal (buffer-name) "*Messages*"))
    (setq eaf--last-visit-buffer (current-buffer))))

(add-hook 'post-command-hook 'eaf-monitor-window-buffer-change)

(defun eaf-clean-file-manager-buffers ()
  (let ((now (current-time)))
    (eaf-for-each-eaf-buffer
     (when (and
            ;; Must be EAF file manager buffer.
            (string-equal eaf--buffer-app-name "file-manager")
            (boundp 'eaf--last-visit-time)
            ;; Not show in frame.
            (not (memq buffer (mapcar #'window-buffer (window-list))))
            ;; Found duplicate buffer.
            (eaf-has-duplicate-path-buffer-p buffer)
            ;; Existing time exceeds than `eaf-duplicate-buffer-survival-time'
            (> (float-time (time-subtract now eaf--last-visit-time)) eaf-duplicate-buffer-survival-time))
       ;; Just log in *messages* buffer silently, don't disturb users.
       (let ((inhibit-message t))
         (message "[EAF] Clean duplicate file manager buffer: %s" buffer))
       (kill-buffer buffer)))))

(defun eaf-has-duplicate-path-buffer-p (eaf-buffer)
  (cl-remove-if-not
   (lambda (buffer)
     (and
      (not (eq buffer eaf-buffer))
      (string-equal (with-current-buffer eaf-buffer eaf--buffer-url)
                    (with-current-buffer buffer eaf--buffer-url))))
   (eaf--get-eaf-buffers)))

(define-obsolete-function-alias 'eaf-install 'eaf-install-and-update
  "Please use M-x eaf-install-and-update instead.")

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

(defun eaf--isearch-forward-advisor (orig-fun &optional arg &rest args)
  (if eaf-search-input-active-p
      (eaf-call-async "handle_search_forward" eaf-search-input-buffer-id eaf-search-input-callback-tag)
    (apply orig-fun arg args)))
(advice-add #'isearch-forward :around #'eaf--isearch-forward-advisor)
(when (and (ignore-errors (require 'swiper)) (featurep 'swiper))
  (advice-add #'swiper :around #'eaf--isearch-forward-advisor)
  (advice-add #'swiper-isearch :around #'eaf--isearch-forward-advisor))

(defun eaf--isearch-backward-advisor (orig-fun &optional arg &rest args)
  (if eaf-search-input-active-p
      (eaf-call-async "handle_search_backward" eaf-search-input-buffer-id eaf-search-input-callback-tag)
    (apply orig-fun arg args)))
(advice-add #'isearch-backward :around #'eaf--isearch-backward-advisor)
(when (and (ignore-errors (require 'counsel)) (featurep 'counsel))
  (advice-add #'counsel-minibuffer-history :around #'eaf--isearch-forward-advisor))

(provide 'eaf)

;;; eaf.el ends here
