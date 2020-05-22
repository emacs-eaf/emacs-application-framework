;; eaf-evil.el --- Emacs application framework  -*- lexical-binding: t; -*-

;; Filename: eaf-evil.el
;; Description: Emacs application framework
;; Author:  lee <loyalpartner@163.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2020-05-17 12:31:12
;; Version: 0.5
;; Last-Updated: Wed May 20 11:48:43 2020 (-0400)
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

(defvar eaf-printable-character
  (mapcar #'char-to-string (number-sequence ?! ?~))
  "printable character")

(defun eaf-evil-lookup-key (key)
  (or (lookup-key (current-local-map) (kbd key))
      (lookup-key eaf-mode-map* (kbd key))
      ;; sequence key
      (when (or (string-prefix-p "C-" key)
                (string-prefix-p "M-" key))
        (lookup-key (current-global-map) (kbd key)))
      'eaf-send-key))


(defun eaf-generate-normal-state-key-func (key)
  (lambda () (interactive)
    (call-interactively (eaf-evil-lookup-key key))))

(defun eaf-evil-define-single-keys ()
  (dolist (key (append  eaf-printable-character
                        '("<escape>" "RET" "DEL" "TAB" "SPC" "<backtab>" "<home>" "<end>" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>" "<backspace>" "<return>")))
    (evil-define-key* '(normal insert) eaf-mode-map* (kbd key)
      (eaf-generate-normal-state-key-func key))))

(defun eaf-evil-define-ctrl-keys ()
  (dolist (key (seq-difference  eaf-printable-character
                                (mapcar #'char-to-string "wWxXcChH[1234567890")))
    (evil-define-key* '(normal insert) eaf-mode-map* (kbd (format "C-%s" key))
      (eaf-generate-normal-state-key-func (format "C-%s" key)))))


(defun eaf-evil-define-meta-keys ()
  (dolist (key (seq-difference  eaf-printable-character
                                (mapcar #'char-to-string "xX::1234567890")))
    (evil-define-key* '(normal insert) eaf-mode-map* (kbd (format "M-%s" key))
      (eaf-generate-normal-state-key-func (format "M-%s" key)))))

(defun eaf-browser-focus-p ()
  (eq (eaf-call "call_function" eaf--buffer-id "is_focus") t))

(defun eaf-browser-focus-handler ()
  (if (eaf-browser-focus-p)
      (unless (evil-insert-state-p) (evil-insert-state))
    (when (evil-insert-state-p) (evil-normal-state))))

(defun eaf-enable-evil-intergration ()
  (interactive)
  (when (featurep 'evil)
    (eaf-evil-define-single-keys)
    (eaf-evil-define-ctrl-keys)
    (eaf-evil-define-meta-keys)
    (add-to-list 'evil-insert-state-modes 'eaf-edit-mode)
    (eaf-bind-key clear_focus "<escape>" eaf-browser-keybinding)
    (add-hook 'eaf-browser-hook (lambda () (add-hook 'post-command-hook #'eaf-browser-focus-handler nil t)))
    ;; TODO: add 'eaf-terminal-hook
    ;; (add-hook 'eaf-mode-hook (lambda () ()
    ;;                            (when (string= "terminal" eaf--buffer-app-name)
    ;;                                 (evil-emacs-state))))
    ))

(with-eval-after-load "evil"
  (eaf-enable-evil-intergration))

(provide 'eaf-evil)
