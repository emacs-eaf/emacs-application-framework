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

(defcustom eaf-evil-leader-key "C-SPC"
  "Leader key trigger" )

(defcustom eaf-evil-leader-keymap #'doom/leader
  "Leader key bind"
  :type 'keymap)

(defun eaf-enable-evil-intergration ()
  "EAF evil intergration."
  (interactive)

  (add-hook 'evil-normal-state-entry-hook
            (lambda ()
              (when (derived-mode-p 'eaf-mode)
                (define-key eaf-mode-map (kbd eaf-evil-leader-key) eaf-evil-leader-keymap)
                (setq emulation-mode-map-alists
                      (delq 'evil-mode-map-alist emulation-mode-map-alists)))))

  (add-to-list 'evil-insert-state-modes 'eaf-edit-mode)

  (eaf-bind-key clear_focus "<escape>" eaf-browser-keybinding))

(with-eval-after-load "evil"
  (eaf-enable-evil-intergration))

(provide 'eaf-evil)
