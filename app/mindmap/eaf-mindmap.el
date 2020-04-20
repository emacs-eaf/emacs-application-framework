;;; eaf-mindmap.el --- Simple description

;; Filename: eaf-mindmap.el
;; Description: Simple description
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2020, Andy Stewart, all rights reserved.
;; Created: 2020-02-28 16:09:02
;; Version: 0.1
;; Last-Updated: 2020-02-28 16:09:02
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/eaf-mindmap.el
;; Keywords:
;; Compatibility: GNU Emacs 26.3
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
;; Simple description
;;

;;; Installation:
;;
;; Put eaf-mindmap.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eaf-mindmap)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET eaf-mindmap RET
;;

;;; Change log:
;;
;; 2020/02/28
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
(require 'json)

;;; Code:

(dbus-register-signal
 :session "com.lazycat.eaf" "/com/lazycat/eaf"
 "com.lazycat.eaf" "export_org_json"
 #'eaf--export-org-json)

(defun eaf--export-org-json (org-json-content org-file-path)
  (let (org-parse-data)
    (with-temp-buffer
      (insert org-json-content)
      (beginning-of-buffer)
      (let* ((json (json-read))
             (root-node-name (cdr (assoc 'topic (assoc 'data json))))
             (children-node (cdr (assoc 'children (assoc 'data json)))))
        ;; (message "%s" (org-json-decode (cdr (assoc 'data json))))
        (insert (org-json-decode (cdr (assoc 'data json))))
        (beginning-of-buffer)
        (while (search-forward-regexp "\\*\\s-topic\\s-\"" nil t)
          (let ((header-string (buffer-substring (save-excursion
                                                   (beginning-of-line)
                                                   (point))
                                                 (save-excursion
                                                   (beginning-of-line)
                                                   (search-forward-regexp "\\*\\s-" nil t)
                                                   (point)
                                                   )))
                (content-string (buffer-substring (point)
                                                  (save-excursion
                                                    (end-of-line)
                                                    (backward-char 1)
                                                    (point)))))
            (when (> (length header-string) 2)
              (setq header-string (string-remove-prefix "*" header-string)))
            (setq org-parse-data (concat org-parse-data (format "%s%s\n" header-string content-string)))))

        (with-temp-file org-file-path
          (insert org-parse-data))
        ))))

(defun json-read-r ()
  (let ((json-object-type 'alist))
    (let ((r (json-read)))
      (if (listp r) (reverse r) r))))

(defun org-json-raw ()
  (let (p1)
    (goto-char (point-min))
    (re-search-forward "[ \n\t]*")
    (beginning-of-line)
    (delete-region (point-min) (point))
    (if (re-search-forward "\\`#\\+begin_src" nil t)
        ;; string
        (progn
          (if (re-search-forward "^#\\+end_src" nil t)
              (progn (beginning-of-line)
                     (setq p1 (point)))
            (setq p1 (point-max)))
          (goto-char (point-min))
          (forward-line 1)
          (buffer-substring-no-properties (point) p1))
      (goto-char (point-min))
      (json-read-r))))

(defun org-json-entry (str)
  (let (lv header has-body p0 p1)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (end-of-line)
      (setq header (buffer-substring (point-min) (point)))
      ;; delete header
      (delete-region (point-min) (point))
      ;; process header
      (string-match "^\\(\\*+\\) +\\(.+?\\) *$" header)
      (setq lv (length (match-string 1 header)))
      ;; remove '*'
      (setq header (match-string 2 header))
      (goto-char (point-min))
      (if (re-search-forward "\\`[ \n\t]*\\'" nil t)
          (list lv header)              ; body is empty
        (list lv header (org-json-raw))))))

(defun org-json-entries ()
  (let (p0 pl)
    ;; get all entries
    (setq pl (let (ret tmp)
               (goto-char (point-min))
               (while (re-search-forward "^\*+ " nil t)
                 (setq tmp (point))
                 (beginning-of-line)
                 (setq ret (cons (point) ret))
                 (goto-char tmp))
               (setq ret (cons (point-max) ret))
               (reverse ret)))
    (setq p0 (car pl))
    (mapcar (lambda (p1)
              (prog1 (org-json-entry
                      (buffer-substring-no-properties p0 p1))
                (setq p0 p1)))
            (cdr pl))))

(defun org-json-split-header (lst &optional ret)
  "Split header for the return of `org-json-entries'."
  (if (null lst) (reverse ret)
    (let (e0 e1 lst1)
      (setq e0 (car lst))
      (setq lst1 (cdr lst))
      (setq e1 (if lst1 (car lst1)
                 '(0 "" "")))
      (if (>= (car e0) (car e1))
          ;; no child
          (when (= (length e0) 2)
            ;; split header
            (if (string-match "^\\(.+?\\)[ \n\t]+\\(.+\\)$" (nth 1 e0))
                (setq e0 (list (nth 0 e0)
                               (match-string 1 (nth 1 e0))
                               (with-temp-buffer
                                 (insert (match-string 2 (nth 1 e0)))
                                 (goto-char (point-min))
                                 (json-read-r))))
              (setq e0 (list (nth 0 e0) (nth 1 e0) :json-null))))
        (when (> (length e0) 2)
          ;; drop unnecessary elem
          (setq e0 (list (nth 0 e0)
                         (nth 1 e0)))))
      (org-json-split-header lst1 (cons e0 ret)))))

(defun org-json-gen-alist (lst)
  "generate alist from return of `org-json-gen-alist',
actural call `org-json-gen-alist1' to work."
  (cdr (org-json-gen-alist1 lst 1)))

(defun org-json-gen-alist1 (lst lv &optional ret)
  (if (or (null lst)
          (< (car (car lst)) lv))
      ;; return list . ret
      (cons lst (reverse ret))
    ;; not return
    (let (e r1 kv)
      ;; r1 is return of children
      ;; kv is key-value pair
      (setq e (car lst))
      (if (> (length e) 2)
          ;; no child
          (progn (setq kv (cons (nth 1 e)
                                (nth 2 e)))
                 (org-json-gen-alist1 (cdr lst) lv (cons kv ret)))

        (setq r1 (org-json-gen-alist1 (cdr lst) (1+ lv)))
        ;; convert to array if necessray
        (let ((seq (mapcar 'car (cdr r1))))
          (when (equal (mapcar 'number-to-string
                               (number-sequence 0 (1- (length seq))))
                       seq)
            (setq r1 (cons (car r1)
                           (vconcat (mapcar 'cdr (cdr r1)))))))
        (setq kv (cons (nth 1 e) (cdr r1)))
        (org-json-gen-alist1 (car r1) lv
                             (cons kv ret))))))


(defun org-json-encode ()
  (save-excursion
    (org-json-gen-alist
     (org-json-split-header
      (org-json-entries)))))

(defun org-json-decode (obj &optional lv)
  "Decode json object to org at level `lv'."
  (unless lv (setq lv 1))
  (cond ((stringp obj) (if (string-match "\n\\'" obj)
                           ;; end with '\n' using #+begin_src block
                           (concat "#+begin_src\n"
                                   obj
                                   "#+end_src")
                         (concat "\""
                                 (replace-regexp-in-string
                                  "\"" "\\\""
                                  (replace-regexp-in-string
                                   "\\\\" "\\\\" obj))
                                 "\"")))
        ((numberp obj) (number-to-string obj))
        ((vectorp obj) (if (= (length obj) 0)
                           "[]"
                         (let ((n 0) ns)
                           (mapconcat
                            (lambda (x)
                              (prog1
                                  (org-json-kv-decode
                                   (cons (number-to-string n)
                                         x)
                                   lv)
                                (setq n (1+ n))))
                            obj "\n"))))
        ((equal obj json-null) "{}")
        ((listp obj) (mapconcat
                      (lambda (x)
                        (org-json-kv-decode x lv))
                      obj "\n"))
        ((equal obj t) "true")
        ((equal obj json-false) "false")
        (t (error "org-json-decode type error: %S" obj))))

(defun org-json-kv-decode (kv lv)
  "Decode a key-value pair."
  (let ((k (car kv))
        (v (cdr kv))
        h ks vs)
    (setq ks (cond ((symbolp k) (symbol-name k))
                   ((stringp k) k)
                   ((numberp k) (number-to-string k))
                   (t (error "org-json-kv-decode key type error: %S" k))))
    (setq h (concat (make-string lv ?*) " " ks))
    (setq vs (org-json-decode v (1+ lv)))
    (if (string-match "\n" vs)
        (concat h "\n" vs)
      (concat h " " vs))
    ))

(provide 'eaf-mindmap)

;;; eaf-mindmap.el ends here
