;; To test:
;; emacs -Q -l "test.el" &
;;
;; To clean up between tests:
;; rm -rf .emacs-eaf.d/{straight/repos/emacs-application-framework/,build/eaf,eaf}


(setq user-emacs-directory "./.emacs-eaf.d")

(setq straight-use-package-by-default t
      straight-recipes-gnu-elpa-use-mirror t
      straight-build-dir (expand-file-name
			  "build"
			  user-emacs-directory)
      straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
	       user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'all-the-icons)

(setq eaf-apps-to-install '(demo))

(straight-use-package
 '(eaf
   :type git
   :host github
   :repo "ThibautVerron/emacs-application-framework"
   ;; :host nil
   ;; :repo "~/Development/emacs-application-framework"
   :branch "installation-test"
   :files ("eaf.el" "eaf.py" "extension" "core" "img" "sync-eaf-resources.py")))

(require 'eaf)
