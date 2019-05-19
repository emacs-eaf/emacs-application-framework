(defun eaf-buffer-eval (code)
  (interactive)
  (eaf-call "eval_code" buffer-id code))

(defun eaf-eval-js (js)
  (interactive)
  (eaf-call "eval_code" buffer-id
            (format "self.web_page.runJavaScript(\"%s\")" js)))

(defun eaf-zoom-out()
  (interactive)
  (eaf-buffer-eval "self.zoom_out()"))

(defun eaf-zoom-in()
  (interactive)
  (eaf-buffer-eval "self.zoom_in()"))

(defun eaf-next-line ()
  (interactive)
  (eaf-eval-js "window.scrollBy(0, 50)"))

(defun eaf-previous-line ()
  (interactive)
  (eaf-eval-js "window.scrollBy(0, -50)"))

(defun eaf-beginning-of-buffer ()
  (interactive)
  (eaf-eval-js "scrollTo(0, 0)"))

(defun eaf-end-of-buffer ()
  (interactive)
    (eaf-eval-js "scrollBy(0,document.body.scrollHeight)"))

(defun eaf-browser-keybind()
  (local-set-key (kbd "C-=") 'eaf-zoom-in)
  (local-set-key (kbd "C--") 'eaf-zoom-out)
  (local-set-key (kbd "M-<") 'eaf-beginning-of-buffer)
  (local-set-key (kbd "M->") 'eaf-end-of-buffer)
  (local-set-key (kbd "C-n") 'eaf-next-line)
  (local-set-key (kbd "C-p") 'eaf-previous-line))

(add-hook 'eaf-mode-hook 'eaf-browser-keybind)
