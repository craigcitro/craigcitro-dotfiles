;; Configure flymake for python
(defun cc/flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-with-folder-structure))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "fpylint"
	  (list local-file
		(format "--indent=%s" py-indent-offset)))))

(defvar cc/flymake-by-mode-alist
  '((python-mode cc/flymake-pylint-init)))

(defun cc/flymake-by-mode-find-file-hook ()
  (let ((tool (assq major-mode cc/flymake-by-mode-alist)))
    (when tool
      (make-variable-buffer-local 'flymake-allowed-file-name-masks)
      (setq flymake-allowed-file-name-masks
	    `(("" ,@(cdr tool))))
      (flymake-mode-on)
      (cc/flymake-by-mode-on)
      )))
(add-hook 'find-file-hook 'cc/flymake-by-mode-find-file-hook)

;; This whole block is stolen from emacswiki
(eval-after-load 'flymake
  '(progn
     (defun cc/flymake-err-at (pos)
       (let ((overlays (overlays-at pos)))
	 (remove nil
		 (mapcar (lambda (overlay)
			   (and (overlay-get overlay 'flymake-overlay)
				(overlay-get overlay 'help-echo)))
			 overlays))))
     (defun cc/echo-flymake-error ()
       (interactive)
       (message "%s" (mapconcat 'identity (cc/flymake-err-at (point)) "\n")))
     (defadvice flymake-goto-next-error (after display-message activate compile)
       (cc/echo-flymake-error))
     (defadvice flymake-goto-prev-error (after display-message activate compile)
       (cc/echo-flymake-error))
     ))

(defvar cc/flymake-by-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-_" 'cc/echo-flymake-error)
    (define-key map "\C-x\C-k\C-n" 'flymake-goto-next-error)
    (define-key map "\C-x\C-k\C-p" 'flymake-goto-prev-error)
    map))

(define-minor-mode cc/flymake-by-mode-minor-mode
  nil
  nil
  :keymap cc/flymake-by-mode-keymap)

(defun cc/flymake-by-mode-on ()
  (unless cc/flymake-by-mode-minor-mode
    (cc/flymake-by-mode-minor-mode)))

;;;;;;;;;;;;;;;
;; trailing whitespace and whatnot

(defface cc/long-line-face
  '((t (:foreground "red")))
  "*Face used for long lines.")
(defface cc/bad-whitespace-face
  '((t (:foreground "red" :background "red")))
  "*Face used for trailing spaces or tabs.")

(defun make-mode-pedantic (&optional mode line-width)
  (interactive)
  (message "hihihi")
  (let ((width (if (null line-width) 81 (1+ line-width))))
    (font-lock-add-keywords
     mode
     `(("[ \t]+$" (0 'cc/bad-whitespace-face t))
       ("\t+" (0 'cc/bad-whitespace-face t))
       (,(format "^%s\\(.+\\)" (make-string width ?.)) (1 font-lock-warning-face t))
       ))))
(add-hook 'python-mode-hook 'make-mode-pedantic)
(add-hook 'lisp-mode-hook 'make-mode-pedantic)
(add-hook 'emacs-lisp-mode-hook 'make-mode-pedantic)

(font-lock-add-keywords 'c++-mode (font-lock-width-keyword 80))
(font-lock-add-keywords 'java-mode (font-lock-width-keyword 100))
(font-lock-add-keywords 'python-mode (font-lock-width-keyword 80))
