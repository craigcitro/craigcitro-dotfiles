;; A handful of configuration to configure flymake by mode instead of
;; by filename regexp.

;; Configure flymake for python
(defun cc/flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
                     ;; 'flymake-create-temp-with-folder-structure))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (let ((indent (cond
                   ((boundp 'python-indent) python-indent)
                   ((boundp 'py-indent-offset) py-indent-offset)
                   (t 2))))
      (list "fpylint"
            (list local-file
                  (format "--indent=%s" indent))))))

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
       (let ((overlays (save-excursion
                         (overlays-in
                          (progn (beginning-of-line) (point))
                          (progn (end-of-line) (point))))))
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
  "Minor mode with my flymake-related keybindings."
  nil
  nil
  :keymap cc/flymake-by-mode-keymap)

(defun cc/flymake-by-mode-on ()
  (unless cc/flymake-by-mode-minor-mode
    (cc/flymake-by-mode-minor-mode)))

(provide 'cc/flymake-by-mode)
