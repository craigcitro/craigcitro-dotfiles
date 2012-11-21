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
      )))
(add-hook 'find-file-hook 'cc/flymake-by-mode-find-file-hook)

(provide 'cc/flymake-by-mode)
