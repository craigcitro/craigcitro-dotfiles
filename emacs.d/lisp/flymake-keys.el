;; Flymake keybindings.

(defvar cc/flymake-keys-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-_" 'cc/echo-flymake-error)
    (define-key map "\C-x\C-k\C-n" 'flymake-goto-next-error)
    (define-key map "\C-x\C-k\C-p" 'flymake-goto-prev-error)
    map))

(define-minor-mode cc/flymake-keys-minor-mode
  "Minor mode with my flymake-related keybindings."
  :keymap cc/flymake-keys-keymap)

(defun cc/flymake-keys-on ()
  (unless cc/flymake-keys-minor-mode
    (cc/flymake-keys-minor-mode)))

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
     (cc/flymake-keys-on)
     ))

(defun cc/activate-flymake-keys ()
  (interactive)
  (when (memq 'flymake-mode minor-mode-list)
    (cc/flymake-keys-on)))

(add-hook 'find-file-hook 'cc/activate-flymake-keys t)
(provide 'cc/flymake-keys)
