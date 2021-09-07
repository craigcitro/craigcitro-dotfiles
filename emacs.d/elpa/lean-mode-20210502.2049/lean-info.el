;; -*- lexical-binding: t -*-
;;
;;; lean-info.el --- Emacs mode for Lean theorem prover
;;
;; Copyright (c) 2016 Gabriel Ebner. All rights reserved.
;;
;; Author: Gabriel Ebner <gebner@gebner.org>
;; Maintainer: Gabriel Ebner <gebner@gebner.org>
;; Created: Oct 29, 2016
;; Keywords: languages
;; Version: 0.1
;; URL: https://github.com/leanprover/lean/blob/master/src/emacs
;;
;; Released under Apache 2.0 license as described in the file LICENSE.
;;

(require 'cl-lib)
(require 'lean-syntax)

;; Lean Info Mode (for "*lean-info*" buffer)
;; Automode List
;;;###autoload
(define-derived-mode lean-info-mode prog-mode "Lean-Info"
  "Major mode for Lean Info Buffer"
  :syntax-table lean-syntax-table
  :group 'lean
  (set (make-local-variable 'font-lock-defaults) lean-info-font-lock-defaults)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set 'compilation-mode-font-lock-keywords '())
  (set-input-method "Lean")
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function))

(cl-defmacro lean-with-info-output-to-buffer (buffer &rest body)
  `(let ((buf (get-buffer ,buffer)))
     (with-current-buffer buf
       (setq buffer-read-only nil)
       (erase-buffer)
       (setq standard-output buf)
       . ,body)))

(defvar lean-info-buffer-names '()
  "These are the buffer names that `lean-toggle-info-buffer` will
  look to see if have an open window, and if so, replace that
  window's buffer.")

(defun lean-ensure-info-buffer (buffer)
  (if (stringp buffer)
      (add-to-list 'lean-info-buffer-names buffer))
  (let ((buf (get-buffer buffer)))
    (unless buf
      (setq buf (get-buffer-create buffer))
      (with-current-buffer buf
        (buffer-disable-undo)
        (lean-info-mode)))
    buf))

(defun lean-locate-info-window ()
  "Finds a window containing a buffer from
`lean-info-buffer-names`, or nil if one does not exist"
  (let (window)
    (dolist (buffer lean-info-buffer-names window)
      (setq window (or window (get-buffer-window buffer))))))

(defun lean-toggle-info-buffer (buffer)
  "If there is an info window and it corresponds to the given
buffer, delete the window.  Otherwise, switch the info window to
the given buffer, creating an info window if it does not already
exist."
  (let* ((buf (lean-ensure-info-buffer buffer))
         (window (or (get-buffer-window buf) (lean-locate-info-window))))
    (cond
     ((and window (eq (window-buffer window) buf))
      (delete-window window))
     (window
      (with-selected-window window
        (switch-to-buffer buf)))
     (t
      (display-buffer buf)))))

(defun lean-info-buffer-active (buffer)
  "Checks whether the given info buffer should show info for the current buffer"
  (and
   ;; info buffer visible
   (get-buffer-window buffer 'visible)
   ;; current window of current buffer is selected (i.e., in focus)
   (eq (current-buffer) (window-buffer))))

(defun lean-get-info-record-at-point (cont)
  "Get info-record at the current point"
  (with-demoted-errors "lean get info: %s"
    (lean-server-send-command
     'info (list :file_name (buffer-file-name)
                 :line (line-number-at-pos)
                 :column (lean-line-offset))
     (cl-function
      (lambda (&key record)
        (funcall cont record))))))

(defun lean-info-right-click-find-definition ()
  "Offer to jump to definition of right-click target."
  (interactive)
  (list 'info
        (list :file_name (buffer-file-name)
              :line (line-number-at-pos)
              :column (lean-line-offset))
        (cl-function
         (lambda (&key record)
           (let ((source-record (plist-get record :source)))
             (if source-record
                 (let ((full-name (plist-get record :full-id)))
                   (list
                    (list :name (if full-name
                                    (concat "Find definition of " full-name)
                                  "Find definition")
                          :action (lambda ()
                                    (apply #'lean-find-definition-cont source-record)))))
               (list)))))))

(cl-defun lean-find-definition-cont (&key file line column)
  (when (fboundp 'xref-push-marker-stack) (xref-push-marker-stack))
  (when file
    (find-file file))
  (goto-char (point-min))
  (forward-line (1- line))
  (forward-char column))


(defun lean-find-definition ()
  "Jump to definition of thing at point"
  (interactive)
  (setq lean-show-goal--handler-mask t) ; avoid the current request to the Lean server to by
                                        ; interrupted by requests made for `lean-show-goal`
  (lean-get-info-record-at-point
   (lambda (info-record)
     (-if-let (source-record (plist-get info-record :source))
         (apply #'lean-find-definition-cont source-record)
       (-if-let (id (plist-get info-record :full-id))
           (message "no source location available for %s" id)
         (message "unknown thing at point"))))))

(provide 'lean-info)
