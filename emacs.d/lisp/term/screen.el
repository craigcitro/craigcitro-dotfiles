;; -*- no-byte-compile: t -*-
;; Treat a screen terminal similar to an xterm.

;; (2011 Jun 29) Quick hack to get tmux+256 color working
;; "Inspired" by:
;;   http://lists.gnu.org/archive/html/emacs-devel/2009-02/msg00780.html
;; which became:
;;   http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/term/screen.el

(load "term/xterm")

(defun terminal-init-screen ()
    "Terminal initialization function for screen."
      ;; Use the xterm color initialization code.
      (xterm-register-default-colors)
        (tty-set-up-initial-frame-faces))

;; screen.el ends here
