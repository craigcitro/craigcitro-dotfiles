;; emacs config

(message "Loading .emacs ...")

;;=======================================
;; Global Settings
;;=======================================

(defvar cc/dot-emacs-loaded nil
  "Whether or not my .emacs finished loading successfully.")

(ansi-color-for-comint-mode-on)
(global-font-lock-mode t)

(show-paren-mode t)
(auto-fill-mode t)
(column-number-mode t)
(setq inhibit-splash-screen t)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(display-time-mode t)
(setq message-log-max 300)
(setq make-backup-files nil)
(setq completion-ignored-extensions '(".a" ".so" ".o" "~" ".bak" ".class"))
(setq vc-follow-symlinks t)
(setq sentence-end-double-space nil)
(fset 'yes-or-no-p 'y-or-n-p)
(when (boundp 'x-select-enable-clipboard)
  (setq x-select-enable-clipboard t))
(setq line-move-visual nil)
(setq split-width-threshold nil)
(setq scroll-margin 2)
(setq recenter-positions '(top middle bottom))
(xterm-mouse-mode)

(setq-default indent-tabs-mode nil)
(setq-default major-mode 'paragraph-indent-text-mode)
(setq-default require-final-newline t)

(defun cc/disabled-command-message (&rest args)
  "Show a simple message when a command has been disabled."
  (interactive)
  (message "Command %s has been disabled"
           (substring (this-command-keys) 0 -1)))
(setq disabled-command-function 'cc/disabled-command-message)

(normal-erase-is-backspace-mode nil)

;; package initialization
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; start the server if it's not already up
(defconst default-server-name "craigcitro" "Default server name.")
(require 'server)
(let ((cc/server-name (or (getenv "EMACS_SERVERNAME") default-server-name)))
  (unless (server-running-p cc/server-name)
    (setq server-name cc/server-name)))
;; this isn't strictly required, but will cause simultaneous server starts
;; to (hopefully?) fail faster.
(server-start nil)

;;--------------------
;; utilities
;;--------------------
(defun cc/first-non-nil (ls)
  (cond
   ((null ls) nil)
   ((null (car ls)) (cc/first-non-nil (cdr ls)))
   (t (car ls))))

(defun cc/find-file-or-nil (path &optional prefix)
  (let ((dirs (list ""
                    prefix
                    (getenv "HOME")
                    (concat (getenv "HOME") "/.emacs.d/lisp")
                    (concat (getenv "HOME") "/ext")
                    (concat (getenv "HOME") "/ext/share/emacs/site-lisp"))))
    (cc/first-non-nil
     (mapcar (lambda (x)
               (let ((full-path (command-line-normalize-file-name (concat x "/" path))))
                 (when (file-exists-p full-path) full-path)))
             dirs))))

(defun cc/add-to-load-path-if-exists (path &optional prefix)
  "Add a path to load-path if it exists."
  (let ((full-path (cc/find-file-or-nil path prefix)))
    (when (and full-path (file-exists-p full-path))
      (add-to-list 'load-path full-path))))

;; Set up local path for lisp files
(cc/add-to-load-path-if-exists ".emacs.d/lisp")
(cc/add-to-load-path-if-exists "share/emacs/site-lisp")

;; select-frame is used in after-make-frame-functions, which is
;; annoying since I can't then run those hooks at startup. Replace it with
;; this more robust option:
(defun robust-select-frame (&optional frame norecord)
  "A version of select-frame that can take no arguments."
  (when frame
    (select-frame frame norecord)))
(when (memq 'select-frame after-make-frame-functions)
  (setq after-make-frame-functions (cons 'robust-select-frame
                                         (remq 'select-frame
                                               after-make-frame-functions))))

;; It's annoying that this is only controlled by the global variable;
;; this is the best way around it I can find:
(defun next-visual-line (&optional arg try-vscroll)
  "Move down a visual line."
  (interactive)
  (progn
    (setq line-move-visual t)
    (next-line arg try-vscroll)
    (setq line-move-visual nil)))
(defun previous-visual-line (&optional arg try-vscroll)
  "Move up a visual line."
  (interactive)
  (progn
    (setq line-move-visual t)
    (previous-line arg try-vscroll)
    (setq line-move-visual nil)))
(global-set-key "\C-n" 'next-visual-line)
(global-set-key "\C-p" 'previous-visual-line)

;; I'm not a fan of horizontal scrolling.
(put 'scroll-left 'disabled t)
(put 'scroll-right 'disabled t)
(global-unset-key "\C-x<")
(global-unset-key "\C-x>")

;;------------------------------------------------------------
;; ido
;;------------------------------------------------------------
(require 'ido)
(ido-mode t)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(add-to-list 'completion-ignored-extensions ".pyc")

;; \C-x\C-b is too close to \C-xb
(global-unset-key "\C-x\C-b")
(global-set-key "\C-x\C-b" 'ido-switch-buffer)
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

(defface cc/ido-system-buffer-face
  '((t (:foreground "BrightBlack")))
  "*Face used to highlight system buffers in the ido matches list."
  :group 'cc/ido-faces)
(defface cc/ido-this-buffer-face
  '((t (:foreground "Blue")))
  "*Face used to highlight the current buffer in the ido matches list."
  :group 'cc/ido-faces)
(defun cc/ido-colorize-bufname (&optional buf-name)
  "Return the name of the given or current buffer, propertized with a color
   as follows:
     BrightBlack: buffer is a system buffer
     Blue: this is the current buffer"
  (let* ((buf (or (get-buffer buf-name) (current-buffer)))
         (buf-name (buffer-name buf)))
    (cond
     ;; buf is current buffer
     ((string= buf-name (buffer-name (current-buffer)))
      (propertize buf-name 'face 'cc/ido-this-buffer-face))
     ;; system buffer
     ((char-equal ?* (elt buf-name 0))
      (propertize buf-name 'face 'cc/ido-system-buffer-face))
     ;; buf or current buffer is not in a git repo
     (t buf-name))))
(defun cc/filter-buffers ()
  (when (boundp 'ido-temp-list)
    (setq ido-temp-list
          (mapcar 'cc/ido-colorize-bufname ido-temp-list))))
(add-hook 'ido-make-buffer-list-hook 'cc/filter-buffers)

;;------------------------------------------------------------
;; Buffer naming
;;------------------------------------------------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;;======================================================
;; Extra config
;;======================================================
(let ((local-config (cc/find-file-or-nil ".emacs.local")))
  (when local-config
    (load-file local-config)))
(let ((gconfig (cc/find-file-or-nil ".emacs.google")))
  (when gconfig
    (load-file gconfig)))

;; (2011 Jun 27) I have no idea why this isn't already set:
(defun byte-compile-dest-file (filename)
  (concat filename "c"))

(defun cc/indent-region-rigidly (count start end)
  "Indent the region rigidly by count, using indent-code-rigidly."
  (indent-code-rigidly start end count))
(defun cc/shift-left (&optional start end moves)
  (interactive "r\np")
  (let ((count (* -2 (if (null moves) 1 moves))))
    (cc/indent-region-rigidly count start end)))
(defun cc/shift-right (&optional start end moves)
  (interactive "r\np")
  (let ((count (* 2 (if (null moves) 1 moves))))
    (cc/indent-region-rigidly count start end)))
(global-set-key "\C-c<" 'cc/shift-left)
(global-set-key "\C-c>" 'cc/shift-right)

;;===========================================
;; Context-dependent config
;;===========================================

;; (defun in-terminal (&optional frame)
;;   "Determine whether or not we seem to be in a terminal."
;;   (not (display-multi-frame-p (or frame (selected-frame)))))

;; (defun various-mac-setup (&optional frame)
;;   "Run a handful of Mac-specific configuration commands."
;;   (when (eq 'darwin system-type)
;;     ;; Set colors
;;     (modify-frame-parameters frame '((foreground-color . "ivory")
;;                                      (background-color . "black")))))
;; (add-to-list 'after-make-frame-functions 'various-mac-setup)

;;===========================================
;; Set up the window
;;===========================================

;; Clean up the window. Each of these sets a global option, so
;; there's no need to use this as a hook.
(defun kill-trim (&optional ignored)
  "Kill all the extras: menu, scrollbar, toolbar. Takes
an (ignored) optional argument so it can be used as a hook in
after-make-frame-functions."
  (menu-bar-mode -1)
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (tool-bar-mode -1))
(kill-trim)

;;==============================================================================
;; Major modes and language-specific config
;;==============================================================================

(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode))

;;---------------
;; text
;;---------------
;; Paragraphs get indented
(add-to-list 'auto-mode-alist '("\\.txt$" . paragraph-indent-text-mode))
;; Set auto-fill and abbreviation for text
(setq text-mode-hook
      '(lambda nil
         (setq fill-column 78)
         (auto-fill-mode 1)
         (abbrev-mode 1)))
;; add this at the end
(add-to-list 'auto-mode-alist '("config$" . conf-unix-mode) t)

;;---------------
;; scratch
;;---------------
(cc/add-to-load-path-if-exists "scratch-el")
(require 'scratch nil t)

;;-------------------
;; ReST and Markdown
;;-------------------
(when (require 'rst nil t)
  (add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))
  (add-to-list 'auto-mode-alist '("\\.rest$" . rst-mode)))

(cc/add-to-load-path-if-exists "markdown-mode")
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(when (require 'markdown-mode nil t)
  (when (boundp 'markdown-mode-map)
    (define-key markdown-mode-map "\M-\r" 'markdown-insert-list-item)
    (define-key markdown-mode-map "\M-=" 'markdown-demote)
    (define-key markdown-mode-map "\M--" 'markdown-promote))
  (dolist (extension '("md" "Rmd" "mdml" "markdown"))
    (add-to-list 'auto-mode-alist `(,(format "\\.%s$" extension) . markdown-mode)))
  )

;;------------------
;; Makefiles
;;------------------
(add-to-list 'auto-mode-alist '("^Makefile$" . makefile-bsdmake-mode))

;;---------------------------
;; Python
;;---------------------------
;; TODO(craigcitro): Do I need any of this?
;; (when (require 'python nil t)
;;   (setq python-indent 2)
;;   (defun cc/python-indent-region ()
;;     (interactive)
;;     (python-indent-region (region-beginning) (region-end)))
;;   (define-key python-mode-map "\C-ca" 'cc/python-indent-region)
;;   (define-key inferior-python-mode-map "\C-p" 'comint-previous-input)
;;   (define-key inferior-python-mode-map "\C-n" 'comint-next-input)
;;   )
(add-to-list 'auto-mode-alist '("\\.?pythonrc$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.?pdbrc$" . python-mode))

;; Nick Alexander and I wrote this at SD12
(defun bs (name)
  "Browse the structure of a Python/Cython file."
  ;; (occur "^\( *def\\|class\\|cdef class\\).*:$"))
  (interactive "sFind name in hierarchy: ")
  (occur (format "^ *\\(def.*%s\\|class\\).*:$" name)))

;;---------------------
;; flymake
;;---------------------
;; (require 'cc/flymake-keys "flymake-keys")
;; (eval-after-load 'flymake
;;   '(progn
;;      (require 'flymake-cursor)
;;      (setq flymake-cursor-number-of-errors-to-display 4)))

;;-------------------------
;; Java
;;-------------------------
;; I shouldn't need this ...
(unless (assoc "\\.java$" auto-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.java$" . java-mode)))

;;---------------------
;; yaml
;;---------------------
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))


;;-------------------------
;; js and friends
;;-------------------------
;; (eval-after-load 'js-mode
;;   (add-hook 'js-mode-hook (lambda () (setq js-indent-level 2))))
;; (require 'jinja2-mode nil t)
;; (when (require 'css-mode nil t)
;;   (add-hook 'css-mode-hook (lambda () (setq css-indent-offset 2))))

;;------------------------
;; Emacs Lisp
;;------------------------
(add-to-list 'auto-mode-alist '("[./-]emacs$" . emacs-lisp-mode))
;; A basic "step and execute" function -- am I reinventing this wheel?
(defun cc/eval-sexp-and-advance (line-mode)
  "Eval the top-level containing sexp. If the next line after
  this sexp is blank, do nothing. If next line is not blank, move
  to the end of that sexp. This command can be repeated by
  pressing the last key in the binding.

  With any prefix arg, steps by sexps at the current level."
  (interactive "P")
  (let ((step (lambda (&optional forward-first)
                (if line-mode
                    (progn
                      (if forward-first
                          (forward-sexp)
                        (end-of-line)))
                  (progn
                    (if forward-first
                        (forward-line))
                    (end-of-defun)
                    (forward-line -1)
                    (end-of-line)))))
        (continue t)
        (final-position (point)))
    (funcall step)
    (while continue
      (let ((val (eval-last-sexp nil)))
        (setq final-position (point))
        (funcall step t)
        (message "(Type e to execute next sexp) Last result: %s" val)
        (unless (equal (event-basic-type ?e) (read-event))
          (setq continue nil))))
    (when last-input-event
      (clear-this-command-keys t)
      (if (equal (event-convert-list '(E)) last-input-event)
          (eval-last-sexp nil)
        (progn
          (setq unread-command-events (list last-input-event))
          (goto-char final-position))))))
(global-set-key "\C-c\C-e" 'cc/eval-sexp-and-advance)

;; tab completion in the Eval: prompt!
(define-key read-expression-map [(tab)] 'hippie-expand)
(define-key read-expression-map [(control i)] 'hippie-expand)
(define-key read-expression-map [(backtab)] "\C-u\C-i")
(define-key read-expression-map "\C-p" 'previous-history-element)
(define-key read-expression-map "\C-n" 'next-history-element)

;;------------------------
;; Shell scripts
;;------------------------
(add-to-list 'auto-mode-alist '("bash[^/]*$" . shell-script-mode) t)
;; add rc files as shell as a last resort
(add-to-list 'auto-mode-alist '("rc$" . shell-script-mode) t)
(add-hook 'sh-mode-hook
          '(lambda ()
             (setq sh-basic-offset 2)
             (setq sh-indentation 2)))

;;------------------------
;; ess-mode
;;------------------------
(cc/add-to-load-path-if-exists "ess")
(when (require 'ess-site nil t)
  (add-to-list 'auto-mode-alist '("\\.R$" . r-mode))
  (add-to-list 'auto-mode-alist '("\\.r$" . r-mode))
  (add-to-list 'auto-mode-alist '("\\.valclass$" . r-mode))
  (add-to-list 'auto-mode-alist '("Rprofile$" . r-mode))
  ;; We want to force _ back to _; ess-toggle-underscore can
  ;; force it to "smart _", but not off. Ugh.
  (ess-toggle-underscore 1)
  (ess-toggle-underscore nil)
  (defun cc/ess-indentation-hook ()
    (when (boundp 'ess-indent-level)
      (setq ess-indent-level 2)))
  (add-hook 'ess-mode-hook 'cc/ess-indentation-hook)
  ;; Indent 4 spaces on a continued line in parens
  (when (boundp 'ess-arg-function-offset)
    (setq ess-arg-function-offset 4))
  ;; no one puts my keymappings in a corner
  (when (boundp 'inferior-ess-mode-map)
    (define-key inferior-ess-mode-map "\C-p" 'comint-previous-input)
    (define-key inferior-ess-mode-map "\C-n" 'comint-next-input))
  (when (boundp 'ess-fancy-comments)
    (setq ess-fancy-comments nil))
)

;;==============================================================================
;; Utility functions
;;==============================================================================

;; Trailing whitespace
(defvar cc/skip-delete-trailing-whitespace nil
  "If t, skip any potential call to delete trailing whitespace.
   Intended to be used as a buffer-local variable.")
(make-variable-buffer-local 'cc/skip-delete-trailing-whitespace)
(defun cc/possibly-delete-trailing-whitespace ()
  (when (and (memq major-mode '(python-mode))
             (null cc/skip-delete-trailing-whitespace))
    (delete-trailing-whitespace)))
(add-to-list 'write-file-functions 'cc/possibly-delete-trailing-whitespace)

;; I'm sure this has to exist somewhere in emacs already ...
(defun get-cursor-position-as-integer ()
  "Return the current cursor position as an integer."
  (interactive)
  (string-to-number (substring (what-line) 5)))

;; Sometimes it's nice to easily find out a keycode: to do this,
;; \M-: (read-event "?") or just run this function:
(defun get-keycode ()
  "Wrapper around read-event for when I forget it exists."
  (interactive)
  (read-event "Hit a key: "))

;; Insert the current date in parentheses.
;; TODO: make the parentheses customizable.
;; TODO: take a prefix argument to insert different dates.
(defun insert-date (&optional prefix)
  "Insert the current date in parentheses into the buffer."
  (interactive "P")
  (if prefix
      (let ((date (format-time-string "%Y %b %d")))
        (insert date)
        (insert ?\n)
        (insert (make-string (length date) ?=))
        (insert ?\n))
    (progn
      (insert (format-time-string "(%Y %b %d)"))
      (insert " "))))
(global-set-key "\C-c\C-d" 'insert-date)

(defun cc/update-alist (alist key &optional val)
  "Update alist to contain (key . val). If key is already a key
  in alist, we replace the existing entry. If val is nil and key
  is a list, instead call (cc/update-alist alist k v) for every
  pair (k . v) in key (i.e. view key as a list of updates)."
  (cond
   ((null key) alist)
   ((and (null val) (listp key))
    (cc/update-alist (cc/update-alist alist (caar key) (cdar key))
                  (cdr key)))
   ((null alist) (list (cons key val)))
   ((equal key (caar alist)) (cons (cons key val) (cdr alist)))
   (t (cons (car alist) (cc/update-alist (cdr alist) key val)))))

;;==============================================================================
;; MISC KEY SETTINGS
;; ==============================================================================
;; Overrides of defaults
(global-set-key "\C-t" 'fill-paragraph)
(global-set-key "\C-w" 'kill-ring-save)
(global-set-key "\M-w" 'kill-region)
(global-set-key "\C-xa" 'mark-whole-buffer)
(global-set-key "\C-x3" 'split-window-vertically)
(global-set-key "\C-x\C-c" 'delete-frame)
(global-set-key "\C-x\C-y" 'save-buffers-kill-emacs)
(global-set-key "\C-x\C-n" 'new-frame)
;; Handy choices
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key "\C-c\C-z" 'shell)
(global-set-key "\M-s" 'isearch-forward-regexp)
(global-set-key "\M-r" 'isearch-backward-regexp)
(global-set-key "\C-ct" 'transpose-chars)
(global-set-key "\C-c\C-t" 'transpose-words)
(global-set-key [C-backspace] 'backward-kill-word)
(global-set-key [C-delete] 'backward-kill-word)
(global-set-key [(control :)] 'eval-expression)
(global-set-key "\C-c#" 'comment-or-uncomment-region)
(global-set-key "\C-cs" 'sort-lines)
(global-set-key "\C-ca" 'indent-region)
(global-set-key "\C-c!" 'toggle-read-only)
(global-set-key "\M-g\M-o" 'occur)
;; Sloppy keys
(global-set-key "\C-x\C-g" 'keyboard-quit)
(global-unset-key [(control x) (control o)])
(global-set-key "\C-x\C-o" 'other-window)
(global-set-key "\C-xs" 'save-buffer)
;; Minibuffer keys
(define-key minibuffer-local-map "\C-p" 'previous-history-element)
(define-key minibuffer-local-map "\C-n" 'next-history-element)
;; This one continually causes me trouble.
(global-unset-key "\M-t")
(global-set-key "\M-p" 'transpose-words)
;; Usability-in-chrome fixes.
(global-set-key "\C-\M-w" 'kill-region)
(global-set-key "\C-\M-v" 'down-one-bounded-page)

;; How did these get disabled?
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; vim-inspired
(defun join-below ()
  "Join the next line to the current line."
  (interactive)
  (delete-indentation t))
(global-set-key "\M-^" 'join-below)
(global-set-key "\C-^" 'delete-indentation)
(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(global-set-key "\M-z" 'down-one)
(global-set-key "\C-z" 'up-one)
(global-set-key [C-S-up] 'down-one)
(global-set-key [C-S-down] 'up-one)

;; get that mouse scroll going:
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

;; Sane pageup/pagedown.
(defvar largest-page-movement-size '40
  "Largest reasonable size for pageup/pagedown.")
(defun up-one-bounded-page ()
  "Scroll up one manageable page."
  (interactive)
  (forward-line (min (/ (frame-height) 2) largest-page-movement-size)))
(defun down-one-bounded-page ()
  "Scroll down one manageable page."
  (interactive)
  (forward-line (* -1 (min (/ (frame-height) 2) largest-page-movement-size))))
(defun adjust-pageup-pagedown (&optional frame)
  "Adjust page size based on the current frame size."
  (when (> (frame-height frame) largest-page-movement-size)
    (global-set-key "\M-v" 'down-one-bounded-page)
    (global-set-key "\C-v" 'up-one-bounded-page)
    (global-set-key [M-up] 'down-one-bounded-page)
    (global-set-key [M-down] 'up-one-bounded-page)))
(adjust-pageup-pagedown)
(add-to-list 'after-make-frame-functions 'adjust-pageup-pagedown)
(global-set-key [M-up] 'down-one-bounded-page)
(global-set-key [M-down] 'up-one-bounded-page)

;; I did this to get M-DEL working in chromeos; maybe there's a better way?
(global-set-key [deletechar] 'backward-kill-word)

;; revert-buffer from:
;;  http://www.stokebloke.com/wordpress/2008/04/17/emacs-refresh-f5-Key/
(defun revert-buffer-keep-history (&optional IGNORE-AUTO NOCONFIRM PRESERVE-MODES)
  (interactive)

  (let ((point (point))
        (window-start (window-start)))
    (unwind-protect
        (progn
          ;; tell Emacs the modtime is fine, so we can edit the buffer
          (clear-visited-file-modtime)

          ;; insert the current contents of the file on disk
          (widen)
          (delete-region (point-min) (point-max))
          (insert-file-contents (buffer-file-name))

          ;; mark the buffer as not modified
          (not-modified)
          (set-visited-file-modtime))
      (goto-char point)
      (set-window-start (selected-window) window-start nil)
      )))
(setq revert-buffer-function 'revert-buffer-keep-history)

(defun reload-buffer ()
  "Reload the current buffer with the current cursor position."
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t))
(global-set-key "\C-c\C-r" 'reload-buffer)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-A ((((type tty)) (:weight light :foreground "firebrick" :background "pale green"))))
 '(ediff-current-diff-B ((((type tty)) (:weight light :foreground "firebrick" :background "pale green"))))
 '(ediff-fine-diff-A ((((type tty)) (:weight light :foreground "navy" :background "sky blue"))))
 '(ediff-fine-diff-B ((((type tty)) (:weight light :foreground "navy" :background "sky blue"))))
 '(flycheck-error ((t (:background "red" :weight normal))))
 '(flycheck-info ((t (:background "blue"))))
 '(flycheck-warning ((t (:background "color-91")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column 79)
 '(flycheck-display-errors-delay 0.2)
 '(flycheck-highlighting-mode (quote lines))
 '(global-flycheck-mode t nil (flycheck))
 '(safe-local-variable-values (quote ((c-indent-level . 2))))
 '(tooltip-mode nil))

;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; NO OTHER CODE BELOW THIS COMMAND
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
(run-hooks 'after-make-frame-functions)

(message "... finished reading .emacs.")
(setq cc/dot-emacs-loaded t)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
