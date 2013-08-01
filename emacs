;; -*- mode: Emacs-lisp; lexical-binding: t -*-
;;
;; emacs configuration
;;

(message "Starting .emacs ...")
;; There's simply no way forward without basic programming
;; facilities.
(require 'cl)

;;=======================================
;; Global Settings
;;=======================================

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
(setq x-select-enable-clipboard t)
(setq line-move-visual nil)
(setq tooltip-use-echo-area t)
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
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

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
(defun cc/empty-or-nil-p (x)
  (or (null x) (string= "" x)))
(defun cc/any (ls)
  (reduce '(lambda (x y) (or x y)) ls))
(defun cc/all (ls)
  (reduce '(lambda (x y) (and x y)) ls))
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

;; I haven't used this much yet, but it seems like it could be cool.
;; (2010 Sep 24) Okay, this is exactly as described: you only need it
;; every so often, but when you do, it's unreasonably good.
(require 'browse-kill-ring)

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
(setq auto-hscroll-mode nil)
(put 'scroll-left 'disabled t)
(put 'scroll-right 'disabled t)
(global-unset-key "\C-x<")
(global-unset-key "\C-x>")

;;------------------------------------------------------------
;; iswitchb
;;------------------------------------------------------------
(iswitchb-mode t)
;; \C-x\C-b is too close to \C-xb
(global-unset-key "\C-x\C-b")
(global-set-key "\C-x\C-b" 'iswitchb-buffer)
(defun iswitchb-local-keys ()
  (define-key iswitchb-mode-map "\C-r" 'iswitchb-prev-match)
  (define-key iswitchb-mode-map "\C-s" 'iswitchb-next-match)
  (define-key iswitchb-mode-map [right] 'iswitchb-next-match)
  (define-key iswitchb-mode-map [left] 'iswitchb-prev-match)
  (define-key iswitchb-mode-map [down] 'iswitchb-next-match)
  (define-key iswitchb-mode-map [up] 'iswitchb-prev-match))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)
;; (2010 Oct 01) It's curious to me why I spontaneously started
;; needing this:
(setq iswitchb-default-method 'samewindow)

;; (2011 Sep 24) Experimenting with smarter iswitchb configuration.
;; TODO(craigcitro): Use symbols instead of strings.
(defface cc/iswitchb-same-branch-face
  '((t (:foreground "Green")))
  "*Face used to highlight iswitchb matches in the same git repo/branch.")
(defface cc/iswitchb-different-branch-face
  '((t (:foreground "Red")))
  "*Face used to highlight iswitchb matches in the same git repo, but a different branch.")
(defface cc/iswitchb-system-buffer-face
  '((t (:foreground "BrightBlack")))
  "*Face used to highlight system buffers in the iswitchb matches list.")
(defface cc/iswitchb-this-buffer-face
  '((t (:foreground "Blue")))
  "*Face used to highlight the current buffer in the iswitchb matches list.")
(defun cc/parse-git-branch (&optional buf)
  "Get the git branch from a buffer."
  (let ((git-info (buffer-local-value 'vc-mode (or buf (current-buffer)))))
    (if (null git-info)
        ""
      (substring-no-properties git-info (length " Git-")))))
(defstruct (cc/buffer-git-info
            (:constructor cc/make-git-info))
  "Git-specific info about a given buffer."
  root branch filename)
(defun cc/make-git-info-from-buffer (&optional buf)
  (let* ((buf (or buf (current-buffer)))
         (filename (buffer-file-name buf)))
    (when (and (not (cc/empty-or-nil-p filename))
               (fboundp 'vc-git-root)
               (vc-git-root filename))
      (cc/make-git-info
       :root (vc-git-root filename)
       :branch (cc/parse-git-branch buf)
       :filename filename))))
(defun cc/iswitchb-colorize-bufname (&optional buf-name)
  "Like the next one, but less wacky."
  (let* ((buf (or (get-buffer buf-name) (current-buffer)))
         (buf-name (buffer-name buf)))
    (cond
     ;; buf is current buffer
     ((string= buf-name (buffer-name (current-buffer)))
      (propertize buf-name 'face 'cc/iswitchb-this-buffer-face))
     ;; system buffer
     ((char-equal ?* (elt buf-name 0))
      (propertize buf-name 'face 'cc/iswitchb-system-buffer-face))
     ;; buf or current buffer is not in a git repo
     (t buf-name))))
(defun cc/iswitchb-colorize-bufname-crazy (&optional buf-name)
  "Return the name of the given or current buffer, propertized with a color
   as follows:
     BrightBlack: buffer is a system buffer
     Blue: this is the current buffer
   When called from a buffer in a git repo:
     Green: buffer is in the same repo and branch as current
     Red: buffer is in the same repo and a DIFFERENT branch as current
   Files in different git repos return nil."
  (let* ((buf (or (get-buffer buf-name) (current-buffer)))
         (buf-name (buffer-name buf)))
    (let ((buf-git-info (cc/make-git-info-from-buffer buf))
          (current-git-info (cc/make-git-info-from-buffer)))
      (cond
       ;; buf is current buffer
       ((string= buf-name (buffer-name (current-buffer)))
        (propertize buf-name 'face 'cc/iswitchb-this-buffer-face))
       ;; system buffer
       ((char-equal ?* (elt buf-name 0))
        (propertize buf-name 'face 'cc/iswitchb-system-buffer-face))
       ;; buf or current buffer is not in a git repo
       ((or (null buf-git-info) (null current-git-info)) buf-name)
       ;; current buffer is in the same git repo as buf
       ((string= (cc/buffer-git-info-root buf-git-info)
                 (cc/buffer-git-info-root current-git-info))
        (propertize
         buf-name 'face
         (if (string= (cc/buffer-git-info-branch buf-git-info)
                      (cc/buffer-git-info-branch current-git-info))
             'cc/iswitchb-same-branch-face
           'cc/iswitchb-different-branch-face)))
       ;; current buffer and buf are in different repos
       (t nil)))))
(defun cc/filter-buffers ()
  (setq iswitchb-temp-buflist
        (reverse (mapcar 'cc/iswitchb-colorize-bufname iswitchb-temp-buflist))))
(add-hook 'iswitchb-make-buflist-hook 'cc/filter-buffers)

;;------------------------------------------------------------
;; ido-based file switching
;;------------------------------------------------------------
;; I'm using ido for org-mode completion, so I'm also going to
;; try it out for finding files.
(ido-mode 'files)
(setq ido-max-directory-size 100000)
(add-to-list 'completion-ignored-extensions ".pyc")

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

(defun in-terminal (&optional frame)
  "Determine whether or not we seem to be in a terminal."
  (not (display-multi-frame-p (or frame (selected-frame)))))

(defun various-mac-setup (&optional frame)
  "Run a handful of Mac-specific configuration commands."
  (when (eq 'darwin system-type)
    ;; Is this necessary?
    ;; (robust-select-frame frame)
    ;; This is documented in the manual, but not in the description of
    ;; this variable. Set this to nil for option as meta, and non-nil
    ;; for command as meta. Given that I always use cmd-key-happy on any
    ;; Mac, nil is the right choice for me.
    (setq mac-command-key-is-meta nil)
    ;; Set colors
    (modify-frame-parameters frame '((foreground-color . "ivory")
                                     (background-color . "black")))))
(add-to-list 'after-make-frame-functions 'various-mac-setup)

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
  (add-to-list 'auto-mode-alist '("\\.rest$" . rst-mode))
  (cc/add-to-load-path-if-exists "markdown-mode")
  (autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.mdml$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode)))

;;------------------
;; Makefiles
;;------------------
(add-to-list 'auto-mode-alist '("^Makefile$" . makefile-bsdmake-mode))

;;---------------------------
;; Python
;;---------------------------
(when (require 'python)
  (provide 'python-mode) ;; bye-bye python-mode.el
  (setq python-indent 2)
  (defun cc/python-indent-region ()
    (interactive)
    (python-indent-region (region-beginning) (region-end)))
  (define-key python-mode-map "\C-ca" 'cc/python-indent-region)
  (define-key inferior-python-mode-map "\C-p" 'comint-previous-input)
  (define-key inferior-python-mode-map "\C-n" 'comint-next-input)
  )
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
(require 'cc/flymake-keys "flymake-keys")
(eval-after-load 'flymake
  '(progn
     (require 'flymake-cursor)
     (setq flymake-cursor-number-of-errors-to-display 4)))

;;-------------------------
;; Java
;;-------------------------
;; I shouldn't need this ...
(unless (assoc "\\.java$" auto-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.java$" . java-mode)))

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
(add-to-list 'auto-mode-alist '("bash[^/]*$" . shell-script-mode))
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
    (setq ess-indent-level 2))
  (add-hook 'ess-mode-hook 'cc/ess-indentation-hook)
  ;; Indent 4 spaces on a continued line in parens
  (setq ess-arg-function-offset 4)
  ;; no one puts my keymappings in a corner
  (define-key inferior-ess-mode-map "\C-p" 'comint-previous-input)
  (define-key inferior-ess-mode-map "\C-n" 'comint-next-input)
  )

;;------------------------
;; magit
;;------------------------
;; change magit diff colors: stolen from
;;   http://readystate4.com/2011/02/22/emacs-changing-magits-default-diff-colors/
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))
(when (require 'magit nil t)
  (global-set-key "\C-c\C-g" 'magit-status))

;;------------------------
;; ediff
;;------------------------
(custom-set-faces
 '(ediff-current-diff-A
   ((((type tty))
     (:weight light :foreground "firebrick" :background "pale green"))))
 '(ediff-current-diff-B
   ((((type tty))
     (:weight light :foreground "firebrick" :background "pale green"))))
 '(ediff-fine-diff-A
   ((((type tty))
     (:weight light :foreground "navy" :background "sky blue"))))
 '(ediff-fine-diff-B
   ((((type tty))
     (:weight light :foreground "navy" :background "sky blue"))))
 )

;;------------------------
;; pedantic coloring
;;------------------------
(defface cc/long-line-face
  '((t (:foreground "red")))
  "*Face used for long lines.")
(defface cc/bad-whitespace-face
  '((t (:background "red")))
  "*Face used for tabs or trailing whitespace.")

(defun cc/make-mode-pedantic (&optional mode line-width)
  (interactive)
  (let ((width (if (null line-width) 81 (1+ line-width))))
    (font-lock-add-keywords
     mode
     `(("[ \t]+$" 0 'cc/bad-whitespace-face t)
       ("\t+" 0 'cc/bad-whitespace-face t)
       (,(format "^%s\\(.+\\)" (make-string width ?.)) 1 font-lock-warning-face t)
       ))))
(add-hook 'python-mode-hook 'cc/make-mode-pedantic)
(eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook 'cc/make-mode-pedantic))
(when (require 'lisp-mode)
  (add-hook 'lisp-mode-hook 'cc/make-mode-pedantic)
  (add-hook 'emacs-lisp-mode-hook 'cc/make-mode-pedantic))
(when (require 'ess-site nil t)
  (add-hook 'ess-mode-hook 'cc/make-mode-pedantic))


;; Trailing whitespace
;; TODO(craigcitro): Wire this into pedantic mode.
(defun cc/possibly-delete-trailing-whitespace ()
  (when (eq 'markdown-mode major-mode)
    (delete-trailing-whitespace)))
(add-to-list 'write-file-functions 'cc/possibly-delete-trailing-whitespace)

;;==============================================================================
;; Utility functions
;;==============================================================================

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
(defun insert-date ()
  "Insert the current date in parentheses into the buffer."
  (interactive)
  (insert (format-time-string "(%Y %b %d)"))
  (insert " "))
(global-set-key "\C-c\C-d" 'insert-date)
;; (2010 Dec 02) HORRIBLE HACK
(defun insert-bare-date ()
  "Insert the current date."
  (interactive)
  (let ((date (format-time-string "%Y %b %d")))
    (insert date)
    (insert ?\n)
    (insert (make-string (length date) ?-))
    (insert ?\n)))
(global-set-key "\C-c\C-b" 'insert-bare-date)

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
  """Join the next line to the current line."""
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
;;  http://www.stokebloke.com/wordpress/2008/04/17/emacs-refresh-f5-key/
(defun reload-buffer ()
  "Reload the current buffer with the current cursor position."
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t))
(global-set-key "\C-c\C-r" 'reload-buffer)

;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; NO OTHER CODE BELOW THIS COMMAND
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
(run-hooks 'after-make-frame-functions)
