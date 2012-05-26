;;
;; This is my .emacs file -- I stole this from Ryan long, long ago,
;; and started modifying it a fair bit recently.
;;

;;=======================================
;; Global Settings
;;=======================================

;; ANSI colors FTW!
(ansi-color-for-comint-mode-on)
;; Turn on fonts and syntax highlighting *always*
(global-font-lock-mode t)

;; show matching parens
(show-paren-mode t)
;; Automatically wrap over-long lines
(auto-fill-mode t)
;; I like seeing the column in the display at the bottom:
(column-number-mode t)
;; Don't need the splash screen
(setq inhibit-splash-screen t)
;; narrow-to-region sounds annoying
(put 'narrow-to-region 'disabled nil)
;; Spaces are better than tabs.
(setq indent-tabs-mode nil)
;; Mask password in shell
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
;; Display the time
(display-time-mode t)

;; Consistently add newlines at end of file
(setq require-final-newline t)

;; Here's a whole chunk I stole from nweiz.
;; Fewer annoying files laying around ...
(setq make-backup-files nil)
(setq completion-ignored-extensions '(".a" ".so" ".o" "~" ".bak" ".class"))
;; Save hitting "y" all the time
(setq vc-follow-symlinks t)
;; This is just smart:
(setq sentence-end-double-space nil)
;; I think I'm a grown-up.
(setq disabled-command-function nil)

;; I fiddled with this for a while, but this seems to be the right
;; setting for my Mac. If this isn't good in X on Ubuntu, I should
;; configure it based on that. Note that this can be annoying to get
;; right when a server starts in a terminal, but the clients live in X
;; (or vice-versa).
(normal-erase-is-backspace-mode nil)
;; this was my old code for handling it:
;; emacs gets easily confused about what backspace should do when the
;; server is running in a windowing system, but the client is in a
;; terminal. this teaches it.
;; (defun configure-backspace-behavior (&optional frame)
;;   "Tell emacs how to handle backspace."
;;   (normal-erase-is-backspace-mode (not (in-terminal))))
;; (add-to-list 'after-make-frame-functions 'configure-backspace-behavior)

;; It's nice when copy-paste is sane
(setq x-select-enable-clipboard t)
;; This is just handy
(fset 'yes-or-no-p 'y-or-n-p)

;; start the server if it's not already up
(defconst default-server-name "craigcitro"
  "Default server name to use when not in tmux.")
(require 'server)
(let ((cc-server-name (or (getenv "EMACS_SERVERNAME") default-server-name)))
  (unless (server-running-p cc-server-name)
    (setq server-name cc-server-name)))
(server-start nil)

;;--------------------
;; utilities
;;--------------------
(defun cc-first-non-nil (ls)
  (cond
   ((null ls) nil)
   ((null (car ls)) (cc-first-non-nil (cdr ls)))
   (t (car ls))))

(defun cc-find-file-or-nil (path &optional prefix)
  (let ((dirs (list ""
		    prefix
		    (getenv "HOME")
                    (concat (getenv "HOME") "/.emacs.d/lisp")
		    (concat (getenv "HOME") "/ext"))))
    (cc-first-non-nil
     (mapcar (lambda (x)
               (let ((full-path (command-line-normalize-file-name (concat x "/" path))))
                 (when (file-exists-p full-path) full-path)))
             dirs))))

(defun cc-add-to-load-path-if-exists (path &optional prefix)
  "Add a path to load-path if it exists."
  (let ((full-path (cc-find-file-or-nil path prefix)))
    (when (file-exists-p full-path)
      (add-to-list 'load-path full-path))))

;; Set up local path for lisp files
(cc-add-to-load-path-if-exists ".emacs.d/lisp")
(cc-add-to-load-path-if-exists "share/emacs/site-lisp")

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

;; Stole these from a wiki on Emacs 23 vs. 22 compatibility:
;; Make ^N work based on the document structure. The default setting
;; of 't' makes it change behavior based on how wide the Emacs window
;; is.
(setq line-move-visual nil)
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
;; Emacs 23 likes to pop up real X windows for tooltips, which is
;; highly annoying on slow connections, especially using VNC or NX.
;; This makes it use the echo-area like it used to.
(setq tooltip-use-echo-area t)
;; In some situations Emacs will try to guess whether to split
;; horizontally or vertically. Put a stop to that by changing
;; split-window-preferred-function, split-width-threshold, or
;; split-height-threshold:
(setq split-width-threshold nil)

;; Wow, scroll-left and scroll-right are horrific. Unbind them.
(global-unset-key "\C-x<")
(global-unset-key "\C-x>")

;;------------------------------------------------------------
;; Better buffer switching -- Nick Alexander showed me this.
;;------------------------------------------------------------
(iswitchb-mode t)
;; \C-x\C-b is too close to \C-xb
(global-unset-key "\C-x\C-b")
(global-set-key "\C-x\C-b" 'iswitchb-buffer)
;; Make arrows work in iswitchb menu, stolen from:
;;   http://www.emacswiki.org/emacs/IswitchBuffers
(require 'edmacro)
(defun iswitchb-local-keys ()
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("\C-r"    . (lambda ()
                         (interactive)
                         (setq iswitchb-temp-buflist iswitchb-buflist)))
          )))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)
;; (2010 Oct 01) It's curious to me why I spontaneously started
;; needing this:
(setq iswitchb-default-method 'samewindow)

;; (2011 Sep 24) Experimenting with smarter iswitchb configuration.
;; TODO(craigcitro): Use symbols instead of strings.
(defun cc-set-frame-params (&optional frame)
  (set-frame-parameter frame 'cc-git-branch (getenv "CC_GIT_BRANCH" frame))
  (set-frame-parameter frame 'cc-git-root (getenv "CC_GIT_ROOT" frame)))
(add-hook 'after-make-frame-functions 'cc-set-frame-params)
(defvar cc-buffer-git-root)
(defvar cc-buffer-git-branch)
(make-variable-buffer-local 'cc-buffer-git-root)
(make-variable-buffer-local 'cc-buffer-git-branch)
(defun cc-set-session-name (&optional frame)
  (setq cc-buffer-git-branch (frame-parameter frame 'cc-git-branch))
  (setq cc-buffer-git-root (frame-parameter frame 'cc-git-root)))
(add-hook 'find-file-hook 'cc-set-session-name)
(defface cc-iswitchb-same-branch-face
  '((t (:foreground "Green")))
  "*Face used to highlight iswitchb matches in the same git repo/branch.")
(defface cc-iswitchb-different-branch-face
  '((t (:foreground "Red")))
  "*Face used to highlight iswitchb matches in the same git repo, but a different branch.")
(defface cc-iswitchb-system-buffer-face
  '((t (:foreground "BrightBlack")))
  "*Face used to highlight system buffers in the iswitchb matches list.")
(defface cc-iswitchb-this-buffer-face
  '((t (:foreground "Blue")))
  "*Face used to highlight the current buffer in the iswitchb matches list.")
(defun empty-or-nil-p (x)
  (or (null x) (string= "" x)))
(defun cc-filter-buffers ()
  (let ((frame-git-root (frame-parameter nil 'cc-git-root))
        (frame-git-branch (frame-parameter nil 'cc-git-branch)))
    (let ((colored-buflist nil))
      (dolist (buf iswitchb-temp-buflist colored-buflist)
        (let* ((buffer (get-buffer buf))
               (buffer-git-branch (buffer-local-value
                                   'cc-buffer-git-branch buffer))
               (buffer-git-root (buffer-local-value
                                 'cc-buffer-git-root buffer)))
          (cond
           ((string= buf (buffer-name (current-buffer)))
            (add-to-list 'colored-buflist
                         (propertize buf 'face 'cc-iswitchb-this-buffer-face)))
           ((char-equal ?* (elt buf 0))
            (add-to-list 'colored-buflist
                         (propertize buf 'face 'cc-iswitchb-system-buffer-face)))
           ((empty-or-nil-p frame-git-root)
            (when (empty-or-nil-p buffer-git-root)
              (add-to-list 'colored-buflist buf)))
           ((or (null buffer-git-branch) (null buffer-git-root))
            (add-to-list 'colored-buflist buf))
           ((and (string= frame-git-root buffer-git-root)
                 (string= frame-git-branch buffer-git-branch))
            (add-to-list 'colored-buflist
                         (propertize buf 'face 'cc-iswitchb-same-branch-face)))
           ((string= frame-git-root buffer-git-root)
            (add-to-list 'colored-buflist
                         (propertize buf 'face 'cc-iswitchb-different-branch-face)))
           ((empty-or-nil-p buffer-git-root)
            (add-to-list 'colored-buflist buf)))))
      (setq iswitchb-temp-buflist (reverse colored-buflist)))))
(add-hook 'iswitchb-make-buflist-hook 'cc-filter-buffers)

;;------------------------------------------------------------
;; Buffer naming
;;------------------------------------------------------------
;; I'm pretty happy with uniquify and forward naming -- see details
;; here:
;;  http://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;======================================================
;; Extra config
;;======================================================
(let ((gconfig (cc-find-file-or-nil ".emacs.google")))
  (when gconfig
    (load-file gconfig)))

;; (2011 Jun 27) I have no idea why this isn't already set:
(defun byte-compile-dest-file (filename)
  (concat filename "c"))

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
    (robust-select-frame frame)
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
  (scroll-bar-mode -1)
  (tool-bar-mode -1))
(kill-trim)

;;==============================================================================
;; Major modes and language-specific config
;;==============================================================================

;;------------------
;; org-mode
;;------------------
;; This initial configuration is heavily "inspired" by:
;;   http://doc.norang.ca/org-mode.html

;; Undefine C-c [ and C-c ], in favor of explicit management of the
;; org-agenda-files variable.
(add-hook 'org-mode-hook
	  '(lambda ()
	     (org-defkey org-mode-map "\C-c[" 'undefined)
	     (org-defkey org-mode-map "\C-c]" 'undefined)
	     (org-defkey org-mode-map "\C-c;" 'undefined))
	  'append)
;; I want some single-chord shortcuts for movement; this seems
;; like a good set, but we'll see.
(add-hook 'org-mode-hook
	  '(lambda ()
	     (org-defkey org-mode-map "\M-\C-n" 'outline-next-visible-heading)
	     (org-defkey org-mode-map "\M-\C-p" 'outline-previous-visible-heading)
	     (org-defkey org-mode-map "\M-\C-f" 'org-forward-same-level)
	     (org-defkey org-mode-map "\M-\C-b" 'org-backward-same-level)
	     (org-defkey org-mode-map "\M-\C-u" 'outline-up-heading)))	  

(when (require 'org-install nil t)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-to-list 'auto-mode-alist '("\\.org_archive$" . org-mode))
  ;; Global org keys
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cg" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  (global-set-key "\C-cc" 'org-capture)
  ;; Secondary keystrokes for each:
  (global-set-key "\M-\C-l" 'org-store-link)
  (global-set-key "\M-\C-a" 'org-agenda)
  (global-set-key "\M-\C-b" 'org-iswitchb)
  (global-set-key "\M-\C-r" 'org-capture)
  ;; Where the org files live
  (setq org-directory "~/w/org")
  (setq org-agenda-files (list org-directory))
  (setq org-default-notes-file (concat org-directory "/refile.org"))
  (setq org-default-tips-file (concat org-directory "/tips.org"))
  ;; More org-mode variable settings, in lieu of customize.
  (setq org-startup-indented t)
  (setq org-M-RET-may-split-line
	'((default . nil) (table . t)))
  ;; Configure the capture templates
  (setq org-capture-templates
	(quote (("t" "todo" entry (file+headline org-default-notes-file "Tasks")
		 "* TODO %?\n%T\n%a\n")
		("p" "tips" entry (file+headline org-default-tips-file "Tips")
		 "* %?\n%^G\n%U\n%a\n")
		("n" "note" entry (file+headline org-default-notes-file "Notes")
		 "* %? :NOTE:\n%U\n%a\n")
		("j" "Journal" entry (file+datetree org-default-notes-file)
		 "* %?\nEntered on %U\n  %i\n  %a")
		)))
  ;; Task states and related configuration
  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
		(sequence "WAITING(w@/!)"))))
  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-todo-state-tags-triggers
	(quote (("WAITING" ("WAITING" . t))
		(done ("WAITING"))
		("TODO" ("WAITING"))
		("NEXT" ("WAITING"))
		("DONE" ("WAITING")))))
  ;; Tags
  (setq org-tag-alist (quote ((:startgroup)
			      ("@flume" . ?f)
			      ("@bigquery" . ?b)
			      ("@config" . ?C)
			      ("@life" . ?l)
			      (:endgroup)
			      ("googs" . ?g)
			      ("bq-cli" . ?c)
			      ("python" . ?P)
			      ("statsy" . ?s)
			      ("emacs" . ?E)
			      ("bash" . ?B)
			      ("tmux" . ?T)
			      ("git" . ?G)
			      )))
  ;; Allow setting single tags without the menu
  (setq org-fast-tag-selection-single-key (quote expert))
  ;; For tag searches ignore tasks with scheduled and deadline dates
  (setq org-agenda-tags-todo-honor-ignore-options t)
  
  )

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
(add-to-list 'auto-mode-alist '("config$" . conf-unix-mode))

;;---------------
;; scratch
;;---------------
(cc-add-to-load-path-if-exists "scratch-el")
(require 'scratch nil t)

;;-------------------
;; ReST and Markdown
;;-------------------
(when (require 'rst nil t)
  (add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))
  (add-to-list 'auto-mode-alist '("\\.rest$" . rst-mode))
  (cc-add-to-load-path-if-exists "markdown-mode")
  (autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.mdml$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode)))

;;------------------
;; deft-mode
;;------------------
(cc-add-to-load-path-if-exists "deft-mode")
(when (require 'deft nil t)
  (setq deft-extension "md")
  (setq deft-directory "~/w/deft")
  (setq deft-text-mode 'markdown-mode)
  (setq deft-use-filename-as-title t))

;;------------------
;; Makefiles
;;------------------
(add-to-list 'auto-mode-alist '("^Makefile$" . makefile-bsdmake-mode))

;;------------------
;; Haskell
;;------------------
;; Generic bit
(let ((haskell-site-file (cc-find-file-or-nil "haskell-mode/haskell-site-file")))
  (when haskell-site-file
    (load haskell-site-file)
    (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)))

;; ghc-mod:
;;   https://github.com/kazu-yamamoto/ghc-mod
(cc-add-to-load-path-if-exists "ghc-mod/elisp")
(let ((cabal-path (concat (getenv "HOME") "/cabal/bin")))
  (when (file-exists-p cabal-path)
    (add-to-list 'exec-path cabal-path)))
;; (autoload 'ghc-init "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

;; hoogle
(when (require 'haskell-mode nil t)
  (setq haskell-hoogle-command "hoogle")
  (define-key haskell-mode-map "\C-c?" 'haskell-hoogle))

;;---------------------------
;; Python, Cython
;;---------------------------
;; TODO(craigcitro): Merge this and the other python mode.
;; Currently, there are problems with this cython/python mode,
;; so just ignore them.
;; (add-to-list 'load-path (expand-file-name "/sage/data/emacs"))
(cc-add-to-load-path-if-exists "src/python-mode")
(when (require 'python-mode nil t)
  (setq python-indent 2))
;; (require 'pyrex "pyrex-mode")
;; (load (concat (getenv "HOME") "/.emacs.d/lisp/cython-mode.el"))
;; (load (concat (getenv "HOME") "/.emacs.d/lisp/python-mode.el"))
;; (add-to-list 'auto-mode-alist '("^SConstruct$" . python-mode))
;; (add-to-list 'auto-mode-alist '("\\.pxi$" . cython-mode))
;; (add-to-list 'auto-mode-alist '("\\.pxd$" . cython-mode))

;; Unison
(add-to-list 'auto-mode-alist '("\\.prf$" . python-mode))

;; Nick Alexander and I wrote this at SD12
(defun bs (name)
  "Browse the structure of a Python/Cython file."
  ;; (occur "^\( *def\\|class\\|cdef class\\).*:$"))
  (interactive "sFind name in hierarchy: ")
  (occur (format "^\\( *\\(cp\\|c\\)?def.*%s\\|class\\|cdef class\\).*:$" name)))

;;-------------------------
;; Java
;;-------------------------
;; I shouldn't need this ...
(unless (assoc "\\.java$" auto-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.java$" . java-mode)))

;;------------------------
;; Emacs Lisp
;;------------------------
(add-to-list 'auto-mode-alist '("\\.emacs$" . emacs-lisp-mode))
;; (2010 Dec 02) Now that I rearranged my git repos, this name comes
;; up differently ...
(add-to-list 'auto-mode-alist '("/emacs$" . emacs-lisp-mode))
;; This is useful, but it would be nice to make it smarter: the C-e is
;; hacky and inelegant. I'd love it to evaluate the sexp and advance
;; iff there's no blank line below.
(defun eval-sexp-and-advance (arg)
  "Eval sexp ending at the end of this line and continue to the
  next sexp. (This is basically a poor man's 'step' function in
  emacs lisp.)"
  (interactive "P")
  (end-of-line)
  (eval-last-sexp nil)
  (unless arg
    (forward-sexp)))
(global-set-key "\C-c\C-e" 'eval-sexp-and-advance)

;; tab completion in the Eval: prompt!
(define-key read-expression-map [(tab)] 'hippie-expand)
;; This second binding takes care of console clients.
(define-key read-expression-map [(control i)] 'hippie-expand)
;; I really want this keybinding to be "previous completion" ...
;;(define-key read-expression-map [(shift tab)] 'unexpand)

;;------------------------
;; Shell scripts
;;------------------------
(add-to-list 'auto-mode-alist '("bash[^/]*$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("rc$" . shell-script-mode))

;;----------------------------------------
;; Javascript and friends
;;----------------------------------------
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
;; More js2 customization in the custom blocks at the bottom.

;;------------------------
;; html
;;------------------------

;; Jump into the late 90s for tag names ... there has to be a better
;; way to do this.
(add-hook 'html-mode
  (lambda ()
    (assq-delete-all 'bold html-face-tag-alist)
    (add-to-list 'html-face-tag-alist '(bold . "strong"))
    (assq-delete-all 'italic html-face-tag-alist)
    (add-to-list 'html-face-tag-alist '(italic . "em"))
    (rassq-delete-all 'bold html-tag-face-alist)
    (add-to-list 'html-tag-face-alist '("strong" . bold))
    (rassq-delete-all 'italic html-tag-face-alist)
    (add-to-list 'html-tag-face-alist '("em" . italic))))
;; (2010 Sep 25) Does this even work? Investigate something smarter.

;;------------------------
;; ess-mode
;;------------------------
(cc-add-to-load-path-if-exists "ess-5.14/lisp")
(when (require 'ess-site nil t)
  (add-to-list 'auto-mode-alist '("\\.R$" . r-mode))
  (add-to-list 'auto-mode-alist '("\\.valclass$" . r-mode))
  ;; We want to force _ back to _; ess-toggle-underscore can
  ;; force it to "smart _", but not off. Ugh.
  (ess-toggle-underscore 1)
  (ess-toggle-underscore nil)
  ;; Indent 4 spaces on a continued line in parens
  (setq ess-arg-function-offset 4))

;;------------------------
;; julia mode
;;------------------------
(cc-add-to-load-path-if-exists "src/julia/contrib/")
(when (require 'julia-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.jl$" . julia-mode)))

;;------------------------
;; magit
;;------------------------
(when (require 'magit nil t)
  (global-set-key "\C-c\C-g" 'magit-status))

;;------------------------
;; Other stuff
;;------------------------

;; Honestly, I really prefer newline-and-indent in general. I'm going
;; to be brave and set it universally, as follows:
(define-key global-map (kbd "RET") 'newline-and-indent)
;; That may be too much; if so, something like this is a little more
;; moderate:
;;   (defun set-newline-and-indent ()
;;     (local-set-key (kbd "RET") 'newline-and-indent))
;;   (add-hook 'lisp-mode-hook 'set-newline-and-indent)
;;   (add-hook 'html-mode-hook 'set-newline-and-indent)

;; Better indenting! I think that in the long-term I'll probably end
;; up copying/evolving these functions, but for now, why not just use
;; the Python macros ...  In particular, I think I'll end up wanting
;; to switch indent-rigidly for indent-code-rigidly.
;; (require 'python)
;; (global-set-key "\C-c<" 'python-shift-left)
;; (global-set-key "\C-c>" 'python-shift-right)
;; (2012 May 05) Switch to the ones from python-mode ...
(global-set-key "\C-c<" 'py-shift-left)
(global-set-key "\C-c>" 'py-shift-right)

;; It probably makes sense to have a default major mode other than
;; Fundamental (which seems to be unexciting); I'm torn between
;; text-mode and markdown-mode.
(setq major-mode 'paragraph-indent-text-mode)

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

;; I often find myself wanting to *update* the values in an alist. I'm
;; sure there's a good way to do this, but I didn't find it: so I'm
;; going to write something that does it. I'm not doing anything
;; clever with the implementation. Note that this simply returns an
;; updated list; for most of my use-cases, I'll actually need to do
;; `(setq alist (update-alist alist key val))` ...
(defun update-alist (alist key &optional val)
  "Update alist to contain (key . val). If key is already a key
  in alist, we replace the existing entry. If val is nil and key
  is a list, instead call (update-alist alist k v) for every
  pair (k . v) in key (i.e. view key as a list of updates)."
  (if (null key)
      alist
    (if (and (null val)
	     (listp key))
	(update-alist (update-alist alist (caar key) (cdar key))
		      (cdr key))
      (cons (cons key val) (assq-delete-all key alist)))))

;;==============================================================================
;; MISC KEY SETTINGS
;; ==============================================================================
;; (2010 Sep 24) Did something break this? Why did I feel the need to re-bind?
(global-unset-key [(control t)])
(global-set-key [(control t)] 'fill-paragraph)

;; This second binding is pretty good -- I should use it more.
(global-set-key "\C-c\C-j" 'goto-line)
(global-set-key "\M-\C-g" 'goto-line)

(global-set-key "\C-c\C-z" 'shell)

;; because I don't like "key not found" messages?
(global-set-key "\C-x\C-g" 'keyboard-quit)

;; This is just smart.
(global-set-key "\M-s" 'isearch-forward-regexp)
(global-set-key "\M-r" 'isearch-backward-regexp)

;; it's nice to easily comment or uncomment a whole region
(global-set-key "\C-c#" 'comment-or-uncomment-region)
;; quickly sort
(global-set-key "\C-cs" 'sort-lines)
;; align a whole region (usually after copy-paste)
(global-set-key "\C-ca" 'indent-region)

;; Transpose!!
(global-set-key "\C-ct" 'transpose-chars)
(global-set-key "\C-c\C-t" 'transpose-words)

;; this is still experimentation -- but I think I prefer that the
;; default is copy, and you request cut. this will also match the
;; keystrokes in tmux, making it easier on my fingers.
;; (2010 Sep 24) Man, I totally prefer this.
(global-set-key "\C-w" 'kill-ring-save)
(global-set-key "\M-w" 'kill-region)

;; \M-: is harder to type than \C-:
(global-set-key [(control :)] 'eval-expression)

;; \C-backspace seems just as good as \M-backspace ...
(global-set-key [C-backspace] 'backward-kill-word)
(global-set-key [C-delete] 'backward-kill-word)

;; \C-x\C-o and \C-xo should be the same.
(global-unset-key [(control x) (control o)])
(global-set-key "\C-x\C-o" 'other-window)

;; I want for a better keystroke here:
(global-set-key "\C-xa" 'mark-whole-buffer)

;; The default binding only ever annoys me in my tmux world:
(global-set-key "\C-x3" 'split-window-vertically)

;; I miss vim's J command, and I always seem to mix up this and
;; "join-above" (i.e. delete-indentation). I'm going to switch them to
;; what I find most natural.
(defun join-below ()
  """Join the next line to the current line."""
  (interactive)
  (delete-indentation t))
(global-set-key "\M-^" 'join-below)
(global-set-key "\C-^" 'delete-indentation)

;; get that mouse scroll going:
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

;; It's silly to page up and page down when a page is a bajillion
;; rows. Instead, make them half a page, capped at 40.
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
;; I don't know that I need to do this more often than at program
;; start ...
(adjust-pageup-pagedown)
;; ... but that doesn't seem to hold, so let's just do it at
;; frame creation time.
(add-to-list 'after-make-frame-functions 'adjust-pageup-pagedown)

;; M-up and M-down seem like good pageup/pagedown keys
(global-set-key [M-up] 'down-one-bounded-page)
(global-set-key [M-down] 'up-one-bounded-page)

(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(global-set-key "\M-z" 'down-one)
(global-set-key "\C-z" 'up-one)
(global-set-key [C-S-up] 'down-one)
(global-set-key [C-S-down] 'up-one)

;; transient-mark-mode
;; turned off by default, but easy to turn on
(global-set-key "\C-xt" 'transient-mark-mode)
(setq transient-mark-mode nil)

;; (2011 Sep 05) Well this use of revert-buffer is a marked
;; improvement. Thanks to:
;;  http://www.stokebloke.com/wordpress/2008/04/17/emacs-refresh-f5-key/
(defun reload-buffer ()
  "Reload the current buffer with the current cursor position."
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t))
(global-set-key "\C-c\C-r" 'reload-buffer)

;; Maybe useful:
(global-set-key "\C-c!" 'toggle-read-only)

;; this is for programmable completion ... maybe one day I'll set that up?
;; (pcomplete-autolist t)

;;----------------------------------------
;; frame-related
;;----------------------------------------

;;; switch kill frame and exit emacs keystrokes
(global-set-key "\C-x\C-n" 'new-frame)
(defun crazy-exit-stuff ()
  (global-set-key "\C-x\C-c" 'delete-frame)
  (global-set-key "\C-x\C-y" 'save-buffers-kill-emacs))
(defun normal-exit-stuff ()
  (global-set-key "\C-x\C-c" 'save-buffers-kill-emacs)
  (global-set-key "\C-x\C-y" 'delete-frame))
(crazy-exit-stuff)

;;----------------------------------------
;; new movement-related stuff
;;----------------------------------------
;; (2011 Sep 05) I had some funny movement stuff here; it wasn't
;; awesome.
;;
;; Read about some nice window movement stuff on
;; Nathan's blog here:
;;  http://nex-3.com/posts/45-efficient-window-switching-in-emacs
(setq windmove-wrap-around t)

;;======================================================
;; M-x customize
;;======================================================
(custom-set-variables
 ;; org-mode
 '(org-startup-indented t)
 ;; js2-mode
 ;; All the cool kids use 2.
 '(js2-basic-offset 2)
 '(js2-cleanup-whitespace t)
 '(js2-enter-indents-newline t)
 '(js2-highlight-level 3)
 '(js2-indent-on-enter-key t)
 ;; js2-mode is smart enough to escape quotes when I insert
 ;; them in a string; in theory that's cool, but so far it's
 ;; been annoying more often than not.
 '(js2-mode-escape-quotes nil)
 ;; three cheers for spastic typing.
 '(js2-mode-indent-ignore-first-tab t))

(custom-set-faces
 ;; js2
 ;; Underlines don't seem to work well in my emacs setup.
 '(js2-magic-paren-face ((t (:foreground "blue"))))
 '(js2-warning-face ((((class color) (background dark)) (:foreground "pink")))))

;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; NO OTHER CODE BELOW THIS COMMAND
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
(run-hooks 'after-make-frame-functions)
