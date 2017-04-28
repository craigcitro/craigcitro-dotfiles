;;; cc-org --- cc's org-mode config
;;; Commentary:
;;;   Sticking with a simple org config for now, mostly around keyboard
;;;   shortcuts for headings and lists.

;;; Code:

(message "Setting up org-mode ...")

;;------------------
;; org-mode
;;------------------

(defun cc/org-capture-readme ()
  "Capture a readme."
  (interactive)
  (org-capture nil "r"))

(defun cc/org-capture-todo ()
  "Capture a todo."
  (interactive)
  (org-capture nil "t"))

(defun cc/org-keys-hook ()
  "Hook for setting up org keystrokes."
  ;; Undefine C-c [ and C-c] , in favor of explicit management of the
  ;; org-agenda-files variable.
  (org-defkey org-mode-map "\C-c[" 'undefined)
  (org-defkey org-mode-map "\C-c]" 'undefined)
  (org-defkey org-mode-map "\C-c;" 'undefined)
  ;; I don't use priority, so simplify moving items.
  (org-defkey org-mode-map [M-right] 'org-shiftmetaright)
  (org-defkey org-mode-map [M-left] 'org-shiftmetaleft)
  (org-defkey org-mode-map [M-S-right] 'org-metaright)
  (org-defkey org-mode-map [M-S-left] 'org-tmetaleft)
  ;; I want some single-chord shortcuts for movement; this seems
  ;; like a good set, but we'll see.
  (org-defkey org-mode-map "\M-\C-f" 'outline-next-visible-heading)
  (org-defkey org-mode-map "\M-\C-b" 'outline-previous-visible-heading)
  (org-defkey org-mode-map "\M-\C-n" 'org-forward-heading-same-level)
  (org-defkey org-mode-map "\M-\C-p" 'org-backward-heading-same-level)
  (org-defkey org-mode-map "\M-\C-u" 'outline-up-heading)
  (org-defkey org-mode-map "\M-\C-k" 'org-cut-subtree)
  ;; I set these globally, but also want to override in org mode.
  (org-defkey org-mode-map "\M-\C-t" 'cc/org-capture-todo)
  )
;; We append to make sure the undefs above take effect.
(add-hook 'org-mode-hook 'cc/org-keys-hook 'append)

;; More org-mode variable settings, in lieu of customize.
(eval-after-load 'org
  '(progn
     ;; General config
     (setq org-startup-indented nil)
     (setq org-startup-folded 'content)
     (setq org-M-RET-may-split-line nil)
     (setq org-special-ctrl-a/e 'reversed)
     (setq org-special-ctrl-k t)
     (setq org-yank-adjusted-subtrees t)
     (setq org-id-method 'uuidgen)
     (setq org-directory "~/ww")
     (setq org-agenda-files (list org-directory))
     (setq org-default-notes-file (concat org-directory "/todo.org"))
     (setq org-archive-location "::* Done")
     (setq org-capture-templates
           (quote (("r" "readme" entry
                    (file+headline org-default-notes-file "Readme")
                    "* %?" :kill-buffer)
                   ("t" "todo" entry
                    (file+headline org-default-notes-file "Incoming")
                    "* %?" :kill-buffer)
                   )))
     ;; ido-based refile completion
     (setq org-refile-targets '((nil :maxlevel . 2)
                                (org-agenda-files :maxlevel . 2)))
     (setq org-refile-use-outline-path 'file)
     (setq org-outline-path-complete-in-steps nil)
     (setq org-refile-allow-creating-parent-nodes 'confirm)
     (setq org-completion-use-ido t)
     ;; Task states and related configuration
     (setq org-todo-keywords '((sequence "TODO" "LIVE" "DONE")))
     (setq org-use-fast-todo-selection t)
     (setq org-treat-S-cursor-todo-selection-as-state-change nil)
     (setq org-log-into-drawer t)
     (setq org-fast-tag-selection-single-key nil)
     ))

(when (require 'org-install nil t)
  ;; The only global keystrokes are for capture and links.
  (global-set-key "\C-cr" 'cc/org-capture-readme)
  (global-set-key "\M-\C-r" 'cc/org-capture-readme)
  (global-set-key "\C-ct" 'cc/org-capture-todo)
  (global-set-key "\M-\C-t" 'cc/org-capture-todo)
  (global-set-key "\C-cl" 'org-insert-link)
  (global-set-key "\M-\C-l" 'org-insert-link)
  )

(message "Setting up org-mode ... done.")
(provide 'cc-org)
;;; cc-org ends here
