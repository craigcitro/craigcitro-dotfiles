;; Copyright (c) 2014 Microsoft Corporation. All rights reserved.
;; Released under Apache 2.0 license as described in the file LICENSE.
;;
;; Author: Soonho Kong
;;

(require 'cl-lib)

(defgroup lean nil
  "Lean Theorem Prover"
  :prefix "lean-"
  :group 'languages
  :link '(url-link :tag "Website" "http://leanprover.github.io")
  :link '(url-link :tag "Github"  "https://github.com/leanprover/lean"))

(defgroup lean-keybinding nil
  "Keybindings for lean-mode."
  :prefix "lean-"
  :group 'lean)

(defvar-local lean-default-executable-name
  (cl-case system-type
    ('windows-nt   "lean.exe")
    ('cygwin       "lean.exe")
    (t             "lean"))
  "Default executable name of Lean")

(defcustom lean-rootdir nil
  "Full pathname of lean root directory. It should be defined by user."
  :group 'lean
  :type 'string)

(defcustom lean-executable-name lean-default-executable-name
  "Name of lean executable"
  :group 'lean
  :type 'string)

(defcustom lean-memory-limit 4096
  "Memory limit for lean process in megabytes"
  :group 'lean
  :type 'number)

(defcustom lean-timeout-limit 100000
  "Deterministic timeout limit (it is approximately the maximum number of memory allocations in thousands)"
  :group 'lean
  :type 'number)

(defcustom lean-extra-arguments nil
  "Extra command-line arguments to the lean process"
  :group 'lean
  :type '(list string))

(defcustom lean-eldoc-use t
  "Use eldoc mode for lean."
  :group 'lean
  :type 'boolean)

(defcustom lean-eldoc-nay-retry-time 0.3
  "When eldoc-function had nay, try again after this amount of time."
  :group 'lean
  :type 'number)

(defcustom lean-delete-trailing-whitespace nil
  "Set this variable to true to automatically delete trailing
whitespace when a buffer is loaded from a file or when it is
written."
  :group 'lean
  :type 'boolean)

(defcustom lean-show-type-add-to-kill-ring nil
  "If it is non-nil, add the type information to the kill-ring so
that user can yank(paste) it later. By default, it's
false (nil)."
  :group 'lean
  :type 'boolean)

(defcustom lean-server-show-pending-tasks t
  "Highlights pending tasks in the current buffer."
  :group 'lean
  :type 'boolean)

(defcustom lean-server-check-mode 'visible-lines-and-above
  "What parts of the open files the Lean server should check"
  :group 'lean
  :type 'symbol
  :options '(nothing visible-lines visible-lines-and-above visible-files open-files))

(defcustom lean-keybinding-std-exe1 (kbd "C-c C-x")
  "Lean Keybinding for std-exe #1"
  :group 'lean-keybinding :type 'key-sequence)
(defcustom lean-keybinding-std-exe2 (kbd "C-c C-l")
  "Lean Keybinding for std-exe #2"
  :group 'lean-keybinding  :type 'key-sequence)
(defcustom lean-keybinding-show-key (kbd "C-c C-k")
  "Lean Keybinding for show-key"
  :group 'lean-keybinding  :type 'key-sequence)
(defcustom lean-keybinding-server-restart (kbd "C-c C-r")
  "Lean Keybinding for server-restart"
  :group 'lean-keybinding  :type 'key-sequence)
(defcustom lean-keybinding-server-switch-version (kbd "C-c C-s")
  "Lean Keybinding for lean-server-switch-version"
  :group 'lean-keybinding :type 'key-sequence)
(defcustom lean-keybinding-find-definition (kbd "M-.")
  "Lean Keybinding for find-definition"
  :group 'lean-keybinding  :type 'key-sequence)
(defcustom lean-keybinding-tab-indent (kbd "TAB")
  "Lean Keybinding for tab-indent"
  :group 'lean-keybinding  :type 'key-sequence)
(defcustom lean-keybinding-auto-complete (kbd "S-SPC")
  "Lean Keybinding for auto completion"
  :group 'lean-keybinding  :type 'key-sequence)
(defcustom lean-keybinding-hole (kbd "C-c SPC")
  "Lean Keybinding for hole manipulation"
  :group 'lean-keybinding  :type 'key-sequence)
(defcustom lean-keybinding-lean-toggle-show-goal (kbd "C-c C-g")
  "Lean Keybinding for show-goal-at-pos"
  :group 'lean-keybinding  :type 'key-sequence)
(defcustom lean-keybinding-lean-toggle-next-error (kbd "C-c C-n")
  "Lean Keybinding for lean-toggle-next-error"
  :group 'lean-keybinding  :type 'key-sequence)
(defcustom lean-keybinding-lean-message-boxes-toggle (kbd "C-c C-b")
  "Lean Keybinding for lean-message-boxes-toggle"
  :group 'lean-keybinding :type 'key-sequence)
(defcustom lean-keybinding-leanpkg-configure (kbd "C-c C-p C-c")
  "Lean Keybinding for lean-leanpkg-configure"
  :group 'lean-keybinding :type 'key-sequence)
(defcustom lean-keybinding-leanpkg-build (kbd "C-c C-p C-b")
  "Lean Keybinding for lean-leanpkg-build"
  :group 'lean-keybinding :type 'key-sequence)
(defcustom lean-keybinding-leanpkg-test (kbd "C-c C-p C-t")
  "Lean Keybinding for lean-leanpkg-test"
  :group 'lean-keybinding :type 'key-sequence)
(provide 'lean-settings)
