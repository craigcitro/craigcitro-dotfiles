;;; flymake-css-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "flymake-css" "flymake-css.el" (21770 119 0
;;;;;;  0))
;;; Generated autoloads from flymake-css.el

(defvar flymake-css-lint-command "csslint" "\
Name (and optionally full path) of csslint executable.")

(custom-autoload 'flymake-css-lint-command "flymake-css" t)

(autoload 'flymake-css-load "flymake-css" "\
Configure flymake mode to check the current buffer's css syntax.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; flymake-css-autoloads.el ends here
