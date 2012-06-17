;;; pcsv-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (pcsv-parse-region pcsv-parse-file pcsv-parse-buffer)
;;;;;;  "pcsv" "pcsv.el" (20445 24052))
;;; Generated autoloads from pcsv.el

(autoload 'pcsv-parse-buffer "pcsv" "\
Parse a current buffer as a csv.
BUFFER non-nil means parse buffer instead of current buffer.

\(fn &optional BUFFER)" nil nil)

(autoload 'pcsv-parse-file "pcsv" "\
Parse FILE as a csv file.

\(fn FILE &optional CODING-SYSTEM)" nil nil)

(autoload 'pcsv-parse-region "pcsv" "\
Parse region as a csv.

\(fn START END)" nil nil)

;;;***

;;;### (autoloads nil nil ("pcsv-pkg.el") (20445 24052 210249))

;;;***

(provide 'pcsv-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pcsv-autoloads.el ends here
