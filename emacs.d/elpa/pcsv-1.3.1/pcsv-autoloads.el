;;; pcsv-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (pcsv-file-parser pcsv-parser pcsv-parse-region
;;;;;;  pcsv-parse-file pcsv-parse-buffer) "pcsv" "pcsv.el" (20855
;;;;;;  34396))
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

(autoload 'pcsv-parser "pcsv" "\
Get a CSV parser to parse BUFFER.
This function supported only Emacs 24 or later.


Example:
\(setq parser (pcsv-parser))
\(let (tmp)
  (while (setq tmp (funcall parser))
    (print tmp)))

\(fn &optional BUFFER)" nil nil)

(autoload 'pcsv-file-parser "pcsv" "\
Create a csv parser to read huge FILE.
This csv parser accept a optional arg which non-nil means terminate the parser.

Optional arg BLOCK-SIZE indicate bytes to read FILE each time.

Example:
\(let ((parser (pcsv-file-parser \"/path/to/csv\")))
  (unwind-protect
      (let (tmp)
        (while (setq tmp (funcall parser))
          (print tmp)))
    ;; Must close the parser
    (funcall parser t)))

\(fn FILE &optional CODING-SYSTEM BLOCK-SIZE)" nil nil)

;;;***

;;;### (autoloads nil nil ("pcsv-pkg.el") (20855 34396 446067))

;;;***

(provide 'pcsv-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pcsv-autoloads.el ends here
