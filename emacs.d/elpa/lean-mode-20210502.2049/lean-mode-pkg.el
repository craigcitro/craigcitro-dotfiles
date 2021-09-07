(define-package "lean-mode" "20210502.2049" "A major mode for the Lean 3 language"
  '((emacs "24.3")
    (dash "2.18.0")
    (s "1.10.0")
    (f "0.19.0")
    (flycheck "30"))
  :commit "bf32bb97930ed67c5cbe0fe3d4a69dedcf68be44" :authors
  '(("Leonardo de Moura" . "leonardo@microsoft.com")
    ("Soonho Kong      " . "soonhok@cs.cmu.edu")
    ("Gabriel Ebner    " . "gebner@gebner.org")
    ("Sebastian Ullrich" . "sebasti@nullri.ch"))
  :maintainer
  '("Sebastian Ullrich" . "sebasti@nullri.ch")
  :keywords
  '("languages")
  :url "https://github.com/leanprover/lean-mode")
;; Local Variables:
;; no-byte-compile: t
;; End:
