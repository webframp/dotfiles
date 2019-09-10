;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

(package! kubernetes)
(when (featurep! :editor evil +everywhere)
  (package! kubernetes-evil))

(package! emoji-cheat-sheet-plus)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
