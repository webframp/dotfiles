;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

(package! kubernetes)
(when (featurep! :editor evil +everywhere)
  (package! kubernetes-evil))

(package! emoji-cheat-sheet-plus)

(package! counsel-spotify)

(package! typopunct :recipe (:host github :repo "emacsmirror/typopunct" :files (:defaults "*")))

(package! edit-server)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
