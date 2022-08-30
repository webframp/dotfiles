;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

(package! emoji-cheat-sheet-plus)
;; (package! company-emoji)
    ;; :recipe (:host github :repo "dunn/company-emoji"))
(package! nyan-mode)

;; (package! elmine) ;; https://github.com/leoc/elmine/
(package! jsonnet-mode)

;; Example unpin: (unpin! lsp-mode) to ~/.doom.d/packages.el and run doom update
;; (package! counsel-spotify)

;; (package! typopunct :recipe (:host github :repo "emacsmirror/typopunct" :files (:defaults "*")))

;; (package! solaire-mode :disable t)

(package! polymode)
(package! org-super-agenda)
(package! ob-mermaid)
(package! mermaid-mode)

;; Sound synth with Haskell
;; http://tidalcycles.org/docs/getting-started/editor/Emacs/#using-doom-emacs-1
(package! tidal)

(package! flycheck-vale)

(package! go-dlv)
;; (package! elpher) ;; gopher client
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
