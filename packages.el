;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; (package! copilot
;;   :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))

;; (package! gptel)

(package! jsonnet-mode)
(package! org-super-agenda)

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))

(package! copilot-chat
  :recipe (:host github :repo "chep/copilot-chat.el" :files ("*.el")))
