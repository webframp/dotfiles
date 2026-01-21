;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

(package! jsonnet-mode)
(package! pkl-mode)
(package! org-super-agenda)

;; Smooth scrolling on macOS: https://github.com/jdtsmith/ultra-scroll
(when (featurep :system 'macos)
  (package! ultra-scroll
    :recipe (:host github :repo "jdtsmith/ultra-scroll" :files ("*.el"))))

;; https://github.com/jwiegley/gptel-prompts
(package! gptel-prompts
  :recipe (:host github :repo "jwiegley/gptel-prompts" :files ("*.el")))

(package! mcp)
(package! gptel-mcp
  :recipe (:host github :repo "lizqwerscott/gptel-mcp.el" :files ("*.el")))

;; TODO maybe, setup mu4e + offline imap for o365
;; https://github.com/UvA-FNWI/M365-IMAP
(package! claude-code-ide
  :recipe (:host github :repo "manzaltu/claude-code-ide.el" :files ("*.el")))
