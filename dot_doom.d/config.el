;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; To modify or add binding for existing modules use add-hook! or after! macros
;; https://github.com/hlissner/doom-emacs/wiki/Customization#reconfigure-packages

;; https://github.com/d12frosted/homebrew-emacs-plus/issues/433#issuecomment-1025547880
;;(add-to-list 'default-frame-alist '(undecorated . t))

;;; UI/Appearance
(when IS-LINUX
  (font-put doom-font :weight 'semi-light))
(when IS-MAC
  (setq ns-use-thin-smoothing t))

(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-bar-width 2)

(setq doom-theme 'doom-old-hope)
(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 18)
      doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font" :size 18))

;; Use a smaller font on mode-line
;; https://github.com/hlissner/doom-emacs/issues/2967#issuecomment-619319082
(custom-set-faces!
  '(mode-line :family "Iosevka Nerd Font" :height 0.9)
  '(mode-line-inactive :family "Iosevka Nerd Font" :height 0.9))

;; (setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 18)
;;       doom-variable-pitch-font (font-spec :family "Noto Sans" :size 18))

;; If solair-mode is a problem
;;(remove-hook 'doom-load-theme-hook #'solaire-global-mode)

;; If confirm quit is a pain
;; (setq confirm-kill-emacs nil)

;;; Windows/Frames
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . yaml-mode))

;; Die, Doc-View-mode! die!
;(defalias 'doc-view-mode #'doc-view-fallback-mode)

;; Magit - if I want these back
;(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

;; disable some checkers
(after! flycheck
  (add-to-list 'flycheck-disabled-checkers 'chef-foodcritic))

;;; Keybinds
;; TODO Combine into single map! use
(map! :after forge
      :map forge-post-mode-map
      :n "ZZ" #'forge-post-submit
      :n "ZQ" #'forge-post-cancel)

(map! :after forge
      :map magit-status-mode-map
      :n "gi" #'forge-jump-to-issues
      :n "gp" #'forge-jump-to-pullreqs
      :n "gy" #'forge-copy-url-at-point-as-kill
      :n "go" #'magit-browse-thing)

;; TODO map ]S and [S to smerge-next and smerge-previous for navigating diffs
;; or see: https://github.com/emacs-evil/evil-collection/blob/master/modes/smerge/evil-collection-smerge-mode.el

;; TODO add mapping for forge-toggle-closed-visibility.
;; Place with dispatch: SPC  g ' x ?, @ x ? (visual, normal states too)
;; NOTE how to customize dispatch menu?

;; Source: https://www.emacswiki.org/emacs/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

;; Rebind SPC f F - freshen file from disk
;; replaces +default/find-file-under-here
;; TODO can this behavior be bound elsewhere?
(map! :leader
      (:prefix "f"
       :desc "Refresh file from disk" "F" #'revert-buffer-no-confirm))

;;; Modules/Packages
;;
(after! emojify
  ;; SPC i e to insert emoji and display automatically in buffers
  (add-hook! 'after-init-hook #'global-emojify-mode))

(use-package! nyan-mode
  :hook (doom-modeline-mode . nyan-mode))

(use-package! jsonnet-mode)

(setq +lookup-open-url-fn #'eww)
;; (setq +lookup-open-url-fn #'browse-url) ;; use default browser

;; TODO Go Development
;; download vsix from: https://github.com/golang/vscode-go/releases/
;; cd /Users/sme/.emacs.d/.local/etc/dap-extension/vscode/golang.go
;; unzip <vsix file>
;; needs extension folder under golang.go
;; dap-debug
;; In Golang test file? Select ’Go Launch File Configuration’ and add your breakpoints
;; ’Launch File’: buffer with program output
;; dap-disconnect > stop session
;; Dap frames:
;; locals: list with all variables in debug session
;; args: list function args
;; expressions: add expression to watch specific vars
;; SPC m d: hydra for dap-mode (useful to open specific DAP-frames)
;; Go dap-repl: call <fn>

(after! dap-mode
  ;;(setq dap-auto-configure-featuressessions (locals breakpoints expressions controls tooltip)))
  (setq dap-auto-configure-features ())
  (setq dap-ui-controls-mode nil))

;; Update dap golang plugin: (dap-go-setup 'FORCE)

;; bind to SPC TAB -
;; (+workspace/other)

;; https://github.com/fxbois/web-mode/blob/master/web-mode.el#L2118
;; (use-package! polymode
;;   :defer t
;;   :mode ("\.tmpl$" . poly-yaml-tmpl-mode)
;;   :config
;;   (setq polymode-prefix-key (kbd "C-c n")) ; some doom var to use here?
;;   (define-hostmode poly-yaml-hostmode :mode 'yaml-mode)
;;   (define-innermode poly-yaml-expr-tmpl-innermode
;;     :mode web-mode
;;     :head-matcher
;;     :tail-matcher
;;     :head-mode 'host
;;     :tail-mode 'host
;;     )
;;   )

;;; Tidal setup
;; ;; Haskell formatting
;; (after!
;;   (setq lsp-haskell-formatting-provider "brittany"))

;; ;; Tidal
;; (setq tidal-boot-script-path "~/.cabal/store/ghc-8.10.7/tdl-1.7.8-1a7352d6/share/BootTidal.hs")

;; Org mode
(after! org
  ;; (setq org-mobile-directory "~/Library/Mobile Documents/iCloud~com~mobileorg~mobileorg/Documents")
  (setq org-mobile-directory (concat
                              (file-name-as-directory (getenv "HOME"))
                              "Dropbox/Apps/MobileOrg")
        org-roam-directory "~/org/"
        org-ellipsis " ▼ "
        org-agenda-files '("~/org/todo.org"
                           "~/org/fsg.org"
                           "~/org/ios.org"
                           "~/org/notes.org"
                           "~/org/devou.org"
                           "~/org/devops.org")
        org-html-htmlize-output-type 'css)
  ;; Test with dired and M-x re-builder
  (add-to-list 'recentf-exclude "org_archive")

  ;; pdf+latex export
  ;; https://www.aidanscannell.com/post/org-mode-resume/
  ;;
  ;; Import ox-latex to get org-latex-classes and other funcitonality
  ;; for exporting to LaTeX from org
  (use-package! ox-latex
    :init
    ;; code here will run immediately
    :config
    ;; code here will run after the package is loaded
    (setq org-latex-pdf-process
          '("pdflatex -interaction nonstopmode -output-directory %o %f"
            "bibtex %b"
            "pdflatex -interaction nonstopmode -output-directory %o %f"
            "pdflatex -interaction nonstopmode -output-directory %o %f"))
    (setq org-latex-with-hyperref nil) ;; stop org adding hypersetup{author..} to latex export
    ;; (setq org-latex-prefer-user-labels t)

    ;; deleted unwanted file extensions after latexMK
    (setq org-latex-logfiles-extensions
          (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf" "acn" "acr" "alg" "glg" "gls" "ist")))

    (unless (boundp 'org-latex-classes)
      (setq org-latex-classes nil)))

  (use-package! ox-extra
    :config (ox-extras-activate '(latex-header-blocks ignore-headlines))))

;; TODO org-edit-src-exit not bound like org-edit-src-abort
(map! :after org
      :map org-mode-map
      :localleader
      (:prefix ("m" . "mobileorg")
       "p" #'org-mobile-push
       "P" #'org-mobile-pull))

;; TODO These should be defaults, why don't they work?
(after! org
  (setq org-startup-indented t)
  (setq org-indent-mode t)
  (setq org-startup-folded t))

(use-package! org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups '((:auto-dir-name t)))
  (org-super-agenda-mode))

(use-package! ob-mermaid
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(mermaid . t)))

;; (use-package! flycheck-vale
;;   :init (flycheck-vale-setup))

;; Display ansi color codes, only possible with emacs 28+
(after! text-mode
  (add-hook! 'text-mode-hook
             ;; Apply ANSI color codes
             (with-silent-modifications
               (ansi-color-apply-on-region (point-min) (point-max) t))))

;; Popup rules
;; https://github.com/hlissner/doom-emacs/tree/develop/modules/ui/popup
;; https://discord.com/channels/406534637242810369/603399769015975996/708468856233918534
(set-popup-rule! "^\\*doom:scratch"    :side 'bottom :size 0.42)
(set-popup-rule! "^\\*Flycheck errors" :side 'bottom :size 0.42)

;; Non git tracked local settings
(load! "+local")

;; Finally, keep my configs clean
(setq-default custom-file (concat doom-private-dir "+custom.el"))
(load! "+custom")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; byte-compile-warnings: (not free-vars)
;; End:
