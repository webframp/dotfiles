;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Sean Escriva"
      user-mail-address "sean.escriva@gmail.com"
      epa-file-encrypt-to user-mail-address

      company-idle-delay 0.1
      auth-sources '("~/.authinfo.gpg"))

(setq epg-gpg-args '("--pinentry-mode" "loopback"))
;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;

;; temp measure to use a good font, need to make a platform dependent decision
(setq doom-font (font-spec :family "Hack Nerd Font Mono" :size 16.0)
      doom-variable-pitch-font (font-spec :family "Hack Nerd Font Mono" :size 16.0))

;; TODO future ideas
;; https://github.com/psibi/justl.el

;; Appearance
(setq doom-theme 'doom-one)
(setq display-line-numbers-type 'relative)

;; Always start maximized on macOS
(if (featurep :system 'macos)
    (add-hook 'window-setup-hook 'toggle-frame-maximized t))


(setq epg-pinentry-mode 'loopback)

(use-package! keychain-environment
  :config
  (keychain-refresh-environment))

;; disable docker format on save
(setq-hook! 'dockerfile-mode-hook +format-inhibit t)

(add-hook 'go-mode-hook #'lsp-deferred)
;; make sure other goimports hooks aren't enabled
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(after! lsp-mode
  (setq lsp-go-use-gofumpt t
        lsp-go-analyses '((fieldalignment . t)
                          (nilness . t)
                          (shadow . t)
                          (unusedparams . t)
                          (unusedwrite . t)
                          (useany . t)
                          (unusedvariable . t))
        ;; nil LSP for Nix
        ;; uses same formatter as configured for aphelia
        ;; compatible with +onsave
        lsp-nix-nil-formatter ["alejandra" "--quiet"]
        lsp-nix-nil-auto-eval-inputs t
        lsp-nix-nil-max-mem 4096)
  (lsp-register-custom-settings
   '(("nil.nix.flake.autoArchive" t t))))

(after! csharp-mode
  (add-to-list 'auto-mode-alist '("\\.csx\\'" . csharp-mode)))

(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

;; Some generic mappings
(map! :leader
      :prefix ("o e" . "elfeed")
      "e" #'elfeed
      "u" #'elfeed-update
      "k" #'elfeed-unjam)

(map! :leader :prefix "o" :desc "Copy secret from pass"  "x" #'+pass/consult)

(map! :after org-tree-slide
      :map org-tree-slide-mode-map
      :n "C-x t l" #'org-tree-slide-move-next-tree
      :n "C-x t h" #'org-tree-slide-move-previous-tree)

;; disable some checkers
(after! flycheck
  (flycheck-define-checker terraform-tflint
    "A Terraform checker using tflint.
     See URL `https://github.com/terraform-linters/tflint'."
    :command ("tflint" "--format=json" "--force"
              (option-list "--var-file=" flycheck-tflint-variable-files concat))
    :error-parser flycheck-parse-tflint-linter
    :predicate flycheck-buffer-saved-p
    :modes terraform-mode)
  (add-to-list 'flycheck-disabled-checkers 'chef-foodcritic))

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

;; Custom ascii banner on startup
;; See: https://discourse.doomemacs.org/t/how-to-change-your-splash-screen/57
;; taken from: https://github.com/jsilve24/kisses
;; other ideas: https://github.com/emacs-dashboard/emacs-dashboard/
(defun sme-dashboard-draw-ascii-banner-fn ()
  (let* ((banner
          '("@@@@@@@@  @@@@@@@@@@    @@@@@@    @@@@@@@   @@@@@@ "
            "@@@@@@@@  @@@@@@@@@@@  @@@@@@@@  @@@@@@@@  @@@@@@@ "
            "@@!       @@! @@! @@!  @@!  @@@  !@@       !@@     "
            "!@!       !@! !@! !@!  !@!  @!@  !@!       !@!     "
            "@!!!:!    @!! !!@ @!@  @!@!@!@!  !@!       !!@@!!  "
            "!!!!!:    !@!   ! !@!  !!!@!!!!  !!!        !!@!!! "
            "!!:       !!:     !!:  !!:  !!!  :!!            !:!"
            ":!:       :!:     :!:  :!:  !:!  :!:           !:! "
            ":: ::::   :::     ::   ::   :::   ::: :::  :::: :: "
            ": :: ::    :      :     :   : :   :: :: :  :: : :  "))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'sme-dashboard-draw-ascii-banner-fn)

(set-formatter! 'alejandra '("alejandra" "--quiet") :modes '(nix-mode))
;; fixes doctor check: https://github.com/doomemacs/doomemacs/blob/master/modules/lang/nix/doctor.el#L9
(setq nix-nixfmt-bin "alejandra")

(after! aphelia
  (push '(alejandra . ("alejandra" "-")) aphelia-formatters)
  (setf (alist-get 'nix aphelia-mode-alist) 'alejandra))

;; TODO
;; https://github.com/eraschle/komorebi.el

;; Non git tracked setttings
(load! "+local")
