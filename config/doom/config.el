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
(setq doom-font (font-spec :family "Hack Nerd Font Mono" :size 18.0)
      doom-variable-pitch-font (font-spec :family "Hack Nerd Font Mono" :size 18.0))

;; TODO future ideas
;; https://github.com/psibi/justl.el

;; Appearance
(setq doom-theme 'doom-one)
(setq display-line-numbers-type 'relative)

;; Always start maximized on macOS
(if (featurep :system 'macos)
    (add-hook 'window-setup-hook 'toggle-frame-maximized t))

(use-package! exec-path-from-shell
  :when (featurep :system 'macos)
  :config
  (exec-path-from-shell-initialize))


(setq epg-pinentry-mode 'loopback)

(unless (featurep :system 'windows)
  (use-package! keychain-environment
    :config
    (keychain-refresh-environment)))

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
  (propertize
   (string-join
    '("@@@@@@@@  @@@@@@@@@@    @@@@@@    @@@@@@@   @@@@@@ "
      "@@@@@@@@  @@@@@@@@@@@  @@@@@@@@  @@@@@@@@  @@@@@@@ "
      "@@!       @@! @@! @@!  @@!  @@@  !@@       !@@     "
      "!@!       !@! !@! !@!  !@!  @!@  !@!       !@!     "
      "@!!!:!    @!! !!@ @!@  @!@!@!@!  !@!       !!@@!!  "
      "!!!!!:    !@!   ! !@!  !!!@!!!!  !!!        !!@!!! "
      "!!:       !!:     !!:  !!:  !!!  :!!            !:!"
      ":!:       :!:     :!:  :!:  !:!  :!:           !:! "
      ":: ::::   :::     ::   ::   :::   ::: :::  :::: :: "
      ": :: ::    :      :     :   : :   :: :: :  :: : :  ")
    "\n")
   'face '+dashboard-banner))

(setq +dashboard-ascii-banner-fn #'sme-dashboard-draw-ascii-banner-fn)

(set-formatter! 'alejandra '("alejandra" "--quiet") :modes '(nix-mode))
;; fixes doctor check: https://github.com/doomemacs/doomemacs/blob/master/modules/lang/nix/doctor.el#L9
(setq nix-nixfmt-bin "alejandra")

(after! aphelia
  (push '(alejandra . ("alejandra" "-")) aphelia-formatters)
  (setf (alist-get 'nix aphelia-mode-alist) 'alejandra))

;; mail setup
(setq +org-msg-accent-color "#3b82a0")

(after! mu4e
  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail
        mu4e-get-mail-command "mbsync proton"
        mu4e-update-interval 300)

  (setq mu4e-maildir-shortcuts
        '(("/proton/Inbox"   . ?i)
          ("/proton/Sent"    . ?s)
          ("/proton/Drafts"  . ?d)
          ("/proton/Archive" . ?a)
          ("/proton/Starred" . ?f)
          ("/proton/Trash"   . ?t)
          ("/proton/Spam"    . ?j)))

  (add-hook 'org-msg-edit-mode-hook #'org-tempo-setup)

  (map! :after org-msg
        :map org-msg-edit-mode-map
        :n "ZZ" #'org-msg-ctrl-c-ctrl-c
        :n "ZQ" #'org-msg-edit-kill-buffer))

;; TODO
;; https://github.com/eraschle/komorebi.el

;; WSL clipboard encoding fixes
(load! "+wsl-clipboard")

;; Windows-native fixes (no-ops under WSL/Linux)
(when (featurep :system 'windows)
  (load! "+windows"))

;; Non git tracked setttings
(load! "+local")


;; Herdr re-encodes semantic key events as CSI-u after a pane negotiates
;; Kitty Keyboard Protocol. Emacs's xterm decoder may already own the
;; ESC[1..9 prefixes, which prevents kkp.el from installing its handlers.
(after! kkp
  (defun sme/kkp-process-keys-from-input (prefix &optional _prompt)
    "Decode PREFIX while ignoring the input decoder prompt argument."
    (kkp--process-keys prefix))

  (defun sme/kkp-reinstall-input-decode-map-h ()
    "Install KKP handlers over conflicting xterm input prefixes in Herdr."
    (when (and (string= (getenv "TERM_PROGRAM") "herdr")
               (bound-and-true-p global-kkp-mode)
               (kkp--this-terminal-has-active-kkp-p))
      (dolist (prefix kkp--key-prefixes)
        (let ((key (kkp--csi-escape (string prefix))))
          (define-key input-decode-map key nil)
          (define-key input-decode-map key
            (apply-partially #'sme/kkp-process-keys-from-input prefix))))))

  (add-hook 'kkp-terminal-setup-complete-hook
            #'sme/kkp-reinstall-input-decode-map-h)

  ;; Doom's config can load before or after the initial TTY negotiation.
  (when (kkp--this-terminal-has-active-kkp-p)
    (sme/kkp-reinstall-input-decode-map-h))
  (run-with-timer 3 nil #'sme/kkp-reinstall-input-decode-map-h))


;; Doom's tty setup hook can run before this config loads. Install the cursor
;; updates locally because buffers with local pre/post-command hooks can shadow
;; the cursor changer's global hooks.
(after! evil-terminal-cursor-changer
  (defun sme/etcc-install-local-hooks-h ()
    "Update the terminal cursor for Evil state and command changes."
    (when (not (display-graphic-p))
      (dolist (hook '(pre-command-hook
                      post-command-hook
                      evil-normal-state-entry-hook
                      evil-insert-state-entry-hook
                      evil-visual-state-entry-hook
                      evil-replace-state-entry-hook
                      evil-emacs-state-entry-hook
                      evil-motion-state-entry-hook
                      evil-operator-state-entry-hook))
        (add-hook hook #'etcc--set-cursor nil t))
      (etcc--set-cursor)))

  (add-hook 'evil-local-mode-hook #'sme/etcc-install-local-hooks-h)
  (when (bound-and-true-p evil-local-mode)
    (sme/etcc-install-local-hooks-h)))

;; Replace Doom's compact modal indicator with a Neovim-style state segment.
;; Keeping the segment named `modals' preserves Doom's existing modeline layouts.
(after! doom-modeline
  (doom-modeline-def-segment modals
    "Display the current Evil state as a colored status segment."
    (when (bound-and-true-p evil-local-mode)
      (let* ((state evil-state)
             (label (cond ((eq state 'normal) "NORMAL")
                          ((eq state 'insert) "INSERT")
                          ((eq state 'visual) "VISUAL")
                          ((eq state 'motion) "MOTION")
                          ((eq state 'operator) "OPERATOR")
                          ((eq state 'replace) "REPLACE")
                          ((eq state 'emacs) "EMACS")
                          (t (upcase (symbol-name state)))))
             (background (cond ((eq state 'normal) "#1c3b4f")
                              ((eq state 'insert) "#30451f")
                              ((eq state 'visual) "#41244a")
                              ((eq state 'motion) "#1d4149")
                              ((eq state 'operator) "#4a321f")
                              ((eq state 'replace) "#4b2627")
                              ((eq state 'emacs) "#3a3f4b")
                              (t "#4a321f")))
             (foreground (cond ((eq state 'normal) "#8ec7ef")
                              ((eq state 'insert) "#b8e18a")
                              ((eq state 'visual) "#e3b4f0")
                              ((eq state 'motion) "#a9ebf5")
                              ((eq state 'operator) "#f1c08a")
                              ((eq state 'replace) "#ff9b9a")
                              ((eq state 'emacs) "#d5d8e0")
                              (t "#f1c08a"))))
        (propertize (format " %s " label)
                    'face (list :foreground foreground
                                :background background
                                :weight 'bold))))))
