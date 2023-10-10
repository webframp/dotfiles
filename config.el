;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Sean Escriva"
      user-mail-address "sean.escriva@gmail.com"
      epa-file-encrypt-to user-mail-address

      company-idle-delay 0.1
      auth-sources '("~/.authinfo.gpg"))

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

;; (setq doom-font (font-spec :family "Inconsolata Nerd Font" :size 18 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "Inconsolata Nerd Font" :size 16))

;; Appearance
(setq doom-theme 'doom-tokyo-night)
(setq display-line-numbers-type 'relative)

(setq org-directory "~/org/")

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-start-day nil ;; i.e. today
        org-agenda-start-on-weekday nil)
  (setq org-agenda-custom-commands
        '(("c" "Super view"
           ((agenda "" ((org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:log t)
                            (:name "To refile"
                             :file-path "refile\\.org")
                            (:name "Next to do"
                             :todo "NEXT"
                             :order 1)
                            (:name "Important"
                             :priority "A"
                             :order 6)
                            (:name "Today's tasks"
                             :file-path  "journals/")
                            (:name "Due Today"
                             :deadline today
                             :order 2)
                            (:name "Scheduled Soon"
                             :deadline future
                             :order 8)
                            (:name "Overdue"
                             :deadline past
                             :order 7)
                            (:name "Meetings"
                             :and (:todo "MEET" :scheduled future)
                             :order 10)
                            (:discard (:not (:todo "TODO")))))))))))
  :config
  (org-super-agenda-mode))

;; Org Roam
(after! org-roam
  (setq org-roam-dailies-directory "journals/"
        rmh-elfeed-org-files '("~/org/roam/pages/news_feeds.org")
        org-roam-capture-templates
        '(("d" "default" plain
           "%?" :target
           (file+head "pages/${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ;; TODO add an agenda specific capture?
          ("p" "Project specific notes capture")
          ("pp" "o11n projects - primary" plain
           "%?" :target
           (file+head "projects/o11n-${slug}.org" "#+title: ${title}\n#+filetags: project:o11n\n")
           :unnarrowed t)
          ("pm" "MEPS projects" plain
           "%?" :target
           (file+head "projects/meps-${slug}.org" "#+title: ${title}\n#+filetags: project:meps\n")
           :unnarrowed t)
          ("pv" "AVS projects" plain
           "%?" :target
           (file+head "projects/avs-${slug}.org" "#+title: ${title}\n#+filetags: project:avs\n")
           :unnarrowed t)
          ("pa" "Apps projects" plain
           "%?" :target
           (file+head "projects/apps-${slug}.org" "#+title: ${title}\n#+filetags: project:apps\n")
           :unnarrowed t)))

  ;; https://github.com/org-roam/org-roam/wiki/User-contributed-Tricks#showing-node-hierarchy
  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    (let ((level (org-roam-node-level node)))
      (concat
       (when (> level 0) (concat (org-roam-node-file-title node) " > "))
       (when (> level 1) (concat (string-join (org-roam-node-olp node) " > ") " > "))
       (org-roam-node-title node))))
  ;; (setq org-roam-node-display-template "${hierarchy:*} ${tags:20}")
  )

;; NOTE fix for "cannot find entry with id", eval this line or use M-:
;; from: https://github.com/org-roam/org-roam/issues/1702
;; (org-id-update-id-locations (directory-files-recursively org-roam-directory ".org$\\|.org.gpg$"))

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

(add-hook 'go-mode-hook #'lsp-deferred)
;; make sure other goimports hooks aren't enabled
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(after! lsp-mode
  (setq lsp-go-use-gofumpt t
        lsp-go-analyses '((fielalignment . t)
                          (nilness . t)
                          (shadow . t)
                          (unusedparams . t)
                          (unusedwrite . t)
                          (useany . t)
                          (unusedvariable . t))))

(after! csharp-mode
  (add-to-list 'auto-mode-alist '("\\.csx\\'" . csharp-mode)))

(map! :after forge
      :map forge-post-mode-map
      :n "ZZ" #'forge-post-submit
      :n "ZQ" #'forge-post-cancel)

(map! :after forge
      :map magit-status-mode-map
      :n "gp" #'forge-jump-to-pullreqs
      :n "gy" #'forge-copy-url-at-point-as-kill
      :n "go" #'forge-browse-dwim)

;; disable some checkers
(after! flycheck
  (add-to-list 'flycheck-disabled-checkers 'chef-foodcritic))

;; WSL Specific
(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))

  ;; open links in default browser
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program cmd-exe
            browse-url-generic-args cmd-args
            browse-url-browser-function 'browse-url-generic
            search-web-default-browser 'browse-url-generic))))

;; Display ansi color codes, only possible with emacs 28+
(after! text-mode
  (add-hook! 'text-mode-hook
             ;; Apply ANSI color codes
             (with-silent-modifications
               (ansi-color-apply-on-region (point-min) (point-max) t))))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

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

;; TODO figure out why this doesn't work like it's supposed to
;; (after! nix
;;   (set-formatter! 'alejandra "alejandra --quiet" :modes '(nix-mode)))

;; Non git tracked setttings
(load! "+local")
