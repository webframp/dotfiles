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

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 20 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 20))

;; TODO future ideas
;; https://github.com/psibi/justl.el

;; Appearance
(setq doom-theme 'doom-dracula)
(setq display-line-numbers-type 'relative)

(setq org-directory "~/org/")

(after! org
  (setq org-startup-indented t
        org-indent-mode t
        org-startup-folded t))

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
          ;; TODO look at how filetags work
          ;; https://d12frosted.io/posts/2020-06-25-task-management-with-roam-vol3.html
          ("c" "Add a new contact")
          ("cc" "Normal contact" plain
           "One date stamped heading for last contact %?" :target
           (file+head "pages/contacts/${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("co" "o11n contact" plain
           "\nOne date stamped heading for last contact\n %?" :target
           (file+head "pages/contacts/${slug}.org" "#+title: ${title}\n#+filetags: o11n\n")
           :unnarrowed t)
          ;; TODO add an agenda specific capture?
          ;; https://systemcrafters.net/build-a-second-brain-in-emacs/capturing-notes-efficiently/
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

  ;; Taken from https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
  ;; trying it mostly as default for now
  (defun sme/org-roam-o11n-finalize-hook ()
    "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
    ;; Remove the hook since it was added temporarily
    (remove-hook 'org-capture-after-finalize-hook #'sme/org-roam-o11n-finalize-hook)

    ;; Add o11n file to the agenda list if the capture was confirmed
    (unless org-note-abort
      (with-current-buffer (org-capture-get :buffer)
        (add-to-list 'org-agenda-files (buffer-file-name)))))

  (defun sme/org-roam-find-project ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'sme/org-roam-o11n-finalize-hook)

    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find
     nil
     nil
     (my/org-roam-filter-by-tag "o11n")
     :templates
     '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
        :if-new (file+head "projects/%<%Y%m%d%H%M%S>-o11n-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: project:o11n\n")
        :unnarrowed t)))
    )

  ;; after org-roam ends here
  )

;; (map! :after org-roam
;;       :map doom-leader-notes-map
;;       :desc "Find o11n project"
;;       :n "nrx" #'sme/org-roam-find-project)

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

(setq-hook! 'dockerfile-mode-hook +format-inhibit t)

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

(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

(map! :after forge
      :map forge-post-mode-map
      :n "ZZ" #'forge-post-submit
      :n "ZQ" #'forge-post-cancel)

(map! :after forge
      :map magit-status-mode-map
      :n "gp" #'forge-jump-to-pullreqs
      :n "gy" #'forge-copy-url-at-point-as-kill)
;; :n "go" #'forge-browse-dwim)

;; Add some push options to magit
;; https://docs.gitlab.com/ee/user/project/push_options.html
;; https://github.com/magit/magit/issues/3717
(after! magit
  (setq magit-repository-directories '(("~/src" . 2)))
  (transient-append-suffix 'magit-push "-u"
    '(1 "=s" "Skip Gitlab CI pipeline" "--push-option=ci.skip"))
  (transient-append-suffix 'magit-push "=s"
    '(1 "=D" "Delete branch when MR is merged" "--push-option=merge_request.remove_source_branch"))
  (transient-append-suffix 'magit-push "=D"
    '(1 "=A" "Auto assign MR to me" "--push-option=merge_request.assign='@sescriva'"))
  (transient-append-suffix 'magit-push "=A"
    '(1 "=C" "Auto create MR" "--push-option=merge_request.create")))

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
;; (use-package! copilot
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . 'copilot-accept-completion)
;;               ("TAB" . 'copilot-accept-completion)
;;               ("C-TAB" . 'copilot-accept-completion-by-word)
;;               ("C-<tab>" . 'copilot-accept-completion-by-word)))

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

;; Display ansi color codes, only possible with emacs 28+
(after! text-mode
  (add-hook! 'text-mode-hook
             ;; Apply ANSI color codes
             (with-silent-modifications
               (ansi-color-apply-on-region (point-min) (point-max) t))))

;; Copilot setup
;; https://github.com/copilot-emacs/copilot.el
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

;; Configuration settings for copilot-chat.el
;; https://github.com/chep/copilot-chat.el
;; TODO add doom compatible evil bindings
(use-package! copilot-chat
  :after copilot
  :config
  (setq copilot-chat-open-on-startup nil
        copilot-chat-window-width 80
        copilot-chat-window-height 20
        copilot-chat-window-position 'bottom
        copilot-chat-window-parameters '((minibuffer . nil)
                                         (no-other-window . t)
                                         (no-delete-other-windows . t))))

(defun +keychain-startup-hook ()
  "Load keychain env after emacs"
  (let* ((ssh (shell-command-to-string "keychain -q --noask --agents ssh --eval"))
         (gpg (shell-command-to-string "keychain -q --noask --agents gpg --eval")))
    (list (and ssh
               (string-match "SSH_AUTH_SOCK[=\s]\\([^\s;\n]*\\)" ssh)
               (setenv       "SSH_AUTH_SOCK" (match-string 1 ssh)))
          (and ssh
               (string-match "SSH_AGENT_PID[=\s]\\([0-9]*\\)?" ssh)
               (setenv       "SSH_AGENT_PID" (match-string 1 ssh)))
          (and gpg
               (string-match "GPG_AGENT_INFO[=\s]\\([^\s;\n]*\\)" gpg)
               (setenv       "GPG_AGENT_INFO" (match-string 1 gpg))))))

(add-hook 'after-init-hook #'+keychain-startup-hook)

;; Non git tracked setttings
(load! "+local")
