;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; To modify or add binding for existing modules use add-hook! or after! macros
;; https://github.com/hlissner/doom-emacs/wiki/Customization#reconfigure-packages

;; HOTFIX for Emacs 26.1 and gnutls 3.6 - no longer needed?
;; https://www.reddit.com/r/emacs/comments/cdf48c/failed_to_download_gnu_archive/
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;;; UI
(when IS-LINUX
  (font-put doom-font :weight 'semi-light))
(when IS-MAC
  (setq ns-use-thin-smoothing t))

(setq doom-modeline-major-mode-icon t)

;; (setq doom-theme 'doom-outrun-electric)
(setq doom-theme 'doom-laserwave)

;; (setq doom-font (font-spec :family "SauceCodePro Nerd Font" :size 18))
(setq doom-font (font-spec :family "FuraCode Nerd Font Mono" :size 18)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 18))

;;; Windows/Frames
;;;
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(when IS-MAC
  (add-hook 'window-setup-hook #'toggle-frame-maximized))

;;; Keybinds
;; TODO Combine into single map! use
(map! :after forge
      :map forge-post-mode-map
      :n "ZZ" #'forge-post-submit
      :n "ZQ" #'forge-post-cancel)

(map! :after forge
      :map forge-topic-mode-map
      "c" #'forge-create-post
      :n "gy" #'forge-copy-url-at-point-as-kill
      :n "go" #'forge-browse-dwim)

(map! :after forge
      :map magit-status-mode-map
      :n "gi" #'forge-jump-to-issues
      :n "gp" #'forge-jump-to-pullreqs
      :n "gy" #'forge-copy-url-at-point-as-kill
      :n "go" #'magit-browse-thing)


;; TODO add mapping for forge-toggle-closed-visibility.
;; Place with dispatch: SPC  g ' x ?, @ x ? (visual, normal states too)
;; NOTE how to customize dispatch menu?
(map! :leader
      :after projectile
      (:prefix "p"
        :desc "Save Project Buffers" "s" #'projectile-save-project-buffers))

;; Bind to SPC f F - freshen file from disk
;; Source: https://www.emacswiki.org/emacs/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(map! :leader
      (:prefix "f"
        :desc "Refresh file from disk" "F" #'revert-buffer-no-confirm))

;; TODO rebind org-archive-subtree to SPC m h a ?
;; bindings for org-move-subtree-up, down, etc ?
;; http://develop.spacemacs.org/layers/+emacs/org/README.html#trees
;; (map! :localleader
;;       :map org-mode-map
;;       :after org
;;       :desc "Archive subtree" "$" #'org-archive-subtree)


;;; Modules/Packages
;;
;; Emoji Cheatsheet
;; https://github.com/syl20bnr/emacs-emoji-cheat-sheet-plus/
(use-package! emoji-cheat-sheet-plus
  :after (magit)
  :config
    (add-hook! 'magit-status-mode-hook 'emoji-cheat-sheet-plus-display-mode)
    (add-hook! 'magit-log-mode-hook 'emoji-cheat-sheet-plus-display-mode)
    (add-hook! 'forge-topic-mode-hook 'emoji-cheat-sheet-plus-display-mode))

;; (map! :leader
;;       :after emoji-cheat-sheet-plus
;;       (:prefix "i"
;;         :desc "Insert emoji" "e" #'emoji-cheat-sheet-plus-insert))

;; lsp-mode
;; control how much data emacs can read on one pass from the server, requires HEAD
;; https://github.com/emacs-mirror/emacs/commit/cc78faee7d23dd0433ba537818a68cbd20fa52a3
(if (featurep! +lsp)
    (setq read-process-output-max (* 1024 1024)))

;; Kubernetes.el
;; https://github.com/chrisbarrett/kubernetes-el
;; TODO Fix kubeconfig handling with iam-authenticator
;; :init config? is :defer-incrementally useful here?
(use-package! kubernetes
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(use-package! kubernetes-evil
  :when (featurep! :editor evil +everywhere)
  :after kubernetes)

;; TODO any more keybindings ?
(map! :leader
      :after kubernetes
      (:prefix "o"
        :desc "Kubernetes status" "k" #'kubernetes-overview))

;; Org mode
;; (setq org-ellipsis " ▼ "
;;       org-mobile-directory "~/Dropbox/Apps/MobileOrg"
;;       org-agenda-files '("~/org/todo.org"
;;                          "~/org/devou.org"
;;                          "~/org/devops.org"
;;                          ;; "~/org/sensuwork.org"))

(after! org
  ;; FIXME https://emacs.stackexchange.com/questions/38800/match-data-clobbered-by-buffer-modification-hooks
  ;; (add-hook! 'org-mode-hook 'emoji-cheat-sheet-plus-display-mode)
  (add-to-list 'org-modules 'org-habit t))

;; Cloudformation mode
;; https://github.com/awkspace/doom-emacs-config/blob/dfd89cdff6204899fc49c5a2df88e71f427df0b5/config.el#L96
(define-derived-mode cfn-mode yaml-mode
  "Cloudformation"
  "Cloudformation template mode.")

(add-to-list 'auto-mode-alist '(".template\\'" . cfn-mode))
(after! flycheck
  (flycheck-define-checker cfn-lint
    "A Cloudformation linter using cfn-python-lint.
See URL 'https://github.com/awslabs/cfn-python-lint'."
    :command ("cfn-lint" "-f" "parseable" source)
    :error-patterns ((warning line-start (file-name) ":" line ":" column
                              ":" (one-or-more digit) ":" (one-or-more digit) ":"
                              (id "W" (one-or-more digit)) ":" (message) line-end)
                     (error line-start (file-name) ":" line ":" column
                            ":" (one-or-more digit) ":" (one-or-more digit) ":"
                            (id "E" (one-or-more digit)) ":" (message) line-end))
    :modes (cfn-mode))
  (add-to-list 'flycheck-checkers 'cfn-lint))

;; Spotify controls
(use-package! counsel-spotify
  :after (counsel))

(map! :leader
      (:prefix ("o" . "open")
        (:prefix ("s" . "Spotify control")
          :desc "play/pause" "SPC" #'counsel-spotify-toggle-play-pause
          :desc "next"       "l" #'counsel-spotify-next
          :desc "previous"   "h" #'counsel-spotify-previous)))

;; Typopunct - insert proper typographical punctuation when writing
;; https://github.com/emacsmirror/typopunct
(use-package! typopunct)
(map! :desc "Typopunct" :n "SPC t t" #'typopunct-mode)

(defun sme-markdown-mode-config ()
  "Enforce customized settings for markdown mode and enable typopunct"
  (set-fill-column 80)
  (typopunct-change-language 'english t)
  (typopunct-mode 1)
  ;; (setq-local comment-use-syntax nil) ;; usefulness is questionable
  )

(add-hook! 'markdown-mode-hook 'sme-markdown-mode-config)
(add-hook! 'gfm-mode-hook 'sme-markdown-mode-config)

;;; Edit Server
;; Edit with emacs, supports editing textareas from FF in Emacs
;; https://github.com/stsquad/emacs_chrome
;; https://addons.mozilla.org/en-US/firefox/addon/edit-with-emacs1/
;; TODO add hook after edit-server-edit minor mode to turn on autofilll/spellcheck/evil bindings
;; NOTE customize based on buffer type? html email vs markdown vs text
(use-package! edit-server
  :config
  (edit-server-start))

;; (map! :map edit-server-edit-mode-map
;;       "ZZ" #'edit-server-done
;;       "ZQ" #'edit-server-abort)

(pushnew! auto-mode-alist
          '("\\.tmpl\\'" . web-mode))

(setq web-mode-engines-alist
      '(("go" . "\\.tmpl\\'")))

;; Non git tracked local settings
(load! "+local")

;; Finally, keep my configs clean
(setq-default custom-file (concat doom-private-dir "+custom.el"))
(load! "+custom")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; byte-compile-warnings: (not free-vars)
;; End:
