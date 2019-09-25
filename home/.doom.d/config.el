;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; To modify or add binding for existing modules use add-hook! or after! macros
;; https://github.com/hlissner/doom-emacs/wiki/Customization#reconfigure-packages

;; HOTFIX for Emacs 26.1 and gnutls 3.6 - no longer needed?
;; https://www.reddit.com/r/emacs/comments/cdf48c/failed_to_download_gnu_archive/
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(when IS-LINUX
  (font-put doom-font :weight 'semi-light))
(when IS-MAC
  (setq ns-use-thin-smoothing t)
  (add-hook 'window-setup-hook #'toggle-frame-maximized))

;; To modify or add binding for existing modules use add-hook! or after! macros
;; https://github.com/hlissner/doom-emacs/wiki/Customization#reconfigure-packages

;; TODO add mapping for forge-toggle-closed-visibility.
;; Place with dispatch: SPC  g ' x ?, @ x ? (visual, normal states too)
;; NOTE how to customize dispatch menu?
(map! :leader
      :after projectile
      (:prefix "p"
        :desc "Save Project Buffers" "s" #'projectile-save-project-buffers))

(setq company-idle-delay 0.1)
;; (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))
(setq auth-sources '("~/.authinfo.gpg"))
;; (setq doom-font (font-spec :family "SauceCodePro Nerd Font" :size 18))
(setq doom-font (font-spec :family "FuraCode Nerd Font Mono" :size 18))

;; Bind to SPC f F - freshen file from disk
;; Source: https://www.emacswiki.org/emacs/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(map! :leader
      (:prefix "f"
        :desc "Refresh file from disk" "F" #'revert-buffer-no-confirm))

;; Aggressive whitespace cleanups
(defun sme-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun sme-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun sme-cleanup-buffer ()
  "Automatically cleanup a bunch of whitespace issues"
  (interactive)
  (sme-indent-buffer)
  (sme-untabify-buffer)
  (delete-trailing-whitespace))

;; TODO rebind org-archive-subtree to SPC m h a
;; bindings for org-move-subtree-up, down, etc
;; http://develop.spacemacs.org/layers/+emacs/org/README.html#trees
;; (map! :localleader
;;       :map org-mode-map
;;       :after org
;;       :desc "Archive subtree" "$" #'org-archive-subtree)

;; BUG: https://stackoverflow.com/questions/41741477/emacs-epa-and-gnupg2-no-usable-configuration
;; can't seem to find gpg
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))

;; Emoji Cheatsheet
;; https://github.com/syl20bnr/emacs-emoji-cheat-sheet-plus/
(use-package! emoji-cheat-sheet-plus
  :after (magit)
  :config
  (add-hook! 'magit-status-mode-hook 'emoji-cheat-sheet-plus-display-mode)
  (add-hook! 'magit-log-mode-hook 'emoji-cheat-sheet-plus-display-mode))

(map! :leader
      :after emoji-cheat-sheet-plus
      (:prefix "i"
        :desc "Insert emoji" "e" #'emoji-cheat-sheet-plus-insert))

;; Kubernetes.el
;; https://github.com/chrisbarrett/kubernetes-el
;; TODO setup keybindings
(use-package! kubernetes) ; :init config? what is :defer-incrementally?
(use-package! kubernetes-evil
  :when (featurep! :editor evil +everywhere)
  :after kubernetes)

(map! :leader
      :after kubernetes
      (:prefix "o"
        :desc "Kubernetes status" "k" #'kubernetes-overview))

;; Org mode
(setq org-ellipsis " ▼ ")
(after! org
  ;; FIXME https://emacs.stackexchange.com/questions/38800/match-data-clobbered-by-buffer-modification-hooks
  (add-hook! 'org-mode-hook 'emoji-cheat-sheet-plus-display-mode)
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
  :after (counsel)
  :config ;; FIXME move to private config
  (setq counsel-spotify-client-id "3816394d09994f8dbc5cd86d4ddf518d"
        counsel-spotify-client-secret "f5d3506b42744f8982e8dfe23e5d083b"))

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

;; Edit with emacs, supports editing textareas from FF in Emacs
;; https://github.com/stsquad/emacs_chrome
;; https://addons.mozilla.org/en-US/firefox/addon/edit-with-emacs1/
;; TODO add hook after edit-server-edit minor mode to turn on autofilll/spellcheck/evil bindings
;; NOTE how to customize based on buffer type? html email vs markdown vs text
(use-package! edit-server
  :config
  (edit-server-start))

;; Finally, keep my configs clean
;; (setq-default custom-file (concat doom-private-dir "+custom.el"))
;; (load! "+custom")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
