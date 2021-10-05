;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; To modify or add binding for existing modules use add-hook! or after! macros
;; https://github.com/hlissner/doom-emacs/wiki/Customization#reconfigure-packages

;;; UI
(when IS-LINUX
  (font-put doom-font :weight 'semi-light))
(when IS-MAC
  (setq ns-use-thin-smoothing t))

(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-bar-width 2)
;; (setq doom-theme 'doom-horizon)

;; (setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 18)
;;       doom-variable-pitch-font (font-spec :family "Noto Sans" :size 18))

;; If solair-mode is a problem
;;(remove-hook 'doom-load-theme-hook #'solaire-global-mode)

;; If confirm quit is a pain
;; (setq confirm-kill-emacs nil)

(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 18)
      doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font" :size 18))

;; Use a smaller font on mode-line
;; https://github.com/hlissner/doom-emacs/issues/2967#issuecomment-619319082
(custom-set-faces!
  '(mode-line :family "Iosevka Nerd Font" :height 0.9)
  '(mode-line-inactive :family "Iosevka Nerd Font" :height 0.9))

;;; Windows/Frames
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . yaml-mode))

;; Die, Doc-View-mode! die!
;(defalias 'doc-view-mode #'doc-view-fallback-mode)

;; Magit - if I want these back
;(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

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

;; TODO map ]S and [S to smerge-next and smerge-previous for navigating diffs

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

;;; Modules/Packages
;;
(after! emojify
  (add-hook! 'after-init-hook #'global-emojify-mode))

;; Emoji Cheatsheet
;; https://github.com/syl20bnr/emacs-emoji-cheat-sheet-plus/
;; (use-package! emoji-cheat-sheet-plus
;;   :after (magit)
;;   :config
;;     (add-hook! 'magit-status-mode-hook 'emoji-cheat-sheet-plus-display-mode)
;;     (add-hook! 'magit-log-mode-hook 'emoji-cheat-sheet-plus-display-mode)
;;     (add-hook! 'forge-topic-mode-hook 'emoji-cheat-sheet-plus-display-mode))

;; (use-package! company-emoji
;;   :defer t
;;   :config
;;   (after! company
;;     (set-company-backend! 'org-mode 'company-emoji)))

(use-package! nyan-mode
  :hook (doom-modeline-mode . nyan-mode))

(use-package! jsonnet-mode)

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

;; (map! :leader
;;       :after emoji-cheat-sheet-plus
;;       (:prefix "i"
;;         :desc "Insert emoji" "e" #'emoji-cheat-sheet-plus-insert))

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
  (add-hook! 'org-mode-hook ' emoji-cheat-sheet-plus-display-mode))

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

;; (map! :leader
;;       :after org
;;       (:prefix "m"
;;        :desc "MobileOrg push" "p" #'org-mobile-push
;;        :desc "MobileOrg pull" "u" #'org-mobile-pull))

(after! atomic-chrome
  (atomic-chrome-start-server))

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
