;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; To modify or add binding for existing modules use add-hook! or after! macros
;; https://github.com/hlissner/doom-emacs/wiki/Customization#reconfigure-packages

;;; UI
(when IS-LINUX
  (font-put doom-font :weight 'semi-light))
(when IS-MAC
  (setq ns-use-thin-smoothing t))

(setq doom-modeline-major-mode-icon t)

;;(setq doom-theme 'doom-outrun-electric)

;; (setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 18)
;;       doom-variable-pitch-font (font-spec :family "Noto Sans" :size 18))

(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 18)
      doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font" :size 18))

;;; Windows/Frames
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

(use-package! nyan-mode
  :hook
  (doom-modeline-mode . nyan-mode))

;; (map! :leader
;;       :after emoji-cheat-sheet-plus
;;       (:prefix "i"
;;         :desc "Insert emoji" "e" #'emoji-cheat-sheet-plus-insert))

;; Org mode
;; (setq org-ellipsis " ▼ "
;;       org-mobile-directory "~/Dropbox/Apps/MobileOrg"
;;       org-agenda-files '("~/org/todo.org"
;;                          "~/org/devou.org"
;;                          "~/org/devops.org"
;;                          ;; "~/org/sensuwork.org"))

(after! org
  (setq org-mobile-directory (concat
                              (file-name-as-directory (getenv "HOME"))
                              "Dropbox/Apps/MobileOrg")
        org-roam-directory "~/org/"
        org-ellipsis " ▼ "
        org-html-htmlize-output-type 'css)
  (add-hook! 'org-mode-hook ' emoji-cheat-sheet-plus-display-mode))

(after! org
    (setq org-startup-indented t)
    (setq org-indent-mode t)
    (setq org-startup-folded t))
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
