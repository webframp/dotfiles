;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(when IS-LINUX
  (font-put doom-font :weight 'semi-light))
(when IS-MAC
  (setq ns-use-thin-smoothing t)
  (add-hook 'window-setup-hook #'toggle-frame-maximized))

;; To modify or add binding for existing modules use add-hook! or after! macros
;; https://github.com/hlissner/doom-emacs/wiki/Customization#reconfigure-packages

;; TODO add mapping for forge-toggle-closed-visibility.
;; Place with dispatch: SPC  g ' x ?, @ x ? (visual, normal states too)
;; how to customize dispatch menu?
(map! :leader
      :after projectile
      (:prefix "p"
        :desc "Save Project Buffers" "s" #'projectile-save-project-buffers))

(setq company-idle-delay 0.1)
(setq doom-font (font-spec :family "SauceCodePro Nerd Font" :size 18))
(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg"))
;; (setq doom-font (font-spec :family "FuraCode Nerd Font Mono" :size 18))

;; Bind to SPC f F - freshen file from disk
;; Source: https://www.emacswiki.org/emacs/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

(map! :leader
      (:prefix "f"
        :desc "Refresh file from disk" "F" #'revert-buffer-no-confirm))

; TODO rebind org-archive-subtree to SPC m h a
; bindings for org-move-subtree-up, down, etc
; http://develop.spacemacs.org/layers/+emacs/org/README.html#trees
(map! :map org-mode-map
      :after org
      :localleader
      :desc "Archive subtree" "$" #'org-archive-subtree)

; bug: https://stackoverflow.com/questions/41741477/emacs-epa-and-gnupg2-no-usable-configuration
; can't seem to find gpg
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))

;; Emoji Cheatsheet
;; https://github.com/syl20bnr/emacs-emoji-cheat-sheet-plus/
(use-package! emoji-cheat-sheet-plus
  :after (magit org)
  :config
  ;; FIXME https://emacs.stackexchange.com/questions/38800/match-data-clobbered-by-buffer-modification-hooks
  ;; (add-hook! 'org-mode-hook 'emoji-cheat-sheet-plus-display-mode)
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
  (add-to-list 'org-modules 'org-habit t))

;; HOTFIX for Emacs 26.1 and gnutls 3.6 - no longer needed?
; https://www.reddit.com/r/emacs/comments/cdf48c/failed_to_download_gnu_archive/
;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Finally, keep my configs clean
;; (setq-default custom-file (concat doom-private-dir "+custom.el"))
;; (load! "+custom")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
