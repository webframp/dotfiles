;;; $DOOMDIR/+local.el -*- lexical-binding: t; -*-

(after! forge
  (setq ghub-use-workaround-for-emacs-bug 'force)
  (add-to-list 'forge-alist '("git.bethelservice.org"
                              "git.bethelservice.org/api/v4"
                              "git.bethelservice.org"
                              forge-gitlab-repository)))

;; (after! browse-at-remote
;;   ;; Use branch name not commit hash
;;   (setq browse-at-remote-prefer-symbolic t)
;;   (dolist (elt '(("^git\\.bethelservice\\.org$" . "gitlab")))
;;     (add-to-list 'browse-at-remote-remote-type-regexps elt)))
