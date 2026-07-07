;;; private/git-workflow/config.el -*- lexical-binding: t; -*-

;; Git workflow customizations including:
;; - Forge keybindings for PR/MR workflow
;; - Magit GitLab push options

;; Forge keybindings
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
