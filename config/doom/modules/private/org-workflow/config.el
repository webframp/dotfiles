;;; private/org-workflow/config.el -*- lexical-binding: t; -*-

;; Org-mode workflow configuration including:
;; - Basic org settings
;; - org-super-agenda custom views
;; - org-roam capture templates and project management
;; - org-download WSL integration

;; org setup
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

  (defun sme/org-roam-filter-by-tag (tag-name)
    "Return a filter function that checks if a node has TAG-NAME."
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))

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
     (sme/org-roam-filter-by-tag "o11n")
     :templates
     '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
        :if-new (file+head "projects/%<%Y%m%d%H%M%S>-o11n-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: project:o11n\n")
        :unnarrowed t))))

  ;; after org-roam ends here
  )

;; (map! :after org-roam
;;       :map doom-leader-notes-map
;;       :desc "Find o11n project"
;;       :n "nrx" #'sme/org-roam-find-project)


;; Only for WSL, needed for screenshots to work
(when (featurep :system 'wsl)
  ;; Only needed for WSL
  (after! org-download
    (setq org-download-screenshot-method
          ;; Absolute path: interop PATH is not appended under systemd mode.
          "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -Command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\"")))
