;;; private/ai-coach/config.el -*- lexical-binding: t; -*-

(use-package! ai-coach
  :commands (ai-coach-start
             ai-coach-stop
             ai-coach-reload
             ai-coach-dashboard
             ai-coach-anti-patterns)
  :config
  (setq ai-coach-project-dir
        (expand-file-name "~/src/webframp/emacs-ai-engineering-coach/"))
  (map! :map ai-coach-mode-map
        :n "q" #'ai-coach-quit
        :n "gr" #'ai-coach-refresh
        :n "gs" #'ai-coach-switch-view
        :n "7" #'ai-coach-set-days-7
        :n "1" #'ai-coach-set-days-14
        :n "3" #'ai-coach-set-days-30))

(map! :leader
      :prefix ("o l" . "llm")
      "p" #'ai-coach-dashboard
      "P" #'ai-coach-anti-patterns)
