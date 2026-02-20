;;; private/wsl/config.el -*- lexical-binding: t; -*-

;; WSL-specific configuration
;; Note: org-download WSL screenshot config is in org-workflow module

(when (featurep :system 'wsl)
  ;; Open links in Windows default browser
  ;; TODO detect platform correctly (what does correct mean?)
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program cmd-exe
            browse-url-generic-args cmd-args
            browse-url-browser-function 'browse-url-generic
            search-web-default-browser 'browse-url-generic))))
