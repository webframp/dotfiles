;;; private/llm/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun sme/get-gemini-api-key ()
  "Retrieve Gemini API key from password store."
  (require 'auth-source-pass)
  (auth-source-pass-get 'secret "gemini-api-key"))

;;; tmux integration for CLI agents (claude, kiro)

(defvar sme/tmux-target nil
  "Target tmux pane as \"session:window.pane\" string.")

(defvar sme/tmux-paste-with-fence t
  "When non-nil, wrap region sends in a markdown code fence.")

;;;###autoload
(defun sme/tmux-set-target ()
  "Interactively select a tmux session and window as the send target."
  (interactive)
  (let* ((sessions (split-string
                    (shell-command-to-string
                     "tmux list-sessions -F '#{session_name}'")
                    "\n" t))
         (session (completing-read "tmux session: " sessions nil t))
         (windows (split-string
                   (shell-command-to-string
                    (format "tmux list-windows -t %s -F '#{window_index}:#{window_name}'"
                            (shell-quote-argument session)))
                   "\n" t))
         (window (completing-read "tmux window: " windows nil t))
         (window-index (car (split-string window ":"))))
    (setq sme/tmux-target (format "%s:%s" session window-index))
    (message "tmux target set to %s" sme/tmux-target)))

(defun sme/tmux--send (text)
  "Send TEXT to the current tmux target via load-buffer/paste-buffer."
  (unless sme/tmux-target
    (call-interactively #'sme/tmux-set-target))
  (let ((tmp (make-temp-file "emacs-tmux-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert text))
          (call-process "tmux" nil nil nil "load-buffer" tmp)
          (call-process "tmux" nil nil nil "paste-buffer"
                        "-t" sme/tmux-target "-p"))
      (delete-file tmp))))

(defun sme/tmux--lang-from-mode ()
  "Derive a markdown language hint from the current major mode."
  (let ((name (symbol-name major-mode)))
    (cond
     ((string-match-p "typescript" name) "typescript")
     ((string-match-p "js" name) "javascript")
     ((string-match-p "python" name) "python")
     ((string-match-p "rust" name) "rust")
     ((string-match-p "go-" name) "go")
     ((string-match-p "sh-mode\\|bash" name) "bash")
     ((string-match-p "ruby" name) "ruby")
     ((string-match-p "yaml" name) "yaml")
     ((string-match-p "json" name) "json")
     ((string-match-p "nix" name) "nix")
     ((string-match-p "hcl\\|terraform" name) "hcl")
     ((string-match-p "elisp\\|emacs-lisp" name) "elisp")
     ((string-match-p "org" name) "org")
     (t ""))))

;;;###autoload
(defun sme/tmux-send-context ()
  "Send current file path and region or line to the tmux target pane."
  (interactive)
  (let* ((file (buffer-file-name))
         (text (if (use-region-p)
                   (format "In %s lines %d-%d:\n\n%s"
                           file
                           (line-number-at-pos (region-beginning))
                           (line-number-at-pos (region-end))
                           (buffer-substring-no-properties
                            (region-beginning) (region-end)))
                 (format "In %s at line %d"
                         file
                         (line-number-at-pos)))))
    (sme/tmux--send text)
    (message "Sent context to %s" sme/tmux-target)))

;;;###autoload
(defun sme/tmux-send-region ()
  "Send the active region as a fenced code block to the tmux target pane."
  (interactive)
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((content (buffer-substring-no-properties
                   (region-beginning) (region-end)))
         (lang (if sme/tmux-paste-with-fence (sme/tmux--lang-from-mode) nil))
         (text (if sme/tmux-paste-with-fence
                   (format "```%s\n%s\n```" lang content)
                 content)))
    (sme/tmux--send text)
    (message "Sent region to %s" sme/tmux-target)))
