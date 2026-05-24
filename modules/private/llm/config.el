;;; private/llm/config.el -*- lexical-binding: t; -*-

;; LLM Customization and setup
;;
;; Mental model: "Thinking vs Doing"
;;
;; | Situation                    | Tool        | Binding     |
;; |------------------------------|-------------|-------------|
;; | "Refactor this function"     | Claude Code | SPC o l c   |
;; | "Add tests for X"            | Claude Code | SPC o l c   |
;; | "Fix this bug"               | Claude Code | SPC o l c   |
;; | "Why does this code do X?"   | gptel       | SPC o l d   |
;; | "Review this approach"       | gptel       | SPC o l d   |
;; | "Explain this error"         | gptel       | SPC o l d   |
;; | "Draft a commit message"     | gptel       | SPC o l d   |
;;
;; Rule: Expect file changes or commands → Claude Code
;;       Want a response to read → gptel

;; Predefine custom prompts directory
(setq gptel-prompts-directory (expand-file-name (concat doom-user-dir "prompts")))
;; Ensure the prompt directory exists
(unless (file-directory-p gptel-prompts-directory) (make-directory gptel-prompts-directory))

(after! gptel
  (setq gptel-model 'claude-4.5-sonnet
        gptel-backend (gptel-make-gh-copilot "Copilot"))
  (gptel-make-gemini "Gemini" :key #'sme/get-gemini-api-key :stream t))

(use-package! gptel-prompts
  :after gptel
  :demand t
  :config
  (gptel-prompts-update)
  ;; Ensure prompts are updated when files change
  (gptel-prompts-add-update-watchers))

;; Use simpler commit message style (Zed-style) instead of conventional commits
(after! gptel-magit
  (setq gptel-magit-commit-prompt gptel-magit-prompt-zed))

;; MCP Servers
;; Note: AWS CDK, documentation, and cost tools are provided by Claude Code CLI plugins
;; (aws-cdk@aws-skills, aws-cost-ops@aws-skills) - no need to duplicate here
(setq mcp-hub-servers
      '(;; https://github.com/modelcontextprotocol/servers/tree/main/src/fetch
        ("fetch" . (:command "uvx"
                    :args ("mcp-server-fetch")))
        ;; https://awslabs.github.io/mcp/servers/eks-mcp-server
        ("awslabs.eks-mcp-server" . (:command "uvx"
                                     :args ("awslabs.eks-mcp-server@latest"
                                            "--allow-sensitive-data-access")
                                     :env (:FASTMCP_LOG_LEVEL "ERROR")))
        ;; https://awslabs.github.io/mcp/servers/terraform-mcp-server
        ("awslabs.terraform-mcp-server" . (:command "uvx"
                                           :args ("awslabs.terraform-mcp-server@latest")
                                           :env (:FASTMCP_LOG_LEVEL "ERROR")))))

(use-package! mcp
  :after gptel
  :demand t
  :config
  (require 'mcp-hub))

(use-package! gptel-mcp
  :bind (:map doom-leader-map
              ("o l d" . gptel-mcp-dispatch)))

(map! :leader
      :prefix ("o l")
      "h" #'mcp-hub
      "t" #'sme/tmux-send-context
      "r" #'sme/tmux-send-region
      "T" #'sme/tmux-set-target)

(use-package! claude-code-ide
  ;; :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :bind (:map doom-leader-map
              ("o l c" . claude-code-ide-menu))
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

;; TODO investigate
;; https://github.com/JDNdeveloper/gptel-autocomplete
