;; ABOUTME: Doom module for Go template + YAML syntax highlighting.
;; ABOUTME: Uses tree-sitter for gotmpl, regex for YAML (more robust with fragments).

;;; lang/gotmpl/config.el -*- lexical-binding: t; -*-

;;; Tree-sitter Grammar Source

(after! treesit
  (add-to-list 'treesit-language-source-alist
               '(gotmpl "https://github.com/ngalaiko/tree-sitter-go-template"
                        "master" "src")))

;;; YAML regex-based font-lock (works with fragmented content)

(defvar gotmpl-yaml-font-lock-keywords
  '(;; YAML comments
    ("^\\s-*#.*$" . font-lock-comment-face)
    ;; Document separators
    ("^---\\s-*$" . font-lock-preprocessor-face)
    ("^\\.\\.\\.$" . font-lock-preprocessor-face)
    ;; Keys (before colon, not inside {{ }})
    ("^\\s-*\\([a-zA-Z_][a-zA-Z0-9_.-]*\\)\\s-*:" 1 font-lock-keyword-face)
    ;; Boolean values
    (":\\s-*\\(true\\|false\\|yes\\|no\\|on\\|off\\)\\s-*$" 1 font-lock-constant-face)
    ;; Null values
    (":\\s-*\\(null\\|~\\)\\s-*$" 1 font-lock-constant-face)
    ;; Numbers
    (":\\s-*\\(-?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[eE][+-]?[0-9]+\\)?\\)\\s-*$" 1 font-lock-number-face)
    ;; Quoted strings
    ("\\(\"[^\"]*\"\\)" 1 font-lock-string-face)
    ("\\('[^']*'\\)" 1 font-lock-string-face)
    ;; Anchors and aliases
    ("\\(&[a-zA-Z0-9_-]+\\)" 1 font-lock-variable-name-face)
    ("\\(\\*[a-zA-Z0-9_-]+\\)" 1 font-lock-variable-use-face)
    ;; Tags
    ("\\(![a-zA-Z0-9_/-]+\\)" 1 font-lock-type-face))
  "Regex-based font-lock keywords for YAML.")

;;; Major Mode Definition

(define-derived-mode gotmpl-yaml-ts-mode prog-mode "YAML/GoTmpl"
  "Major mode for YAML files with Go template syntax.
Uses tree-sitter for Go templates, regex for YAML highlighting."
  :syntax-table nil

  (require 'treesit)

  (unless (treesit-ready-p 'gotmpl t)
    (user-error "Tree-sitter grammar for gotmpl not available. Run: M-x treesit-install-language-grammar RET gotmpl"))

  ;; Create parser for Go templates only
  (setq-local treesit-primary-parser (treesit-parser-create 'gotmpl))

  ;; Font-lock settings for Go templates
  (setq-local treesit-font-lock-settings
              (treesit-font-lock-rules
               :language 'gotmpl
               :feature 'comment
               '((comment) @font-lock-comment-face)

               :language 'gotmpl
               :feature 'keyword
               '((if_action "if" @font-lock-keyword-face)
                 (if_action "else" @font-lock-keyword-face)
                 (if_action "end" @font-lock-keyword-face)
                 (range_action "range" @font-lock-keyword-face)
                 (range_action "end" @font-lock-keyword-face)
                 (with_action "with" @font-lock-keyword-face)
                 (with_action "end" @font-lock-keyword-face)
                 (block_action "block" @font-lock-keyword-face)
                 (block_action "end" @font-lock-keyword-face)
                 (define_action "define" @font-lock-keyword-face)
                 (define_action "end" @font-lock-keyword-face)
                 (template_action "template" @font-lock-keyword-face))

               :language 'gotmpl
               :feature 'function
               '((function_call
                  function: (identifier) @font-lock-function-call-face))

               :language 'gotmpl
               :feature 'variable
               '(;; Field access: .name
                 (field name: (identifier) @font-lock-variable-use-face)
                 ;; Field in selector: $.foo or .bar.baz
                 (field_identifier) @font-lock-property-use-face
                 ;; Variable: $
                 (variable) @font-lock-variable-use-face
                 ;; Standalone dot
                 (dot) @font-lock-variable-use-face
                 ;; Variable definition: $Var :=
                 (variable_definition
                  variable: (variable) @font-lock-variable-name-face)
                 ;; Assignment operator
                 (variable_definition ":=" @font-lock-operator-face))

               :language 'gotmpl
               :feature 'operator
               '((chained_pipeline "|" @font-lock-operator-face))

               :language 'gotmpl
               :feature 'string
               '((interpreted_string_literal) @font-lock-string-face)))

  (setq-local treesit-font-lock-feature-list
              '((comment)
                (keyword string)
                (function variable)
                (operator)))

  (setq-local treesit-font-lock-level 4)

  ;; Initialize tree-sitter font-lock
  (treesit-major-mode-setup)

  ;; Add YAML regex highlighting on top of tree-sitter
  (font-lock-add-keywords nil gotmpl-yaml-font-lock-keywords 'append)

  ;; Indentation
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local standard-indent 2)
  (setq-local comment-start "# ")
  (setq-local comment-end ""))

;;; File Associations

(add-to-list 'auto-mode-alist '("\\.yaml\\.tmpl\\'" . gotmpl-yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\.tmpl\\'" . gotmpl-yaml-ts-mode))
