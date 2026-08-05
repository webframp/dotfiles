;;; $DOOMDIR/+windows.el -*- lexical-binding: t; -*-

;; Native Windows has no locale environment variables (LANG/LC_ALL), unlike
;; WSL/Linux, which leaves Emacs unable to infer a default coding system and
;; sometimes prompts interactively ("Select coding system") in batch contexts
;; like `doom upgrade`/`doom sync`. Setting LANG in the Windows environment
;; fixes the CLI prompt (see BOOTSTRAP.md); this covers the in-session
;; default so newly created buffers/files stay consistent too.
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
