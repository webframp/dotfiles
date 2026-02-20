;;; private/ms-office/config.el -*- lexical-binding: t; -*-

;; This module provides MS Teams and SharePoint URL parsing for org-mode links.
;; Functions are autoloaded from autoload.el and used by yasnippets:
;;   - mst: Teams link from clipboard
;;   - msp: SharePoint link from clipboard

;; Register module's snippets directory with yasnippet
(after! yasnippet
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" (file-name-directory load-file-name))))
