;;; private/ms-office/autoload.el -*- lexical-binding: t; -*-

;;; MS Teams URL parsing

;;;###autoload
(defun sme/teams-url-parse (url)
  "Parse a MS Teams URL and return an alist of components.
Extracts teamName, channelName, and createdTime from URL query params."
  (when (and url (string-match-p "teams\\.microsoft\\.com" url))
    (let ((params (url-parse-query-string
                   (car (last (split-string url "?"))))))
      `((url . ,url)
        (team . ,(sme/teams--clean-name (cadr (assoc "teamName" params))))
        (channel . ,(sme/teams--clean-name (cadr (assoc "channelName" params))))
        (timestamp . ,(sme/teams--format-timestamp (cadr (assoc "createdTime" params))))))))

;;;###autoload
(defun sme/teams--clean-name (name)
  "Clean up a Teams name by replacing hyphens/underscores with spaces."
  (when name
    (replace-regexp-in-string "[-_]+" " " (url-unhex-string name))))

;;;###autoload
(defun sme/teams--format-timestamp (ts)
  "Convert Teams timestamp (milliseconds since epoch) to readable date."
  (when ts
    (let ((time (seconds-to-time (/ (string-to-number ts) 1000.0))))
      (format-time-string "%b %d" time))))

;;;###autoload
(defun sme/teams-url-from-clipboard ()
  "Get Teams URL from clipboard, return it if valid."
  (let ((clip (current-kill 0 t)))
    (when (and clip (string-match-p "teams\\.microsoft\\.com" clip))
      clip)))

;;;###autoload
(defun sme/teams-url-build-title ()
  "Parse clipboard Teams URL and build a default title."
  (let* ((url (sme/teams-url-from-clipboard))
         (parsed (sme/teams-url-parse url)))
    (if parsed
        (format "Teams: %s / %s - %s: "
                (or (alist-get 'team parsed) "Team")
                (or (alist-get 'channel parsed) "Channel")
                (or (alist-get 'timestamp parsed) ""))
      "Teams: ")))

;;; SharePoint URL parsing

;;;###autoload
(defun sme/sharepoint-url-parse (url)
  "Parse a SharePoint URL and return an alist of components.
Extracts document type and owner from URL path."
  (when (and url (string-match-p "sharepoint\\.com" url))
    (let* ((doc-type (sme/sharepoint--parse-doc-type url))
           (owner (sme/sharepoint--parse-owner url)))
      `((url . ,url)
        (doc-type . ,doc-type)
        (owner . ,owner)))))

;;;###autoload
(defun sme/sharepoint--parse-doc-type (url)
  "Extract document type from SharePoint URL.
:w: = Word, :x: = Excel, :p: = PowerPoint, :o: = OneNote, etc."
  (cond
   ((string-match-p "/:w:/" url) "Word")
   ((string-match-p "/:x:/" url) "Excel")
   ((string-match-p "/:p:/" url) "PowerPoint")
   ((string-match-p "/:o:/" url) "OneNote")
   ((string-match-p "/:b:/" url) "PDF")
   ((string-match-p "/:f:/" url) "Folder")
   (t "Doc")))

;;;###autoload
(defun sme/sharepoint--parse-owner (url)
  "Extract owner username from SharePoint URL path."
  (when (string-match "/personal/\\([^/]+\\)/" url)
    (let ((raw-owner (match-string 1 url)))
      ;; Convert mparella_bethel_jw_org to mparella
      (car (split-string raw-owner "_")))))

;;;###autoload
(defun sme/sharepoint-url-from-clipboard ()
  "Get SharePoint URL from clipboard, return it if valid."
  (let ((clip (current-kill 0 t)))
    (when (and clip (string-match-p "sharepoint\\.com" clip))
      clip)))

;;;###autoload
(defun sme/sharepoint-url-build-title ()
  "Parse clipboard SharePoint URL and build a default title."
  (let* ((url (sme/sharepoint-url-from-clipboard))
         (parsed (sme/sharepoint-url-parse url)))
    (if parsed
        (format "SharePoint: %s (%s): "
                (or (alist-get 'doc-type parsed) "Doc")
                (or (alist-get 'owner parsed) "shared"))
      "SharePoint: ")))
