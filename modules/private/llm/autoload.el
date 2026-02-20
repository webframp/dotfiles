;;; private/llm/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun sme/get-gemini-api-key ()
  "Retrieve Gemini API key from password store."
  (require 'auth-source-pass)
  (auth-source-pass-get 'secret "gemini-api-key"))
