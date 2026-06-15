;;; $DOOMDIR/+wsl-clipboard.el -*- lexical-binding: t; -*-

;; Fix Windows-1252 (CP1252) bytes that appear when pasting from Windows
;; clipboard into Emacs under WSL. The C1 control range (U+0080-U+009F)
;; displays as \200-\237 in buffers. These are the CP1252 printable chars
;; that got decoded as their Unicode codepoint equivalents rather than
;; mapped to their intended glyphs.

;; Uses translate-region with a char-table rather than search-forward because
;; "\xNN" string literals are unibyte and silently fail to match in multibyte buffers.
(defvar sme/cp1252-char-table
  (let ((table (make-char-table 'translation-table)))
    (aset table #x80 #x20AC)  ; €
    (aset table #x82 #x201A)  ; ‚
    (aset table #x83 #x0192)  ; ƒ
    (aset table #x84 #x201E)  ; „
    (aset table #x85 #x2026)  ; …
    (aset table #x86 #x2020)  ; †
    (aset table #x87 #x2021)  ; ‡
    (aset table #x88 #x02C6)  ; ˆ
    (aset table #x89 #x2030)  ; ‰
    (aset table #x8A #x0160)  ; Š
    (aset table #x8B #x2039)  ; ‹
    (aset table #x8C #x0152)  ; Œ
    (aset table #x8E #x017D)  ; Ž
    (aset table #x91 #x2018)  ; '
    (aset table #x92 #x2019)  ; '
    (aset table #x93 #x201C)  ; "
    (aset table #x94 #x201D)  ; "
    (aset table #x95 #x2022)  ; •
    (aset table #x96 #x2013)  ; –
    (aset table #x97 #x2014)  ; —
    (aset table #x98 #x02DC)  ; ˜
    (aset table #x99 #x2122)  ; ™
    (aset table #x9A #x0161)  ; š
    (aset table #x9B #x203A)  ; ›
    (aset table #x9C #x0153)  ; œ
    (aset table #x9E #x017E)  ; ž
    (aset table #x9F #x0178)  ; Ÿ
    table)
  "Translation table mapping CP1252 C1 codepoints to proper Unicode.")

(defun sme/fix-cp1252-region (beg end)
  "Replace CP1252 C1 control characters in region with proper Unicode."
  (interactive "r")
  (translate-region beg end sme/cp1252-char-table))

(defun sme/fix-cp1252-after-yank (&rest _)
  "Clean CP1252 bytes from the just-yanked region."
  (let ((beg (region-beginning))
        (end (region-end)))
    (when (and beg end (< beg end))
      (sme/fix-cp1252-region beg end))))

(when (and (getenv "WSL_DISTRO_NAME")
           (not (string-empty-p (getenv "WSL_DISTRO_NAME"))))
  (advice-add 'yank :after #'sme/fix-cp1252-after-yank)
  (advice-add 'yank-pop :after #'sme/fix-cp1252-after-yank))
