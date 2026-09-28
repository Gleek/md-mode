;;; md-render-inline.el --- Inline Markdown rendering -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie

;; Author: yibie <https://github.com/yibie>
;; URL: https://github.com/yibie/md-mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Passes that rewrite inline Markdown: emphasis, headings, code spans and
;; links.  Each pass deletes the markup, keeps the inner text with its own
;; properties, adds a face, and records the original Markdown on
;; `md-render-source' so the text can be reconstructed.

;;; Code:

(require 'cl-lib)
(require 'md-render-core)

;;;; Replacing markup

(defun md-render--replace-markup (start end text)
  "Replace START to END with TEXT and return the end of the new text.
The original Markdown is stored on the new text unless the character at
START already carried a stored source."
  (let ((source (unless (get-text-property start 'md-render-source)
                  (md-render-reconstruct start end))))
    (goto-char start)
    (delete-region start end)
    (insert text)
    (when source
      (put-text-property start (point) 'md-render-source source))
    (point)))

(defun md-render--rewrite-matches (regexp group avoid-ranges function)
  "Rewrite every unprotected match of REGEXP in the buffer.
GROUP is the regexp group that spans the markup.  Matches wholly inside
AVOID-RANGES are skipped.  FUNCTION is called with point after the match
and the match data set; it rewrites the markup and returns the position
to continue from, or nil to continue after the markup.  Return non-nil
when FUNCTION rewrote anything."
  (let ((case-fold-search nil)
        (changed nil))
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let* ((start (match-beginning group))
             (end (match-end group))
             (range (md-render-in-avoid-range-p start end avoid-ranges)))
        (if range
            (goto-char (max end (cdr range)))
          (let ((next (funcall function)))
            (when next
              (setq changed t))
            (goto-char (or next end))))))
    changed))

;;;; Emphasis

(defconst md-render--emphasis-rules
  '((italic
     ("\\(?:^\\|[\n \t]\\)\\(\\*\\([^\n*]+\\)\\*\\)" . md-render-italic)
     ("\\(?:^\\|[\n \t]\\)\\(_\\([^\n_]+\\)_\\)\\(?:\\s.\\|\\s-\\|$\\)"
      . md-render-italic))
    (bold
     ("\\(?:^\\|\\s-\\)\\(\\*\\*\\([^\n*]+\\)\\*\\*\\)\\(?:\\s.\\|\\s-\\|$\\)"
      . md-render-bold)
     ("\\(?:^\\|\\s-\\)\\(__\\([^\n_]+\\)__\\)\\(?:\\s.\\|\\s-\\|$\\)"
      . md-render-bold))
    (strike
     ("\\(~~\\([^\n~]+\\)~~\\)" . md-render-strikethrough)))
  "Emphasis passes in the order one round runs them.
Each pass lists (REGEXP . FACE) rules.  Group 1 spans the markup and
group 2 the emphasized text.")

(defun md-render--emphasize (regexp face avoid-ranges)
  "Replace each REGEXP match with its inner text in FACE.
Matches inside AVOID-RANGES are skipped.  Return non-nil on change."
  (md-render--rewrite-matches
   regexp 1 avoid-ranges
   (lambda ()
     (let* ((start (match-beginning 1))
            (inner (match-string 2))
            (end (md-render--replace-markup start (match-end 1) inner)))
       (add-face-text-property start end face)
       end))))

(defun md-render--style-emphasis (avoid-ranges)
  "Render emphasis until a full round changes nothing.
Matches inside AVOID-RANGES are skipped.  Each round runs the italic,
bold and strikethrough passes in turn, so nested delimiters peel off
one layer per round."
  (let ((changed t))
    (while changed
      (setq changed nil)
      (pcase-dolist (`(,_pass . ,rules) md-render--emphasis-rules)
        (pcase-dolist (`(,regexp . ,face) rules)
          (when (md-render--emphasize regexp face avoid-ranges)
            (setq changed t)))))))

;;;; Headings

(defun md-render--header-face (level)
  "Return the heading face for LEVEL."
  (intern (format "md-render-header-%d" level)))

(defun md-render--mark-heading (start end level)
  "Give the heading title from START to END its LEVEL face and context."
  (let ((face (md-render--header-face level)))
    (add-face-text-property start end face)
    (put-text-property start end 'md-render-line-context
                       (list :kind 'heading :level level :face face))))

(defun md-render--style-setext-headings (avoid-ranges)
  "Render underlined headings outside AVOID-RANGES."
  (md-render--rewrite-matches
   "^\\([ \t]*[^ \t\n][^\n]*\\)\n[ \t]*\\(=+\\|-+\\)[ \t]*\\(\n\\)" 0
   avoid-ranges
   (lambda ()
     (let* ((start (match-beginning 0))
            (title-end (match-end 1))
            (newline (match-beginning 3))
            (level (if (eq (char-after (match-beginning 2)) ?=) 1 2)))
       (unless (save-excursion
                 (goto-char start)
                 (looking-at-p "[ \t]*#+\\(?:[ \t]\\|$\\)"))
         (let ((title (buffer-substring start title-end))
               (source (md-render-reconstruct start newline))
               (newline-properties (md-render--carry-properties newline)))
           (goto-char start)
           (delete-region start (1+ newline))
           (insert title)
           (let ((end (point)))
             (insert (apply #'propertize "\n" newline-properties))
             (md-render--mark-heading start end level)
             (put-text-property start end 'md-render-source source)
             (point))))))))

(defun md-render--style-atx-headings (avoid-ranges)
  "Render `#' headings outside AVOID-RANGES."
  (md-render--rewrite-matches
   "^[ \t]*\\(#+\\)[ \t]+\\([^\n]+\\)\n" 0 avoid-ranges
   (lambda ()
     (let* ((line-start (match-beginning 0))
            (hashes (match-beginning 1))
            (title-start (match-beginning 2))
            (title-end (match-end 2))
            (level (min 6 (- (match-end 1) hashes)))
            (source (unless (get-text-property hashes 'md-render-source)
                      (md-render-reconstruct hashes title-end)))
            (end (- title-end (- title-start line-start))))
       (delete-region line-start title-start)
       (md-render--mark-heading line-start end level)
       (when source
         (put-text-property line-start end 'md-render-source source))
       (1+ end)))))

(defun md-render--style-headings (avoid-ranges)
  "Render Setext and then ATX headings outside AVOID-RANGES."
  (md-render--style-setext-headings avoid-ranges)
  (md-render--style-atx-headings avoid-ranges))

;;;; Inline code

(defun md-render--style-inline-code (spans avoid-ranges)
  "Render the complete code SPANS that lie outside AVOID-RANGES."
  (dolist (span (reverse spans))
    (let* ((markup (alist-get :markup span))
           (body (alist-get :body span))
           (start (marker-position (alist-get :start markup)))
           (end (marker-position (alist-get :end markup)))
           (body-start (alist-get :start body))
           (body-end (alist-get :end body)))
      (when (and (alist-get :complete span)
                 (< body-start body-end)
                 (not (md-render-in-avoid-range-p start end avoid-ranges)))
        (let ((new-end (md-render--replace-markup
                        start end (buffer-substring body-start body-end))))
          (add-face-text-property start new-end 'md-render-inline-code)
          (add-text-properties start new-end
                               '(md-render-frozen
                                 t rear-nonsticky (md-render-frozen))))))))

;;;; Links

(cl-defun md-render--link-markup-regexp (&key as-image?)
  "Return the regexp for Markdown link markup.
Group 1 is the label.  The destination is either an angle-bracketed
path in group 2, which may contain spaces and parentheses, or a bare
destination in group 3.  When AS-IMAGE? is non-nil, match image markup
with a leading `!', whose label may be empty."
  (concat (if as-image? "!\\[\\([^]]*\\)\\]" "\\[\\([^]]+\\)\\]")
          "(\\(?:<\\([^<>\n]*\\)>\\|\\([^)]+\\)\\))"))

(defun md-render--link-markup-url ()
  "Return the destination of the last link markup match."
  (substring-no-properties (or (match-string 2) (match-string 3))))

(defun md-render--style-links (avoid-ranges)
  "Render links outside AVOID-RANGES."
  (md-render--rewrite-matches
   (md-render--link-markup-regexp) 0 avoid-ranges
   (lambda ()
     (let ((start (match-beginning 0)))
       (unless (eq (char-before start) ?!)
         (let* ((url (md-render--link-markup-url))
                (end (md-render--replace-markup
                      start (match-end 0) (match-string 1))))
           (add-face-text-property start end 'md-render-link)
           (add-text-properties
            start end
            `(keymap ,(md-render--link-map url)
                     mouse-face highlight
                     md-render-url ,url))
           end))))))

(defun md-render--link-map (url)
  "Return a keymap that opens URL."
  (md-render--action-map
   (lambda ()
     (interactive)
     (md-render--open-link url))))

(defun md-render-link-url-at-point (&optional pos)
  "Return the URL of the rendered link at POS, or at point."
  (get-text-property (or pos (point)) 'md-render-url))

;;;; Opening links

(defun md-render--parse-local-link (url)
  "Return (FILE . LINE) when URL names an existing local file, else nil.
LINE is nil when URL carries no line suffix."
  (let* ((line-suffix "\\(?:#L\\([0-9]+\\)\\|:\\([0-9]+\\)\\)?\\'")
         (parsed
          (save-match-data
            (cond
             ((string-match (concat "\\`file://\\(.+?\\)" line-suffix) url)
              (list (match-string 1 url) (match-string 2 url)
                    (match-string 3 url)))
             ((string-match (concat "\\`file:\\([^/].*?\\)" line-suffix) url)
              (list (match-string 1 url) (match-string 2 url)
                    (match-string 3 url)))
             ((string-match
               "\\`\\(\\(?:/?[A-Za-z]:/\\)?[^:#]+\\)#L\\([0-9]+\\)\\'" url)
              (list (match-string 1 url) (match-string 2 url) nil))
             ((string-match
               "\\`\\(\\(?:/?[A-Za-z]:/\\)?[^:#]+\\):\\([0-9]+\\)\\'" url)
              (list (match-string 1 url) nil (match-string 2 url)))
             ((not (string-empty-p url))
              (list url nil nil))))))
    (pcase parsed
      (`(,path ,hash-line ,colon-line)
       (let ((file (expand-file-name path))
             (line (or hash-line colon-line)))
         (when (file-exists-p file)
           (cons file (and line (string-to-number line)))))))))

(defun md-render--binary-file-p (file)
  "Return non-nil when the start of FILE contains a NUL byte."
  (and (file-readable-p file)
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (insert-file-contents-literally file nil 0 4096)
         (goto-char (point-min))
         (search-forward "\0" nil t))))

(defun md-render--open-externally (file)
  "Offer to open FILE with the system's default application."
  (when (y-or-n-p (format "Open %s externally? "
                          (file-name-nondirectory file)))
    (if (fboundp 'shell-command-do-open)
        (shell-command-do-open (list file))
      (browse-url-of-file file))))

(defun md-render--open-local-link (url)
  "Visit URL when it names a local file and return non-nil.
Binary files are offered to an external application; text files are
visited, at the linked line when URL names one."
  (pcase (md-render--parse-local-link url)
    (`(,file . ,line)
     (if (md-render--binary-file-p file)
         (md-render--open-externally file)
       (find-file file)
       (when line
         (goto-char (point-min))
         (forward-line (1- line))))
     t)))

(defun md-render--open-link (url)
  "Open URL inside Emacs when it is local, else with `browse-url'."
  (unless (md-render--open-local-link url)
    (browse-url url)))

(provide 'md-render-inline)
;;; md-render-inline.el ends here
