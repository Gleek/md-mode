;;; md-render-block.el --- Block-level Markdown rendering -*- lexical-binding: t; -*-

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

;; Passes for Markdown blocks that keep their line structure: thematic
;; breaks, GitHub callouts, block quotes and fenced source panels.  Also
;; the continuation layout that indents wrapped list items and headings.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'md-render-core)

;;;; Dividers

(defun md-render--style-dividers (avoid-ranges)
  "Draw thematic breaks outside AVOID-RANGES as a rule."
  (let ((case-fold-search nil)
        (rule (concat (propertize (make-string 12 ?\s) 'face '(:underline t))
                      "\n")))
    (goto-char (point-min))
    (while (re-search-forward
            "^[ \t]*\\(?:\\*\\{3,\\}\\|-\\{3,\\}\\|_\\{3,\\}\\)[ \t]*$" nil t)
      (let ((start (match-beginning 0))
            (end (match-end 0)))
        (unless (or (= start end)
                    (md-render-in-avoid-range-p start end avoid-ranges))
          (add-text-properties
           start end
           `(display ,rule
                     md-render-frozen t
                     rear-nonsticky (display md-render-frozen))))
        (unless (eobp)
          (forward-char 1))))))

;;;; Callouts

(defconst md-render--callout-kinds
  '((NOTE "Note" font-lock-constant-face)
    (TIP "Tip" success)
    (IMPORTANT "Important" font-lock-keyword-face)
    (WARNING "Warning" warning)
    (CAUTION "Caution" error))
  "GitHub callout kinds with their titles and accent faces.")

(defun md-render--panel-prefix (face)
  "Return the line prefix of a tinted panel drawn in FACE.
Two plain columns of margin precede two columns of panel background,
so every panel's left edge lines up."
  (concat "  " (propertize "  " 'face face)))

(defun md-render--callout-face (accent)
  "Return the panel face of a callout whose kind is drawn in ACCENT.
The panel background is ACCENT's color faded into the default
background by `md-render-callout-tint'.  When either color is unknown,
as on some terminals, fall back to `md-render-callout'."
  (let ((foreground (face-foreground accent nil t))
        (background (face-background 'default nil t)))
    (if (and (stringp foreground) (color-defined-p foreground)
             (stringp background) (color-defined-p background))
        `((:background
           ,(apply #'color-rgb-to-hex
                   (append (cl-mapcar (lambda (tint base)
                                        (+ (* md-render-callout-tint tint)
                                           (* (- 1 md-render-callout-tint)
                                              base)))
                                      (color-name-to-rgb foreground)
                                      (color-name-to-rgb background))
                           '(2)))
           :extend t)
          md-render-callout)
      'md-render-callout)))

(defun md-render--style-callouts (avoid-ranges)
  "Render GitHub callout blocks outside AVOID-RANGES."
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (re-search-forward
            (concat "^[ \t]*\\(>[ \t]+\\)\\(\\[!"
                    "\\(NOTE\\|TIP\\|IMPORTANT\\|WARNING\\|CAUTION\\)"
                    "\\]\\)[ \t]*\n")
            nil t)
      (let ((start (match-beginning 0))
            (marker-end (match-end 1))
            (title-start (match-beginning 2))
            (title-end (match-end 2))
            (kind (intern (match-string-no-properties 3))))
        (unless (md-render-in-avoid-range-p start (point) avoid-ranges)
          (while (looking-at "[ \t]*>[^\n]*\n")
            (goto-char (match-end 0)))
          (goto-char
           (md-render--decorate-callout
            :start start :end (point) :kind kind
            :title-start title-start :title-end title-end
            :first-marker-end marker-end)))))))

(cl-defun md-render--decorate-callout
    (&key start end kind title-start title-end first-marker-end)
  "Draw the callout from START to END as a KIND panel and return its end.
TITLE-START and TITLE-END bound the `[!KIND]' marker, which shows the
kind's title.  Each line's `>' and the blank after it become the
panel's left padding; on the first line the padding runs to
FIRST-MARKER-END.  Like a source panel, the callout gets a padding
line above and below; they are decoration without Markdown source."
  (pcase-let* ((`(,title ,accent) (alist-get kind md-render--callout-kinds))
               (face (md-render--callout-face accent))
               (padding (propertize "  " 'face face))
               (prefix (concat "  " padding))
               (carried (md-render--carry-properties start))
               (end (copy-marker end)))
    (add-face-text-property start end face t)
    (add-text-properties
     start end
     `(md-render-callout ,kind
                         md-render-frozen t
                         line-prefix "  "
                         wrap-prefix ,prefix
                         rear-nonsticky (md-render-callout md-render-frozen)))
    (put-text-property
     title-start title-end 'display
     (propertize title 'face `(md-render-callout-title ,accent ,@(ensure-list
                                                                  face))))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (when (looking-at "[ \t]*\\(>[ \t]?\\)")
          (put-text-property (match-beginning 1)
                             (if (= (point) start)
                                 first-marker-end
                               (match-end 1))
                             'display padding))
        (forward-line 1)))
    (let ((pad (lambda ()
                 (let ((from (point)))
                   (insert (propertize "\n"
                                       'face face
                                       'line-prefix prefix
                                       'wrap-prefix prefix
                                       'md-render-callout kind
                                       'md-render-source ""
                                       'md-render-non-trimmable t
                                       'rear-nonsticky
                                       '(md-render-non-trimmable)))
                   (when carried
                     (add-text-properties from (point) carried))))))
      (goto-char end)
      (funcall pad)
      (let ((after (point)))
        (goto-char start)
        (funcall pad)
        (+ after 1)))))

;;;; Block quotes

(defun md-render--style-blockquotes (avoid-ranges)
  "Render block quote lines outside AVOID-RANGES.
Each `>' shows as a bar, and the line is indented like a source panel
so quotes and panels share one left edge."
  (let ((case-fold-search nil)
        (bar (propertize "┃" 'face 'md-render-blockquote-bar)))
    (goto-char (point-min))
    (while (re-search-forward "^\\([ \t]*>[ \t>]*\\)[^\n]*\n" nil t)
      (let* ((start (match-beginning 0))
             (prefix-end (match-end 1))
             (end (match-end 0))
             (markers (mapconcat (lambda (char)
                                   (if (eq char ?>) bar (string char)))
                                 (match-string-no-properties 1) "")))
        (unless (or (md-render-in-avoid-range-p start end avoid-ranges)
                    (get-text-property start 'md-render-callout))
          (save-excursion
            (goto-char start)
            (while (search-forward ">" prefix-end t)
              (put-text-property (1- (point)) (point) 'display bar)))
          (add-face-text-property start (1- end) 'md-render-blockquote)
          (add-text-properties
           start end
           `(md-render-frozen t
                              line-prefix "  "
                              wrap-prefix ,(concat "  " markers)
                              rear-nonsticky (md-render-frozen))))))))

;;;; Fenced source blocks

(defun md-render--language-mode (language)
  "Return the major mode for fence LANGUAGE, or nil when there is none."
  (let ((name (downcase (string-trim (or language "")))))
    (unless (string-empty-p name)
      (let ((mode (intern (concat (or (cdr (assoc name
                                                  md-render-language-mapping))
                                      name)
                                  "-mode"))))
        (and (fboundp mode) mode)))))

(defun md-render--highlight-code (code language)
  "Return CODE fontified by the major mode of LANGUAGE.
CODE is returned unchanged when LANGUAGE has no major mode."
  (if-let* ((mode (md-render--language-mode language)))
      (with-temp-buffer
        (insert code)
        (let ((inhibit-message t))
          (delay-mode-hooks (funcall mode)))
        (font-lock-ensure)
        (buffer-string))
    code))

(defun md-render--layer-faces (text offset)
  "Prepend the faces of TEXT onto the buffer starting at OFFSET."
  (let ((pos 0))
    (while (< pos (length text))
      (let ((face (get-text-property pos 'face text))
            (next (next-single-property-change pos 'face text (length text))))
        (when face
          (let ((start (+ offset pos))
                (end (+ offset next)))
            (if (and (consp face) (not (keywordp (car face))))
                (dolist (layer (reverse face))
                  (add-face-text-property start end layer))
              (add-face-text-property start end face))))
        (setq pos next)))))

(defun md-render--copy-source-block ()
  "Copy the body of the source block after point to the kill ring."
  (interactive)
  (let* ((start (next-single-property-change
                 (point) 'md-render-source-block-body))
         (end (and start
                   (get-text-property start 'md-render-source-block-body)
                   (next-single-property-change
                    start 'md-render-source-block-body nil (point-max)))))
    (when end
      (kill-new (buffer-substring-no-properties start end))
      (message "Copied"))))

(defun md-render--announce-copy (_window _old-position direction)
  "Explain how to copy a source block when DIRECTION is `entered'."
  (when (eq direction 'entered)
    (message "Press RET to copy")))

(defconst md-render--source-block-prefix
  (md-render--panel-prefix 'md-render-source-block)
  "Line prefix that indents the panel of a fenced source block.")

(defun md-render--source-block-vpad ()
  "Return a newline that pads a source block panel."
  (propertize "\n"
              'face 'md-render-source-block
              'line-prefix md-render--source-block-prefix
              'wrap-prefix md-render--source-block-prefix
              'md-render-non-trimmable t
              'rear-nonsticky '(md-render-non-trimmable)))

(defun md-render--source-block-label (language)
  "Return the copy label for a source block in LANGUAGE."
  (propertize (concat (if (string-empty-p language) "snippet" language)
                      " " md-render-source-block-copy-symbol)
              'face 'md-render-source-block-language
              'mouse-face 'highlight
              'pointer 'hand
              'keymap (md-render--action-map #'md-render--copy-source-block)
              'cursor-sensor-functions (list #'md-render--announce-copy)
              'md-render-frozen t
              'rear-nonsticky '(md-render-frozen)
              'line-prefix md-render--source-block-prefix
              'wrap-prefix md-render--source-block-prefix))

(defun md-render--find-closing-fence (ticks body-start)
  "Return the match of the fence closing TICKS after BODY-START, or nil.
The match data then covers the closing line."
  (let ((closer (concat "^[ \t]*" (regexp-quote ticks)
                        "[ \t]*\\(?:\n\\|\\'\\)"))
        (found nil))
    (while (and (not found) (re-search-forward closer nil t))
      (when (> (match-beginning 0) body-start)
        (setq found t)))
    found))

(defun md-render--style-source-blocks (highlight)
  "Render complete fenced blocks as panels, fontified when HIGHLIGHT."
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (re-search-forward
            "^[ \t]*\\(`\\{3,\\}\\)[ \t]*\\([[:alnum:]+#-]*\\)[ \t]*\n" nil t)
      (let ((open-start (match-beginning 0))
            (body-start (match-end 0))
            (ticks (match-string-no-properties 1))
            (language (match-string-no-properties 2)))
        (if (and (not (get-text-property body-start 'md-render-frozen))
                 (md-render--find-closing-fence ticks body-start))
            (goto-char
             (md-render--render-source-block
              :open-start open-start :body-start body-start
              :close-start (match-beginning 0) :close-end (match-end 0)
              :language language :highlight highlight))
          (goto-char body-start))))))

(cl-defun md-render--render-source-block
    (&key open-start body-start close-start close-end language highlight)
  "Turn one fenced block into a panel and return the end of its body.
OPEN-START and BODY-START bound the opening fence line; CLOSE-START and
CLOSE-END bound the closing one.  LANGUAGE labels the panel, and the
body is fontified in its mode when HIGHLIGHT is non-nil."
  (let* ((source (buffer-substring-no-properties open-start close-end))
         (body-end (1- close-start))
         (body (buffer-substring-no-properties body-start body-end))
         (styled (if highlight (md-render--highlight-code body language) body))
         (carried (md-render--carry-properties body-start))
         (prefix md-render--source-block-prefix))
    (delete-region close-start close-end)
    (delete-region open-start body-start)
    (let* ((start open-start)
           (content-end (+ start (length body)))
           (panel-end (1+ content-end)))
      (put-text-property start panel-end 'face 'md-render-source-block)
      (md-render--layer-faces styled start)
      (add-text-properties
       start panel-end
       `(md-render-frozen t
                          md-render-non-trimmable t
                          rear-nonsticky (md-render-frozen
                                          md-render-non-trimmable)
                          line-prefix ,prefix
                          wrap-prefix ,prefix))
      (goto-char start)
      (insert (md-render--source-block-vpad)
              (md-render--source-block-label language)
              (md-render--source-block-vpad)
              (md-render--source-block-vpad))
      (let* ((header-end (point))
             (content-end (+ header-end (length body)))
             (panel-end (1+ content-end)))
        (when carried
          (add-text-properties start header-end carried))
        (put-text-property header-end content-end
                           'md-render-source-block-body t)
        (goto-char panel-end)
        (insert (md-render--source-block-vpad))
        (when carried
          (add-text-properties panel-end (point) carried))
        (put-text-property start (point) 'md-render-source "")
        (put-text-property header-end content-end 'md-render-source source)
        content-end))))

(defun md-render-source-block-at-point (&optional pos)
  "Return the body of the source block at POS, or at point, or nil."
  (let ((pos (or pos (point))))
    (when (get-text-property pos 'md-render-source-block-body)
      (buffer-substring-no-properties
       (or (previous-single-property-change
            (1+ pos) 'md-render-source-block-body)
           (point-min))
       (next-single-property-change
        pos 'md-render-source-block-body nil (point-max))))))

;;;; Continuation layout

(defconst md-render--list-marker-regexp
  "[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+"
  "Regexp for the marker that opens a list item line.")

(defun md-render--remove-owned-wrap-prefixes ()
  "Remove the wrap prefixes the continuation layout added."
  (dolist (run (md-render--property-runs 'md-render-wrap-prefix))
    (remove-text-properties (car run) (cdr run)
                            '(wrap-prefix nil md-render-wrap-prefix nil))))

(defun md-render--clear-layout-state ()
  "Remove owned continuation prefixes and line contexts from the buffer."
  (with-silent-modifications
    (md-render--remove-owned-wrap-prefixes)
    (remove-text-properties (point-min) (point-max)
                            '(md-render-line-context nil))))

(defun md-render--laid-out-line-p (pos)
  "Return non-nil when the line starting at POS already has a layout."
  (or (get-text-property pos 'md-render-frozen)
      (get-text-property pos 'line-prefix)
      (get-text-property pos 'wrap-prefix)))

(defun md-render--annotate-list-lines (avoid-ranges)
  "Record the marker width of list lines outside AVOID-RANGES."
  (goto-char (point-min))
  (while (< (point) (point-max))
    (let ((start (point)))
      (unless (or (= start (pos-eol))
                  (md-render--laid-out-line-p start)
                  (get-text-property start 'md-render-line-context)
                  (md-render-in-avoid-range-p start (1+ start) avoid-ranges)
                  (not (looking-at md-render--list-marker-regexp)))
        (put-text-property
         start (1+ start) 'md-render-line-context
         (list :kind 'list :width (string-width (match-string 0)))))
      (forward-line 1))))

(defun md-render--continuation-prefix (context)
  "Return the wrap prefix for a line with CONTEXT, or nil."
  (pcase (plist-get context :kind)
    ('list (make-string (plist-get context :width) ?\s))
    ('heading (propertize (make-string (plist-get context :level) ?\s)
                          'face (plist-get context :face)))))

(cl-defun md-render-apply-continuation-layout (&key enabled)
  "Indent wrapped continuation lines of list items and headings.
Remove the prefixes this function added before; when ENABLED is
non-nil, add fresh ones.  Only text properties change."
  (save-excursion
    (with-silent-modifications
      (md-render--remove-owned-wrap-prefixes)
      (when enabled
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let ((start (point))
                (end (pos-eol)))
            (when-let* (((< start end))
                        ((not (md-render--laid-out-line-p start)))
                        (prefix (md-render--continuation-prefix
                                 (get-text-property
                                  start 'md-render-line-context))))
              (add-text-properties start end
                                   `(wrap-prefix ,prefix
                                                 md-render-wrap-prefix t))))
          (forward-line 1))))))

(provide 'md-render-block)
;;; md-render-block.el ends here
