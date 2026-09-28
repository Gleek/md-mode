;;; md-render-core.el --- Shared state for Markdown rendering -*- lexical-binding: t; -*-

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

;; The vocabulary every md-render pass shares: the customization group,
;; faces and options, the text properties that record what was rendered,
;; protected ranges, source reconstruction, the scanners for fenced blocks
;; and code spans that build the render context, and the streaming
;; watermark.

;;; Code:

(require 'cl-lib)
(require 'org-faces)
(require 'subr-x)

(defgroup md-render nil
  "Render Markdown text into propertized form."
  :group 'text)

;;;; Faces

(defface md-render-bold
  '((t :inherit bold))
  "Face for bold Markdown text."
  :group 'md-render)

(defface md-render-italic
  '((t :inherit italic))
  "Face for italic Markdown text."
  :group 'md-render)

(defface md-render-strikethrough
  '((t :strike-through t))
  "Face for struck-through Markdown text."
  :group 'md-render)

(defface md-render-inline-code
  '((t :inherit org-code))
  "Face for inline Markdown code."
  :group 'md-render)

(defface md-render-link
  '((t :inherit link))
  "Face for Markdown link titles."
  :group 'md-render)

(defface md-render-blockquote
  '((t :inherit font-lock-comment-face))
  "Face for Markdown block quotes."
  :group 'md-render)

(defface md-render-blockquote-bar
  '((t :inherit shadow))
  "Face for the bar along the left edge of a block quote."
  :group 'md-render)

(defface md-render-callout
  '((t :inherit org-block :foreground unspecified :extend t))
  "Face for the panel of a GitHub callout.
Each callout kind tints this panel with its own accent color; see
`md-render-callout-tint'."
  :group 'md-render)

(defface md-render-callout-title
  '((t :inherit bold))
  "Face for the title of a GitHub callout."
  :group 'md-render)

(defface md-render-header-1
  '((((type tty) (background dark)) :inherit org-level-1 :foreground "#ffffff")
    (((type tty) (background light)) :inherit org-level-1 :foreground "#000000")
    (t :inherit bold :height 2.0))
  "Face for level 1 Markdown headings."
  :group 'md-render)

(defface md-render-header-2
  '((((type tty) (background dark)) :inherit org-level-2 :foreground "#d2b580")
    (((type tty) (background light)) :inherit org-level-2 :foreground "#624416")
    (t :inherit bold :height 1.7))
  "Face for level 2 Markdown headings."
  :group 'md-render)

(defface md-render-header-3
  '((((type tty) (background dark)) :inherit org-level-3 :foreground "#82b0ec")
    (((type tty) (background light)) :inherit org-level-3 :foreground "#193668")
    (t :inherit bold :height 1.4))
  "Face for level 3 Markdown headings."
  :group 'md-render)

(defface md-render-header-4
  '((((type tty) (background dark)) :inherit org-level-4 :foreground "#feacd0")
    (((type tty) (background light)) :inherit org-level-4 :foreground "#721045")
    (t :inherit bold :height 1.1))
  "Face for level 4 Markdown headings."
  :group 'md-render)

(defface md-render-header-5
  '((((type tty) (background dark)) :inherit org-level-5 :foreground "#88ca9f")
    (((type tty) (background light)) :inherit org-level-5 :foreground "#2a5045")
    (t :inherit bold :height 1.0))
  "Face for level 5 Markdown headings."
  :group 'md-render)

(defface md-render-header-6
  '((((type tty) (background dark)) :inherit org-level-6 :foreground "#ff9580")
    (((type tty) (background light)) :inherit org-level-6 :foreground "#7f0000")
    (t :inherit bold :height 1.0))
  "Face for level 6 Markdown headings."
  :group 'md-render)

(defface md-render-table-header
  '((t :inherit bold))
  "Face for Markdown table header rows."
  :group 'md-render)

(defface md-render-table-border
  '((t :inherit shadow))
  "Face for Markdown table borders."
  :group 'md-render)

(defface md-render-table-zebra
  '((((class color) (background light)) :background "gray95")
    (((class color) (background dark)) :background "gray20")
    (t :inherit highlight))
  "Face for alternate Markdown table data rows."
  :group 'md-render)

(defface md-render-source-block
  '((t :inherit org-block :foreground unspecified :extend t))
  "Face for the panel of a fenced source block."
  :group 'md-render)

(defface md-render-source-block-language
  '((t :inherit (italic font-lock-type-face md-render-source-block)))
  "Face for the language label of a fenced source block."
  :group 'md-render)

;;;; Options

(defcustom md-render-image-max-width 0.4
  "Maximum width of inline images.
An integer is a width in pixels.  A float between 0 and 1 is a fraction
of the pixel width of the window that shows the buffer."
  :type '(choice (integer :tag "Pixels")
                 (float :tag "Fraction of window width"))
  :group 'md-render)

(defcustom md-render-prettify-tables t
  "When non-nil, render Markdown tables as aligned grids."
  :type 'boolean
  :group 'md-render)

(defcustom md-render-table-use-unicode-borders t
  "When non-nil, draw table borders with box-drawing characters."
  :type 'boolean
  :group 'md-render)

(defcustom md-render-table-wrap-columns t
  "When non-nil, shrink and wrap wide tables to fit the window."
  :type 'boolean
  :group 'md-render)

(defcustom md-render-table-max-width-fraction 0.9
  "Fraction of the window width that a wrapped text table may use."
  :type 'float
  :group 'md-render)

(defcustom md-render-table-zebra-stripe t
  "When non-nil, give alternate table data rows a background."
  :type 'boolean
  :group 'md-render)

(defcustom md-render-language-mapping
  '(("elisp" . "emacs-lisp")
    ("objective-c" . "objc")
    ("objectivec" . "objc")
    ("cpp" . "c++"))
  "Alist mapping fence languages to major mode name prefixes.
Keys are lower-case fence languages.  Values name a major mode without
its \"-mode\" suffix."
  :type '(alist :key-type string :value-type string)
  :group 'md-render)

(defcustom md-render-source-block-copy-symbol "⎘"
  "Symbol after the language label that copies a source block."
  :type 'string
  :group 'md-render)

(defcustom md-render-callout-tint 0.15
  "How strongly a callout's accent color tints its panel background.
0 leaves the default background and 1 uses the accent color itself."
  :type 'float
  :group 'md-render)

;;;; Rendered-text properties

(defconst md-render--owned-properties
  '(face font-lock-face display wrap-prefix md-render-wrap-prefix
         md-render-line-context md-render-frozen md-render-table-source
         md-render-source rear-nonsticky)
  "Text properties that the renderer computes and never carries over.")

(defun md-render--carry-properties (pos)
  "Return the properties at POS that belong to the caller, as a plist.
Properties the renderer computes itself, such as faces, displays and
source bookkeeping, are left out; everything else keeps its order."
  (cl-loop for (property value) on (text-properties-at pos) by #'cddr
           unless (memq property md-render--owned-properties)
           append (list property value)))

(defun md-render-reconstruct (beg end)
  "Return the Markdown source of the text between BEG and END.
Each run of text whose `md-render-source' lies wholly inside the span
contributes that stored source; an empty stored source marks inserted
decoration and contributes nothing; any other text contributes itself.
The result has no text properties."
  (let ((pos beg)
        (parts nil))
    (while (< pos end)
      (let* ((source (get-text-property pos 'md-render-source))
             (run-end (next-single-property-change
                       pos 'md-render-source nil (point-max)))
             (piece-end (min run-end end))
             (whole (and (stringp source)
                         (<= run-end end)
                         (or (= pos (point-min))
                             (not (eq source
                                      (get-text-property
                                       (1- pos) 'md-render-source)))))))
        (cond
         ((equal source ""))
         (whole (push source parts))
         (t (push (buffer-substring-no-properties pos piece-end) parts)))
        (setq pos piece-end)))
    (apply #'concat (nreverse parts))))

(defun md-render--deconstruct (text)
  "Split TEXT into a list of (STRING FACES) runs.
Adjacent characters whose faces are `equal' share one run.  STRING has
no properties and FACES is always a list."
  (let ((pos 0)
        (runs nil))
    (while (< pos (length text))
      (let ((face (get-text-property pos 'face text))
            (next (1+ pos)))
        (while (and (< next (length text))
                    (equal face (get-text-property next 'face text)))
          (setq next (next-single-property-change next 'face text
                                                  (length text))))
        (push (list (substring-no-properties text pos next)
                    (if (listp face) face (list face)))
              runs)
        (setq pos next)))
    (nreverse runs)))

(defun md-render--text-has-face-p (text)
  "Return non-nil when any character of TEXT has a `face'."
  (text-property-not-all 0 (length text) 'face nil text))

(defun md-render--action-map (command)
  "Return a keymap that runs COMMAND on RET and on a mouse click.
Self-inserting keys are ignored so rendered text stays intact."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") command)
    (define-key map [mouse-1] command)
    (define-key map [remap self-insert-command] #'ignore)
    map))

(defun md-render--mirror-faces (beg end)
  "Copy each `face' run between BEG and END onto `font-lock-face'."
  (let ((pos beg))
    (while (< pos end)
      (let ((face (get-text-property pos 'face))
            (next (next-single-property-change pos 'face nil end)))
        (when face
          (put-text-property pos next 'font-lock-face face))
        (setq pos next)))))

(defun md-render--yank-plain (string)
  "Insert STRING without any text properties."
  (insert (substring-no-properties string)))

;;;; Window geometry

(defun md-render--window-columns (window)
  "Return how many characters fit on one line of WINDOW."
  (window-max-chars-per-line window))

(defun md-render--display-width ()
  "Return the width in columns available to rendered text."
  (let ((window (selected-window)))
    (if (window-live-p window)
        (md-render--window-columns window)
      80)))

;;;; Lines

(defun md-render--line-start (pos)
  "Return the start of the line containing POS, ignoring fields."
  (save-excursion (goto-char pos) (pos-bol)))

(defun md-render--line-end (pos)
  "Return the end of the line containing POS, ignoring fields."
  (save-excursion (goto-char pos) (pos-eol)))

;;;; Protected ranges

(defun md-render-sort-ranges (&rest range-collections)
  "Merge RANGE-COLLECTIONS into one vector sorted by start position.
Each collection is a list or vector of (BEG . END) conses whose
endpoints are integers or markers.  The conses are shared."
  (sort (apply #'vconcat range-collections)
        (lambda (a b) (< (car a) (car b)))))

(defun md-render-in-avoid-range-p (start end avoid-ranges)
  "Return the range of AVOID-RANGES that contains START to END, or nil.
AVOID-RANGES is a vector of non-overlapping ranges sorted as by
`md-render-sort-ranges'.  Containment includes both endpoints."
  (when (and avoid-ranges (> (length avoid-ranges) 0))
    (let ((ranges (if (vectorp avoid-ranges)
                      avoid-ranges
                    (vconcat avoid-ranges)))
          (low 0)
          (high (1- (length avoid-ranges)))
          (candidate nil))
      (while (<= low high)
        (let* ((middle (/ (+ low high) 2))
               (range (aref ranges middle)))
          (if (<= (car range) start)
              (setq candidate range
                    low (1+ middle))
            (setq high (1- middle)))))
      (and candidate (<= end (cdr candidate)) candidate))))

(defun md-render--marker-range (start end)
  "Return a (START . END) cons of fresh markers."
  (cons (copy-marker start) (copy-marker end)))

(defun md-render--property-runs (property)
  "Return (START . END) runs where PROPERTY is non-nil, in order."
  (let ((pos (point-min))
        (runs nil))
    (while (< pos (point-max))
      (if (not (get-text-property pos property))
          (setq pos (next-single-property-change
                     pos property nil (point-max)))
        (let ((end (or (text-property-any pos (point-max) property nil)
                       (point-max))))
          (push (cons pos end) runs)
          (setq pos end))))
    (nreverse runs)))

(defun md-render--frozen-ranges ()
  "Return marker ranges covering every frozen run in the buffer."
  (mapcar (lambda (run) (md-render--marker-range (car run) (cdr run)))
          (md-render--property-runs 'md-render-frozen)))

;;;; Front matter

(defun md-render-front-matter-end ()
  "Return the position just after YAML front matter, or nil.
Front matter opens with a `---' line at the very start of the buffer
and closes with the next `---' or `...' line.  Narrowing is ignored."
  (save-excursion
    (save-restriction
      (widen)
      (save-match-data
        (goto-char (point-min))
        (when (and (looking-at "---[ \t]*$")
                   (zerop (forward-line 1))
                   (re-search-forward "^\\(?:---\\|\\.\\.\\.\\)[ \t]*$"
                                      nil t))
          (if (eobp) (point-max) (1+ (point))))))))

;;;; Fenced blocks and code spans

(defconst md-render--fence-line-regexp
  "^[ \t]*\\(`\\{3,\\}\\)[ \t]*\\([[:alnum:]+#-]*\\).*$"
  "Regexp for a line that opens or closes a fenced block.")

(defun md-render--block-descriptor (language start end body)
  "Return a fenced block descriptor for LANGUAGE from START to END.
BODY is the block's text, or nil while the block is still open."
  `((:language . ,language)
    (:block . ((:start . ,(copy-marker start))
               (:end . ,(copy-marker end))))
    (:body . ,body)
    (:complete . ,(and body t))))

(defun md-render--source-blocks ()
  "Return descriptors for every fenced block in the buffer, in order."
  (let ((case-fold-search nil)
        (blocks nil)
        (open nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward md-render--fence-line-regexp nil t)
        (let ((ticks (length (match-string 1)))
              (line-start (match-beginning 0))
              (next-line (min (point-max) (1+ (match-end 0)))))
          (pcase open
            ('nil
             (setq open (list line-start ticks
                              (downcase (match-string-no-properties 2))
                              next-line)))
            (`(,start ,count ,language ,body-start)
             (when (>= ticks count)
               (let ((body (buffer-substring-no-properties
                            body-start (max body-start line-start))))
                 (push (md-render--block-descriptor
                        language start next-line
                        (string-remove-suffix "\n" body))
                       blocks))
               (setq open nil)))))
        (unless (eobp)
          (forward-char 1))))
    (when open
      (push (md-render--block-descriptor
             (nth 2 open) (car open) (point-max) nil)
            blocks))
    (nreverse blocks)))

(defun md-render--block-ranges (blocks)
  "Return the (START . END) marker ranges of fenced BLOCKS."
  (mapcar (lambda (block)
            (let ((range (alist-get :block block)))
              (cons (alist-get :start range) (alist-get :end range))))
          blocks))

(defun md-render--span-descriptor (open-start open-end close-start close-end)
  "Return a code span descriptor from its delimiter positions.
OPEN-START and OPEN-END bound the opening backticks.  CLOSE-START and
CLOSE-END bound the closing ones; when CLOSE-START is nil the span is
incomplete and runs to CLOSE-END."
  (let ((body-end (or close-start close-end)))
    `((:markup . ((:start . ,(copy-marker open-start))
                  (:end . ,(copy-marker close-end))))
      (:body . ((:start . ,(copy-marker open-end))
                (:end . ,(copy-marker body-end))))
      (:complete . ,(and close-start t)))))

(defun md-render--code-spans (&optional avoid-ranges)
  "Return inline code span descriptors, skipping AVOID-RANGES."
  (let ((case-fold-search nil)
        (spans nil))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((eol (pos-eol))
              (open nil))
          (while (re-search-forward "`+" eol t)
            (let* ((start (match-beginning 0))
                   (end (match-end 0))
                   (range (md-render-in-avoid-range-p
                           start (1+ start) avoid-ranges)))
              (cond
               (range (goto-char (min eol (max end (cdr range)))))
               ((null open) (setq open (cons start end)))
               ((= (- end start) (- (cdr open) (car open)))
                (push (md-render--span-descriptor
                       (car open) (cdr open) start end)
                      spans)
                (setq open nil)))))
          (when open
            (push (md-render--span-descriptor (car open) (cdr open) nil eol)
                  spans))
          (goto-char (min (point-max) (1+ eol))))))
    (nreverse spans)))

(defun md-render-context ()
  "Return the render context of the accessible buffer as an alist.
It holds `:source-blocks', the fenced block descriptors, `:code-spans',
the inline code span descriptors, and `:inline-code-ranges', one marker
range per code span body."
  (let* ((blocks (md-render--source-blocks))
         (spans (md-render--code-spans
                 (md-render-sort-ranges (md-render--block-ranges blocks)))))
    `((:source-blocks . ,blocks)
      (:code-spans . ,spans)
      (:inline-code-ranges
       . ,(mapcar (lambda (span)
                    (let ((body (alist-get :body span)))
                      (cons (alist-get :start body) (alist-get :end body))))
                  spans)))))

;;;; Streaming watermark

(defun md-render--watermark-start ()
  "Return where the next render should start scanning."
  (let ((mark (and (< (point-min) (point-max))
                   (get-text-property (point-min) 'md-render-watermark))))
    (if (and (integerp mark) (<= (point-min) mark (point-max)))
        mark
      (point-min))))

(defun md-render--extending-table-start ()
  "Return the start of a table that later text may still extend, or nil."
  (save-excursion
    (goto-char (point-max))
    (if (and (bolp) (not (bobp)))
        (forward-line -1)
      (goto-char (pos-bol)))
    (let ((pending nil)
          (found nil)
          (done nil))
      (while (not done)
        (cond
         ((and (< (point) (point-max))
               (get-text-property (point) 'md-render-table-source))
          (setq found (or (previous-single-property-change
                           (1+ (point)) 'md-render-table-source)
                          (point-min))
                done t))
         ((looking-at-p "[ \t]*|")
          (setq pending (point))
          (if (bobp)
              (setq done t)
            (forward-line -1)))
         (t (setq done t))))
      (or found pending))))

(defun md-render--update-watermark (blocks watermarks)
  "Record where the next streaming render must resume.
BLOCKS are the fenced block descriptors and WATERMARKS the positions
that external renderers asked to hold back."
  (when (< (point-min) (point-max))
    (let* ((last-block (car (last blocks)))
           (block-range (alist-get :block last-block))
           (candidates
            (append
             (list (save-excursion (goto-char (point-max)) (pos-bol)))
             (when (and block-range
                        (= (alist-get :end block-range) (point-max)))
               (list (marker-position (alist-get :start block-range))))
             (delq nil (list (md-render--extending-table-start)))
             watermarks)))
      (with-silent-modifications
        (put-text-property (point-min) (1+ (point-min)) 'md-render-watermark
                           (apply #'min candidates))))))

(provide 'md-render-core)
;;; md-render-core.el ends here
