;;; md-render.el --- Render Markdown as propertized text -*- lexical-binding: t; -*-

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

;; Turn Markdown into propertized text, either as a string:
;;
;;   (md-render-convert "hello **world**")
;;
;; or in place in the current buffer:
;;
;;   (md-render-replace-markup)
;;
;; Markup characters are removed and replaced by faces, images and other
;; text properties.  Every rendered construct keeps its Markdown on the
;; `md-render-source' property, so `md-render-reconstruct' returns the
;; exact source.  Rendering is incremental: text streamed into a buffer is
;; rendered as it completes, and unfinished constructs stay literal.
;;
;; The renderer is split by concern:
;;
;;   md-render-core    faces, options, ranges, context and reconstruction
;;   md-render-inline  emphasis, headings, inline code and links
;;   md-render-block   rules, callouts, quotes, source panels and layout
;;   md-render-media   images, math and diagram previews
;;   md-render-table   text-grid tables and the TextUI table widget

;;; Code:

(require 'cl-lib)
(require 'md-render-block)
(require 'md-render-core)
(require 'md-render-inline)
(require 'md-render-media)
(require 'md-render-table)
(require 'textui)

(defun md-render--run-render-functions (context)
  "Run `md-render-render-functions' with CONTEXT and collect results."
  (let ((results nil))
    (run-hook-wrapped 'md-render-render-functions
                      (lambda (function)
                        (when-let* ((result (funcall function context)))
                          (push result results))
                        nil))
    (nreverse results)))

(cl-defun md-render-replace-markup (&key force (render-images t)
                                         (highlight-blocks t)
                                         image-cache-directory
                                         defer-tables)
  "Render the Markdown in the current buffer in place.
Rendering resumes where the previous call stopped; with FORCE, the
whole buffer is scanned again.  RENDER-IMAGES displays images and
HIGHLIGHT-BLOCKS fontifies fenced code in its language.  Remote images
are downloaded into IMAGE-CACHE-DIRECTORY when it is non-nil.  With
DEFER-TABLES, tables keep their styled source so that a
`md-render-table-widget' can lay them out."
  (save-excursion
    (when force
      (with-silent-modifications
        (remove-text-properties (point-min) (point-max)
                                '(md-render-watermark nil))))
    (let* ((watermark (md-render--watermark-start))
           (front-matter (md-render-front-matter-end))
           (start (min (point-max) (max watermark (or front-matter watermark))))
           (blocks nil)
           (watermarks nil))
      (save-restriction
        (narrow-to-region start (point-max))
        (md-render--clear-layout-state)
        (let* ((context (md-render-context))
               (source-ranges (md-render-sort-ranges
                               (md-render--block-ranges
                                (alist-get :source-blocks context))))
               (results (when md-render-render-functions
                          (md-render--run-render-functions context)))
               (avoid-ranges (md-render-sort-ranges
                              source-ranges
                              (md-render--frozen-ranges)
                              (alist-get :inline-code-ranges context))))
          (setq blocks (alist-get :source-blocks context)
                watermarks (delq nil (mapcar (lambda (result)
                                               (alist-get :watermark result))
                                             results)))
          (md-render--style-emphasis avoid-ranges)
          (md-render--style-headings avoid-ranges)
          (md-render--style-inline-code (alist-get :code-spans context)
                                        source-ranges)
          (md-render--style-links avoid-ranges)
          (when render-images
            (md-render--style-images avoid-ranges image-cache-directory)
            (md-render--style-image-paths avoid-ranges))
          (md-render--style-dividers avoid-ranges)
          (md-render--style-callouts avoid-ranges)
          (md-render--style-blockquotes avoid-ranges)
          (md-render--style-source-blocks highlight-blocks)
          (md-render--style-tables :avoid-ranges source-ranges
                                   :defer defer-tables)
          (md-render--annotate-list-lines avoid-ranges)
          (md-render--mirror-faces (point-min) (point-max))
          (put-text-property (point-min) (point-max) 'yank-handler
                             (list #'md-render--yank-plain))
          (put-text-property (point-min) (point-max) 'fontified t)))
      (md-render--update-watermark blocks watermarks))))

(defun md-render-convert (markdown)
  "Return MARKDOWN rendered as a propertized string."
  (with-temp-buffer
    (insert markdown)
    (md-render-replace-markup)
    (buffer-string)))

(provide 'md-render)
;;; md-render.el ends here
