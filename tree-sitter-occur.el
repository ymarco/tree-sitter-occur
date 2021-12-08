;;; tree-sitter-occur.el --- TODO description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Yoav Marco
;;
;; Author: Yoav Marco <https://github.com/ymarco>
;; Maintainer: Yoav Marco <yoavm448@gmail.com>
;; Created: December 08, 2021
;; Modified: December 08, 2021
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/ymarco/consult-tree-sitter-grep
;; Package-Requires: ((emacs "26.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  TODO description
;;
;;; Code:
(require 'replace)
(require 'tree-sitter)
(require 'consult)

(defun tree-sitter-occur--find-patterns (patterns)
  "Return a list of ranges matching PATTERNS in the current buffer.

\(tree-sitter-occur--find-patterns \"\\\"break\\\" @keyword\")
 => ((414 . 420) (400 . 406) (318 . 324))"
  ;; TODO maybe accept an already-compiled query
  (let ((results nil))
    (cl-flet ((tree-sitter-query--highlight-capture
               (capture)
               (push (tsc-node-position-range (cdr capture))
                     results)))
      ;; this is pretty much the expansion of (tree-sitter-query--eval-query patterns)
      (tsc--without-restriction
        (when-let*
            ((query
              (condition-case err
                  (tsc-make-query tree-sitter-language patterns)
                ((tsc-query-invalid-node-type
                  tsc-query-invalid-field
                  tsc-query-invalid-capture)
                 (message "%s: %s" (get (car err) 'error-message) (cadr err))
                 nil)
                (tsc-query-invalid
                 (message "%s" (get (car err) 'error-message))
                 nil)))
             (root-node (tsc-root-node tree-sitter-tree))
             (captures (tsc-query-captures query root-node #'tsc--buffer-substring-no-properties)))
          (if (= (length captures) 0)
              (message "No captures found")
            (message "captures: %s" captures)
            (mapc #'tree-sitter-query--highlight-capture captures)))))
    (nreverse results)))

(defun tree-sitter-occur--find-patterns-in-buffers (patterns buffers)
  "Return an alist ((buffer . matches) ...) for each buffer in BUFFERS.
Skip a buffer if it doesn't have a `tree-sitter-language'."
  (let (results)
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (when-let* ((l tree-sitter-language) ; if there's an active lang
                    (matches (tree-sitter-occur--find-patterns patterns)))
          (push (cons buffer matches) results))))
    (nreverse results)))

(defun tree-sitter-occur (patterns buffers)
  (interactive (list (if (region-active-p)
                         (buffer-substring (region-beginning) (region-end))
                       (read-string "Query: "))
                     (cdr (consult--buffer-query-prompt
                           "Go to line"
                           '(:sort alpha :directory project)))))
  (let ((results (tree-sitter-occur--find-patterns-in-buffers patterns buffers)))
    (message "results: %s" results)
    (with-current-buffer (generate-new-buffer "*TS Occur*")
      (let (line line-number beg-marker beg-in-line end-in-line)
        (cl-loop
         for (buffer . matches) in results do
         (insert (propertize
                  (format "lines from buffer: %s\n" buffer)
                  'face list-matching-lines-buffer-name-face))
         (cl-loop
          for (beg . end) in matches do
          ;; aiming for the same text properties used by the real occur
          (with-current-buffer buffer
            (save-excursion
              (goto-char beg)
              (setq line (buffer-substring (line-beginning-position)
                                           (line-end-position))
                    beg-marker (point-marker)
                    line-number (line-number-at-pos)
                    beg-in-line (- beg (line-beginning-position))
                    end-in-line (- end (line-beginning-position)))))

          (put-text-property beg-in-line end-in-line 'face 'match line)
          (add-text-properties 0 (length line)
                               `(
                                 occur-match t
                                 follow-link t
                                 help-echo "mouse-2: go to this occurrence")
                               line)
          (insert
           (propertize (format "%7d:" line-number)
                       'occur-prefix t
                       ;; Allow insertion of text at the end
                       ;; of the prefix (for Occur Edit mode).
                       'front-sticky t
                       'rear-nonsticky t
                       'follow-link t
                       'help-echo "mouse-2: go to this occurrence")
           line
           "\n"))
         (setq prev-buffer buffer)))
      (goto-char (point-min))
      (occur-mode)
      (setq-local occur-highlight-regexp "^.*$")
      (pop-to-buffer (current-buffer)))))



(provide 'tree-sitter-occur)
;;; tree-sitter-occur.el ends here
