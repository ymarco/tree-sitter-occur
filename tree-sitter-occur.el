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

(defun tree-sitter-occur (patterns)
  (interactive "sQuery: ")
  (let ((buffers (list (current-buffer)))
        (output-buffer (generate-new-buffer "*TS Query Occur*")))
    (with-current-buffer output-buffer
      (dolist (buffer buffers)
        ;; aiming for the same text properties used by the real occur
        (insert (propertize
                 (format "lines from buffer: %s\n" buffer)
                 'face list-matching-lines-buffer-name-face))
        (with-current-buffer buffer
          (cl-flet ((tree-sitter-query--highlight-capture
                     (capture)
                     (let* ((beg (tsc-node-start-position (cdr capture)))
                            (end (tsc-node-end-position (cdr capture)))
                            (line (progn (goto-char beg)
                                         (buffer-substring (line-beginning-position)
                                                           (line-end-position))))
                            (beg-marker (point-marker))
                            (line-number (line-number-at-pos beg))
                            (beg-in-line (- beg (line-beginning-position)))
                            (end-in-line (- end (line-beginning-position))))
                       (put-text-property beg-in-line end-in-line 'face 'highlight line)
                       (add-text-properties 0 (length line)
                                            `(
                                              occur-match t
                                              follow-link t
                                              help-echo "mouse-2: go to this occurrence")
                                            line)
                       (with-current-buffer output-buffer
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
                          "\n")
                         (put-text-property (- (point) (length line) 9)
                                            (point) 'occur-target beg-marker)))))
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
                  (mapc #'tree-sitter-query--highlight-capture captures)))))))

      (goto-char (point-min))
      (occur-mode)
      (setq-local occur-highlight-regexp "^.*$"))
    (pop-to-buffer output-buffer)))



(provide 'tree-sitter-occur)
;;; tree-sitter-occur.el ends here
