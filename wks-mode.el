;;; wks-mode.el --- Major mode for editing wks files -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2023, 3L0C
;; Author: 3L0C ( dotbox at mailbox dot org )
;; Version: 0.0.1
;; Created: December 24 2023
;; Keywords: Configuration files
;; Homepage: https://codeberg.org/3L0C/wks-mode
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;;; License: GPLv3

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Major mode for editing wks files.
;;
;; Would not exist without the `zig-mode' package which was used
;; to get indentation working propperly. See
;;
;;     "https://github.com/ziglang/zig-mode"
;;
;; for details. Licensed under GPLv3 and all credit goes to the maintainers
;; and contributors.
;;
;;; Code:

(require 'rx)

;;; Vars

(defvar wks-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?\# "<")
    (modify-syntax-entry ?\n ">")
    (modify-syntax-entry ?\\ "\\")
    (modify-syntax-entry ?\" "\"")
    (syntax-table))
  "Syntax table for `wks-mode'.")

(defvar wks-indent-offset 4
  "Number of spaces to use for indentation in `wks-mode'.")

(defconst wks-font-lock-keywords
  `(
    ;; Commands
    (,(rx (seq (group (seq "%{{")) (group (*? anything)) (group (seq "}}"))))
     (1 font-lock-builtin-face)
     (2 'default t)
     (3 font-lock-builtin-face))
    ;; ;; Commented commands
    ;; (,(rx (group (seq "#" (zero-or-more (or (not "%")
    ;;                                         (seq not-newline))
    ;;                                     "%{{" (*? anything) "}}"))))
    ;;  (1 font-lock-comment-face t))
    ;; Hooks
    (,(rx (seq (group (seq "^"))
               (group (or "before"
                          "after"
                          "sync-before"
                          "sync-after"))))
     (1 'default)
     (2 font-lock-keyword-face))
    ;; Flags
    (,(rx (seq (group (seq ?\+))
               (group (or "keep" "close" "inherit" "execute"
                          "ignore" "unhook" "deflag"
                          "no-before" "no-after"
                          "write" "sync-command"))))
     (1 'default)
     (2 font-lock-keyword-face))
    ;; Preprocessor macros
    (,(rx (seq (group (seq ?\:)
                      (or
                       ;; Switch macros
                       "debug" "sort" "top" "bottom"
                       ;; String macros
                       (seq (or "include"
                                "fg" "fg-key" "fg-delimiter"
                                "fg-prefix" "fg-chord"
                                "bg" "bd" "shell" "font")
                            (zero-or-more space)
                            (char ?\")
                            (zero-or-more (or (seq ?\\ ?\\)
                                              (seq ?\\ ?\")
                                              (seq ?\\ (not (any ?\" ?\\)))
                                              (not (any ?\" ?\\))))
                            (char ?\"))
                       ;; [-]integer macros
                       (seq (or "menu-width" "menu-gap")
                            (one-or-more space)
                            (zero-or-one "-")
                            (one-or-more digit))
                       ;; integer macros
                       (seq (or "max-columns" "border-width" "width-padding"
                                "height-padding" "delay")
                            (one-or-more space)
                            (one-or-more digit))
                       ;; Digit[.digit] macros
                       (seq (or "border-radius")
                            (one-or-more space)
                            (one-or-more digit))
                            (zero-or-more (seq "." (one-or-more digit)))))))
     (1 font-lock-preprocessor-face t))
    ;; Interpolations
    (,(rx (group (seq "%(")
                 (seq (or "key" "index" "index+1"
                          "desc" "desc^" "desc^^"
                          "desc," "decs,,"))
                 (seq ")")))
     (1 font-lock-constant-face t))
    ;; Catch escaped special characters explicitly
    (,(rx (seq ?\\ (or ?\\ ?\[ ?\] ?\{ ?\} ?\# ?\" ?\: "^" ?\+ ?\( ?\))))
     (0 font-lock-variable-name-face t))
    ;; Delimiters
    (,(rx (or ?\{ ?\} ?\[ ?\] ?\( ?\)))
     (0 font-lock-builtin-face))
    ;; ;; Comments
    ;; (,(rx (seq (group (seq "#" (zero-or-more (not (any "\n"))) "\n"))))
    ;;  (1 font-lock-comment-face t))
    ;; Keys - Assumes a propperly formated document.
    ("." . 'font-lock-variable-name-face)
    ;; note: order above matters, because once colored, that part won't change.
    ;; in general, put longer words first
    ))

(defun wks-paren-nesting-level ()
  "Paren nesting for `wks-mode'."
  (nth 0 (syntax-ppss)))
(defun wks-currently-in-str ()
  "Check if in a string for `wks-mode'."
  (nth 3 (syntax-ppss)))
(defun wks-start-of-current-str-or-comment ()
  "Check if at start of str or comment `wks-mode'."
  (nth 8 (syntax-ppss)))

(defun wks-skip-backwards-past-whitespace-and-comments ()
  "Skip backwards for `wks-mode'."
  (while (or
          ;; If inside a comment, jump to start of comment.
          (let ((start (wks-start-of-current-str-or-comment)))
            (and start
                 (not (wks-currently-in-str))
                 (goto-char start)))
          ;; Skip backwards past whitespace and comment end delimiters.
          (/= 0 (skip-syntax-backward " #")))))

(defun wks-mode-indent-line ()
  "Handle line indentation in `wks' documents."
  ;; This was ripped from the official `zig-mode' package
  ;; along with all related variables and functions used here
  ;; and defined above. See `zig-mode' credit and licensing above.
  (interactive)
  (let ((indent-col
         (save-excursion
           (back-to-indentation)
           (let* ((paren-level (wks-paren-nesting-level))
                  (prev-block-indent-col
                   (if (<= paren-level 0) 0
                     (save-excursion
                       (while (>= (wks-paren-nesting-level) paren-level)
                         (backward-up-list)
                         (back-to-indentation))
                       (+ (current-column) wks-indent-offset)))))
             (cond
              ((looking-at "[]})]")
               (let ((matching-open-line
                      (save-excursion
                        (backward-up-list)
                        (line-number-at-pos))))
                 (if (= matching-open-line (line-number-at-pos))
                     ;; Closing bracket on the same line as opening bracket
                     (current-indentation)
                   ;; Closing bracket on a different line
                   (- prev-block-indent-col wks-indent-offset))))
              ((looking-at "#")
               ;; Comment line
               (if (bobp)
                   0
                 (save-excursion
                   (forward-line -1)
                   (current-indentation))))
              (t
               ;; Regular line
               prev-block-indent-col))))))
    (if (<= (current-column) (current-indentation))
        (indent-line-to indent-col)
      (save-excursion (indent-line-to indent-col)))))

;;; Define Major Mode

;;;###autoload
(define-derived-mode wks-mode fundamental-mode "wks"
  "Major mode for editing wks files."
  (set-syntax-table wks-mode-syntax-table)
  (setq-local font-lock-defaults '(wks-font-lock-keywords))
  (setq-local indent-line-function 'wks-mode-indent-line)
  (setq-local comment-start "#")
  (setq-local comment-end ""))

;; Associate `.wks' extension with `wks-mode'.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wks\\'" . wks-mode))

;; Add the mode to the `features' list
(provide 'wks-mode)

;;; wks-mode.el ends here
