;;; wks-mode.el --- Major mode for editing wks files -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2023-2025, 3L0C
;; Author: 3L0C ( dotbox at mailbox dot org )
;; Version: 0.1.0
;; Package-Version: 20250110.1
;; Created: December 24 2023
;; Keywords: languages, tools, wk, which-key
;; Homepage: https://github.com/3L0C/wks-mode
;; URL: https://github.com/3L0C/wks-mode
;; Package-Requires: ((emacs "26.1"))

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
;; Major mode for editing wks (Which-Key Source) files.
;;
;; wks files define key chord mappings for the `wk` program, a Which-Key
;; implementation for X11 and Wayland.
;;
;; Features:
;; - Syntax highlighting for chords, prefixes, commands, hooks, and macros
;; - Support for :var macro and variable interpolations
;; - Automatic indentation with configurable offset
;; - Imenu support for navigating chords, prefixes, and variables
;; - Outline-mode support for folding
;; - Interactive commands for compiling and running
;; - Completion for macros, flags, hooks, and interpolations
;; - Flymake integration for real-time syntax checking
;; - Electric pair mode for auto-pairing brackets and quotes
;;
;; Installation:
;;
;; Add wks-mode.el to your load-path and add:
;;   (require 'wks-mode)
;;
;; Or with use-package:
;;   (use-package wks-mode
;;     :mode "\\.wks\\'")
;;
;; Usage:
;;
;; Open a .wks file and the mode will activate automatically.
;;
;; Key bindings:
;;   C-c C-c - Compile current file (wk --transpile)
;;   C-c C-r - Run wk with current file
;;   C-c C-w i c - Insert chord template
;;   C-c C-w i p - Insert prefix template
;;
;; Customization:
;;
;; M-x customize-group RET wks RET
;;
;; See the wk documentation for wks syntax details:
;;   https://codeberg.org/3L0C/wk
;;
;; Acknowledgments:
;;
;; Indentation code adapted from `zig-mode' (https://github.com/ziglang/zig-mode).
;; Licensed under GPLv3. Credit to the zig-mode maintainers and contributors.
;;
;;; Code:

(require 'rx)

;;; Customization

(defgroup wks nil
  "Major mode for editing wks (Which-Key Source) files."
  :group 'languages
  :prefix "wks-")

(defcustom wks-indent-offset 4
  "Number of spaces to use for indentation in `wks-mode'."
  :type 'integer
  :group 'wks
  :safe #'integerp)

(defcustom wks-command "wk"
  "Command to use for running wk."
  :type 'string
  :group 'wks
  :safe #'stringp)

;;; Syntax Table

(defvar wks-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?\" "\"" st)
    st)
  "Syntax table for `wks-mode'.")

;;; Syntax Propertize Function

(defconst wks--command-delimiters-re
  (rx "%"
      (or (seq "{{" (*? anything) "}}")
          (seq "((" (*? anything) "))")
          (seq "[[" (*? anything) "]]")
          ;; Arbitrary single-char delimiters: %||...||, %##...##, etc.
          (seq (group (any "!" "#" "$" "&" "'" "*" "+" "," "-" "."
                           "/" ":" ";" "<" "=" ">" "?" "@" "\\" "^"
                           "_" "`" "|" "~"))
               (backref 1)
               (*? anything)
               (backref 1)
               (backref 1))))
  "Regexp matching wks command delimiters.")

;;; Rx Pattern Constants (for reducing duplication in font-lock)

(defconst wks--rx-string-content
  '(zero-or-more (or (seq "\\" anything)
                     (not (any "\""))))
  "Rx pattern for string content with escape handling.
Matches any sequence of characters inside double quotes, properly
handling escaped characters like \\\" and \\\\.")

(defconst wks--rx-escapable-chars
  '(any "\\" "[" "]" "{" "}" "#" "\"" ":" "^" "+" "(" ")")
  "Rx pattern for characters that can be escaped in wks syntax.
These characters have special meaning and may need backslash escaping.")

(defun wks-syntax-propertize (start end)
  "Apply syntax properties to region from START to END.
This neutralizes comment syntax for # characters inside command delimiters,
allowing commands like %{{echo # text}} or %##cmd## to work correctly."
  (goto-char start)
  (while (re-search-forward wks--command-delimiters-re end t)
    (let ((cmd-start (match-beginning 0))
          (cmd-end (match-end 0)))
      ;; Set all # characters in this command region to punctuation syntax
      (save-excursion
        (goto-char cmd-start)
        (while (search-forward "#" cmd-end t)
          (put-text-property (1- (point)) (point)
                             'syntax-table (string-to-syntax ".")))))))

;;; Syntactic Face Function

(defun wks-syntactic-face-function (state)
  "Return syntactic face given STATE.
STATE is a `parse-partial-sexp' state, and the returned face is the face to
apply to the character at point."
  (cond
   ((nth 3 state)
    ;; In a string
    font-lock-string-face)
   ((nth 4 state)
    ;; In a comment - return comment face with high priority
    font-lock-comment-face)
   (t
    ;; Otherwise, no face
    nil)))

(defun wks--match-unless-comment (regex limit)
  "Match REGEX up to LIMIT but only if not in a comment.
This allows matching in strings and normal code, but skips comments
to prevent syntax elements from being highlighted in comments."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward regex limit t))
      (unless (nth 4 (syntax-ppss))  ; nil if not in comment
        (setq found t)))
    found))

;;; Font Lock - Helper Functions

(defun wks--make-command-pattern (open close)
  "Generate font-lock pattern for command with OPEN and CLOSE delimiters."
  `(,(rx-to-string `(seq (group ,open)
                     (group (*? (not (any "\n"))))
                     (group ,close)))
    (1 font-lock-builtin-face keep)
    (2 'default keep)
    (3 font-lock-builtin-face keep)))

(defun wks--font-lock-commands ()
  "Font-lock patterns for wks commands."
  `(
    ;; Commands with standard delimiters: %{{...}}, %((...)), %[[...]]
    ,(wks--make-command-pattern "%{{" "}}")
    ,(wks--make-command-pattern "%((" "))")
    ,(wks--make-command-pattern "%[[" "]]")
    ;; Commands with single-char arbitrary delimiter (e.g., %||...||, %##...##)
    ;; Note: Simplified to avoid catastrophic backtracking
    (,(rx (seq (group "%")
               (group (seq (group (any "!" "#" "$" "&" "'" "*" "+" "," "-" "."
                                       "/" ":" ";" "<" "=" ">" "?" "@" "\\" "^"
                                       "_" "`" "|" "~"))
                           (backref 3)))
               (group (*? (not (any "\n"))))
               (group (backref 2))))
     (1 font-lock-builtin-face keep)
     (2 font-lock-builtin-face keep)
     (4 'default keep)
     (5 font-lock-builtin-face keep))))

(defun wks--font-lock-keywords ()
  "Font-lock patterns for wks keywords (hooks and flags)."
  `(
    ;; Hooks
    (,(rx (seq (group "^")
               (group (or "before" "after" "sync-before" "sync-after"))))
     (1 'default)
     (2 font-lock-keyword-face))
    ;; Flags - ensure flag name is not followed by hyphen/alnum (to avoid matching +ignore in +ignore-sort)
    (,(rx (seq (group "+")
               (group (or "keep" "close" "inherit" "execute"
                          "ignore" "unhook" "deflag"
                          "no-before" "no-after" "write"
                          "sync-command" "title" "wrap" "unwrap"))
               (or (not (any "-" alnum)) eol)))
     (1 'default)
     (2 font-lock-keyword-face))))

(defun wks--font-lock-macros ()
  "Font-lock patterns for wks preprocessor macros."
  `(
    ;; Preprocessor switch macros (no arguments)
    (,(rx (seq ":"
               (group (or "debug" "sort" "unsorted" "top" "bottom"))
               (or space eol)))
     (1 font-lock-preprocessor-face))

    ;; Special handling for :var - highlight variable name distinctly
    (,(rx-to-string `(seq ":"
                      (group "var")
                      (one-or-more space)
                      (group (seq "\"" ,wks--rx-string-content "\""))
                      (one-or-more space)
                      (group (seq "\"" ,wks--rx-string-content "\""))))
     (1 font-lock-preprocessor-face)
     (2 font-lock-variable-name-face t)
     (3 font-lock-string-face t))

    ;; Other string macros
    (,(rx-to-string `(seq ":"
                      (group (or "include" "implicit-array-keys"
                                 "fg-key" "fg-delimiter" "fg-prefix"
                                 "fg-chord" "fg" "fg-color" "bg" "bg-color" "bd" "bd-color"
                                 "shell" "font" "title" "title-font" "fg-title"
                                 "wrap-cmd" "delimiter"))
                      (one-or-more space)
                      (group (seq "\"" ,wks--rx-string-content "\""))))
     (1 font-lock-preprocessor-face)
     (2 font-lock-string-face))

    ;; Integer macros (including negative)
    (,(rx (seq ":"
               (group (or "menu-width" "menu-gap" "table-padding"))
               (one-or-more space)
               (group (seq (zero-or-one "-") (one-or-more digit)))))
     (1 font-lock-preprocessor-face)
     (2 font-lock-constant-face))

    ;; Positive integer macros
    (,(rx (seq ":"
               (group (or "max-columns" "border-width" "width-padding"
                          "height-padding" "delay" "keep-delay"))
               (one-or-more space)
               (group (one-or-more digit))))
     (1 font-lock-preprocessor-face)
     (2 font-lock-constant-face))

    ;; Floating-point macros
    (,(rx (seq ":"
               (group "border-radius")
               (one-or-more space)
               (group (seq (one-or-more digit)
                           (zero-or-more (seq "." (one-or-more digit)))))))
     (1 font-lock-preprocessor-face)
     (2 font-lock-constant-face))))

(defun wks--font-lock-meta-commands ()
  "Font-lock patterns for wks meta commands (@ prefixed)."
  `(
    ;; @goto meta command - highlight only 'goto' as keyword (not @)
    (,(rx-to-string `(seq "@"
                      (group "goto")
                      (one-or-more space)
                      (group (seq "\"" ,wks--rx-string-content "\""))))
     (1 font-lock-keyword-face)
     (2 font-lock-string-face t))))

(defun wks--font-lock-interpolations ()
  "Font-lock patterns for wks interpolations.
NOTE: Order matters! Builtin interpolations MUST come before user-defined
variables."
  `(
    ;; Builtin interpolations - split into delimiter and content groups
    ;; Use matcher function to skip comments while highlighting in strings
    ((lambda (limit)
       (wks--match-unless-comment
        (rx (group "%(")
            (group (or "key"
                       "index+1" "index"
                       "desc^^" "desc^"
                       "desc,," "desc,"
                       "desc" "wrap_cmd"))
            (group ")"))
        limit))
     (1 font-lock-builtin-face prepend)    ; %(
     (2 font-lock-constant-face prepend)   ; content
     (3 font-lock-builtin-face prepend))   ; )

    ;; User-defined variable interpolations - catch-all for %(anything-not-builtin)
    ;; Constrained to not cross newlines to prevent runaway highlighting
    ;; Skip %(( which is the %((...)) command delimiter, not an interpolation
    ;; Use matcher function to skip comments while highlighting in strings
    ((lambda (limit)
       (let ((found nil))
         (while (and (not found)
                     (wks--match-unless-comment
                      (rx (group "%(")
                          (group (one-or-more (not (any ")" "\n"))))
                          (group ")"))
                      limit))
           ;; Skip if this is %(( - that's a command delimiter
           (unless (eq (char-after (match-end 1)) ?\()
             (setq found t)))
         found))
     (1 font-lock-builtin-face prepend)         ; %(
     (2 font-lock-variable-name-face prepend)   ; content
     (3 font-lock-builtin-face prepend))))

(defun wks--font-lock-special-keys ()
  "Font-lock patterns for special keys and chord arrays."
  `(
    ;; Chord array trigger - three dots with optional modifiers
    (,(rx (seq line-start
               (zero-or-more space)
               (group (zero-or-more (or "C-" "M-" "H-" "S-"))
                      "...")
               (one-or-more space)))
     (1 font-lock-keyword-face))

    ;; Explicit chord array - [keys] with separate highlighting for brackets and keys
    (,(rx (seq line-start
               (zero-or-more space)
               (group "[")
               (group (one-or-more (not (any "]" "\n"))))
               (group "]")
               (one-or-more space)))
     (1 font-lock-builtin-face)       ; Opening bracket
     (2 font-lock-constant-face)      ; Keys (trigger key face)
     (3 font-lock-builtin-face))      ; Closing bracket

    ;; Chord expressions inside arrays - (key "desc" ...)
    ;; Match opening paren at line start (after whitespace) inside arrays
    (,(rx (seq line-start
               (zero-or-more space)
               "("
               (group (one-or-more (not (any space "\t" "\n"))))  ; key
               (one-or-more space)
               "\""))
     (1 font-lock-constant-face))

    ;; Bare keys inside multi-line arrays - single char on its own line (indented)
    (,(rx (seq line-start
               (one-or-more space)  ; Must be indented (inside array)
               (group (not (any space "\t" "\n" "[" "]" "(" ")" "{" "}")))
               (or space eol)))     ; Followed by space or end of line
     (1 font-lock-constant-face))

    ;; Escaped characters as trigger keys
    ;; When at line start position (followed by description string)
    (,(rx-to-string `(seq line-start
                      (zero-or-more space)
                      (group (seq "\\" ,wks--rx-escapable-chars))
                      (one-or-more space)
                      "\""))
     (1 font-lock-constant-face))

    ;; Single-character trigger keys with optional modifiers (unified highlighting)
    ;; Captures modifiers + key as one unit with constant face
    (,(rx (seq line-start
               (zero-or-more space)
               ;; Capture modifiers + key as one unit
               (group (zero-or-more (or "C-" "M-" "H-" "S-"))
                      (not (any space "\t" "\n")))
               (one-or-more space)
               "\""))
     (1 font-lock-constant-face))

    ;; Special keys with optional modifiers (TAB, SPC, F1-F35, arrow keys, etc.)
    ;; Matches S-BS, C-M-TAB, etc.
    (,(rx (seq (or line-start space)
               (group (zero-or-more (or "C-" "M-" "H-" "S-"))
                      (or "TAB" "SPC" "RET" "BS" "DEL" "ESC" "Home" "End" "Begin"
                          "PgUp" "PgDown" "Left" "Right" "Up" "Down"
                          "VolDown" "VolMute" "VolUp" "Play" "Stop" "Prev" "Next"
                          (seq "F" (one-or-more digit))))
               word-boundary))
     (1 font-lock-constant-face))))

(defun wks--font-lock-strings ()
  "Font-lock patterns for strings."
  `(
    ;; Strings (descriptions and other quoted text)
    ;; Use keep to not override interpolations already highlighted
    (,(rx-to-string `(seq "\"" ,wks--rx-string-content "\""))
     (0 font-lock-string-face keep))))

(defun wks--font-lock-misc ()
  "Font-lock patterns for miscellaneous syntax."
  `(
    ;; Escaped special characters (but NOT at line start - those are trigger keys)
    ;; Use matcher function to skip comments while highlighting in strings
    ((lambda (limit)
       (let ((found nil)
             (pattern ,(rx-to-string `(seq "\\" ,wks--rx-escapable-chars))))
         (while (and (not found)
                     (wks--match-unless-comment pattern limit))
           ;; Skip if at line start (after optional whitespace) - those are trigger keys
           (unless (save-excursion
                     (goto-char (match-beginning 0))
                     (skip-chars-backward " \t")
                     (bolp))
             (setq found t)))
         found))
     (0 font-lock-warning-face prepend))

    ;; Delimiters
    (,(rx (or "{" "}" "[" "]" "(" ")"))
     (0 font-lock-builtin-face))))

;;; Font Lock Keywords - Three Levels

(defconst wks-font-lock-keywords-1
  (append
   (wks--font-lock-misc))
  "Minimal highlighting for wks mode.")

(defconst wks-font-lock-keywords-2
  (append
   (wks--font-lock-commands)
   (wks--font-lock-keywords)
   (wks--font-lock-macros)
   (wks--font-lock-meta-commands)
   (wks--font-lock-interpolations)
   (wks--font-lock-strings)
   wks-font-lock-keywords-1)
  "Medium highlighting for wks mode.")

(defconst wks-font-lock-keywords-3
  (append
   (wks--font-lock-special-keys)
   wks-font-lock-keywords-2)
  "Maximum highlighting for wks mode.")

(defvar wks-font-lock-keywords wks-font-lock-keywords-3
  "Default highlighting for wks mode.")

;;; Indentation

(defun wks-paren-nesting-level ()
  "Return the paren nesting level for `wks-mode'."
  (nth 0 (syntax-ppss)))

(defun wks-currently-in-str ()
  "Return non-nil if point is inside a string in `wks-mode'."
  (nth 3 (syntax-ppss)))

(defun wks-start-of-current-str-or-comment ()
  "Return position of start of current string or comment in `wks-mode'."
  (nth 8 (syntax-ppss)))

(defun wks-skip-backwards-past-whitespace-and-comments ()
  "Skip backwards past whitespace and comments in `wks-mode'."
  (while (or
          ;; If inside a comment, jump to start of comment.
          (let ((start (wks-start-of-current-str-or-comment)))
            (and start
                 (not (wks-currently-in-str))
                 (goto-char start)))
          ;; Skip backwards past whitespace and comment end delimiters.
          (/= 0 (skip-syntax-backward " #")))))

(defun wks-mode-indent-line ()
  "Indent current line for wks syntax.

Indentation rules:
- Base level is 0 (no indentation at top level)
- Each nested {} [] () block adds `wks-indent-offset' spaces
- Closing brackets align with the indentation of their opening bracket's line
- Comments maintain the previous line's indentation

This function is adapted from `zig-mode'."
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

;;; Imenu

(defvar wks-imenu-generic-expression
  '(("Chords" "^\\s-*\\([^[:space:]]+\\)\\s-+\"[^\"]+\"\\s-+.*%{{" 1)
    ("Prefixes" "^\\s-*\\([^[:space:]]+\\)\\s-+\"[^\"]+\"\\s-*\n?\\s-*{" 1)
    ("Variables" "^\\s-*:var\\s-+\"\\([^\"]+\\)\"" 1))
  "Imenu generic expression for wks mode.")

;;; Interactive Commands

(defun wks-insert-chord ()
  "Insert a chord template at point."
  (interactive)
  (insert "KEY \"Description\" %{{command}}")
  (backward-char 12))

(defun wks-insert-prefix ()
  "Insert a prefix template at point."
  (interactive)
  (insert "KEY \"Prefix\"\n{\n    \n}")
  (forward-line -1)
  (indent-according-to-mode))

(defun wks-compile ()
  "Compile the current wks file with wk --transpile."
  (interactive)
  (cond
   ((not buffer-file-name)
    (user-error "Buffer is not visiting a file"))
   ((not (executable-find wks-command))
    (user-error "Cannot find wk command: %s" wks-command))
   (t
    (compile (format "%s --transpile %s"
                     wks-command
                     (shell-quote-argument buffer-file-name))))))

(defun wks-run ()
  "Run wk with the current file as key chords."
  (interactive)
  (cond
   ((not buffer-file-name)
    (user-error "Buffer is not visiting a file"))
   ((not (executable-find wks-command))
    (user-error "Cannot find wk command: %s" wks-command))
   (t
    (compile (format "%s --key-chords %s"
                     wks-command
                     (shell-quote-argument buffer-file-name))))))

;;; Completion

(defconst wks--macro-names
  '("include" "var" "fg-key" "fg-delimiter" "fg-prefix" "fg-chord"
    "fg" "fg-color" "bg" "bg-color" "bd" "bd-color"
    "shell" "font" "debug" "sort" "unsorted" "top" "bottom"
    "menu-width" "menu-gap" "max-columns" "border-width"
    "width-padding" "height-padding" "delay" "keep-delay"
    "border-radius" "table-padding" "implicit-array-keys")
  "List of macro names for completion.")

(defconst wks--flag-names
  '("keep" "close" "inherit" "execute" "ignore"
    "unhook" "deflag" "no-before" "no-after" "write"
    "sync-command" "title" "wrap" "unwrap")
  "List of flag names for completion.")

(defconst wks--hook-names
  '("before" "after" "sync-before" "sync-after")
  "List of hook names for completion.")

(defconst wks--interpolation-names
  '("key" "index" "index+1" "desc" "desc^" "desc^^"
    "desc," "desc,," "wrap_cmd")
  "List of interpolation variable names for completion.")

(defconst wks--meta-names
  '("goto")
  "List of meta command names for completion.")

(defconst wks--special-keys
  '("TAB" "SPC" "RET" "BS" "DEL" "ESC" "Home" "End" "Begin"
    "PgUp" "PgDown" "Left" "Right" "Up" "Down"
    "VolDown" "VolMute" "VolUp" "Play" "Stop" "Prev" "Next"
    "F1" "F2" "F3" "F4" "F5" "F6" "F7" "F8" "F9" "F10"
    "F11" "F12" "F13" "F14" "F15" "F16" "F17" "F18" "F19" "F20"
    "F21" "F22" "F23" "F24" "F25" "F26" "F27" "F28" "F29" "F30"
    "F31" "F32" "F33" "F34" "F35")
  "List of special key names for completion.")

(defconst wks--completion-categories
  '((macro . (:annotation " Macro" :kind keyword))
    (flag . (:annotation " Flag" :kind property))
    (hook . (:annotation " Hook" :kind event))
    (interpolation . (:annotation " Var" :kind variable))
    (meta . (:annotation " Meta" :kind function))
    (special-key . (:annotation " Key" :kind constant)))
  "Metadata for completion categories.
Each entry maps a category symbol to a plist with :annotation and :kind.")

(defun wks--make-completion-table (candidates category)
  "Create completion table from CANDIDATES with CATEGORY metadata.
Returns a function suitable for use as a completion table that provides
annotation and company-kind metadata for corfu/company integration."
  (let* ((meta (alist-get category wks--completion-categories))
         (annotation (plist-get meta :annotation))
         (kind (plist-get meta :kind)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata
            (annotation-function . ,(lambda (_) annotation))
            (company-kind . ,(lambda (_) kind))
            (category . ,category))
        (complete-with-action action candidates string pred)))))

(defun wks--find-completion-bounds ()
  "Find bounds for completion, looking backward from point if needed.
Returns (START . END) or nil.
Uses `word' instead of `symbol' to avoid including prefix chars like +, :, etc."
  (or (bounds-of-thing-at-point 'word)
      ;; If at end of word, look backward
      (save-excursion
        (skip-chars-backward "[:alnum:]_-")
        (when (< (point) (save-excursion (skip-chars-forward "[:alnum:]_-") (point)))
          (let ((start (point)))
            (skip-chars-forward "[:alnum:]_-")
            (cons start (point)))))))

(defun wks-completion-at-point ()
  "Completion at point function for wks mode.
Provides context-aware completion with annotations and icons for corfu."
  (let* ((bounds (wks--find-completion-bounds))
         (start (or (car bounds) (point)))
         (end (or (cdr bounds) (point)))
         (char-before-start (char-before start))
         (char-before-point (char-before (point))))
    (cond
     ;; After : (macros)
     ((or (eq char-before-start ?:)
          (eq char-before-point ?:))
      (let ((actual-start (if (eq char-before-point ?:) (point) start)))
        (list actual-start end
              (wks--make-completion-table wks--macro-names 'macro)
              :exclusive 'no)))

     ;; After + (flags)
     ((or (eq char-before-start ?+)
          (eq char-before-point ?+))
      (let ((actual-start (if (eq char-before-point ?+) (point) start)))
        (list actual-start end
              (wks--make-completion-table wks--flag-names 'flag)
              :exclusive 'no)))

     ;; After ^ (hooks)
     ((or (eq char-before-start ?^)
          (eq char-before-point ?^))
      (let ((actual-start (if (eq char-before-point ?^) (point) start)))
        (list actual-start end
              (wks--make-completion-table wks--hook-names 'hook)
              :exclusive 'no)))

     ;; After %( (interpolations)
     ((and (> start 2)
           (save-excursion
             (goto-char start)
             (equal (buffer-substring (- (point) 2) (point)) "%(")))
      (list start end
            (wks--make-completion-table wks--interpolation-names 'interpolation)
            :exclusive 'no))

     ;; After @ (meta commands)
     ((or (eq char-before-start ?@)
          (eq char-before-point ?@))
      (let ((actual-start (if (eq char-before-point ?@) (point) start)))
        (list actual-start end
              (wks--make-completion-table wks--meta-names 'meta)
              :exclusive 'no)))

     ;; Default: special keys (when at word boundary)
     (bounds
      (let ((at-word-start (save-excursion
                             (goto-char start)
                             (or (bolp)
                                 (memq (char-before) '(?\s ?\t ?\n))))))
        (when at-word-start
          (list start end
                (wks--make-completion-table wks--special-keys 'special-key)
                :exclusive 'no))))

     (t nil))))

;;; Flymake

(defun wks-flymake (report-fn &rest _args)
  "Flymake backend for wks mode using wk --transpile.
REPORT-FN is a callback function to report diagnostics."
  (unless (executable-find wks-command)
    (error "Cannot find wk command: %s" wks-command))
  (let* ((source (current-buffer))
         (temp-file (make-nearby-temp-file "wks-flymake" nil ".wks")))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) temp-file nil 'silent))
    (make-process
     :name "wks-flymake"
     :buffer (generate-new-buffer " *wks-flymake*")
     :command (list wks-command "--transpile" temp-file)
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (unwind-protect
             (if (buffer-live-p source)
                 (with-current-buffer (process-buffer proc)
                   (goto-char (point-min))
                   (let (diags)
                     (while (search-forward-regexp
                             "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)$"
                             nil t)
                       (let ((line (string-to-number (match-string 2)))
                             (col (string-to-number (match-string 3)))
                             (msg (match-string 4)))
                         (with-current-buffer source
                           (save-excursion
                             (goto-char (point-min))
                             (forward-line (1- line))
                             (move-to-column col)
                             (push (flymake-make-diagnostic
                                    source
                                    (point)
                                    (line-end-position)
                                    :error
                                    msg)
                                   diags)))))
                     (funcall report-fn (nreverse diags))))
               (flymake-log :warning "Canceling obsolete check %s" proc))
           (kill-buffer (process-buffer proc))
           (delete-file temp-file)))))))

;;; Keymap

(defvar wks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'wks-compile)
    (define-key map (kbd "C-c C-r") #'wks-run)
    (define-key map (kbd "C-c C-w i c") #'wks-insert-chord)
    (define-key map (kbd "C-c C-w i p") #'wks-insert-prefix)
    map)
  "Keymap for `wks-mode'.")

;;; Define Major Mode

;;;###autoload
(define-derived-mode wks-mode fundamental-mode "wks"
  "Major mode for editing wks (Which-Key Source) files.

\\{wks-mode-map}"
  :syntax-table wks-mode-syntax-table
  :group 'wks

  ;; Font lock
  (setq-local font-lock-defaults
              '((wks-font-lock-keywords-1
                 wks-font-lock-keywords-2
                 wks-font-lock-keywords-3)
                nil nil nil nil
                (font-lock-syntactic-face-function . wks-syntactic-face-function)))

  ;; Syntax propertize - neutralize # inside command delimiters
  (setq-local syntax-propertize-function #'wks-syntax-propertize)

  ;; Indentation
  (setq-local indent-line-function #'wks-mode-indent-line)

  ;; Comments
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local comment-end "")
  (setq-local comment-use-syntax t)
  (setq-local comment-auto-fill-only-comments t)

  ;; Imenu
  (setq-local imenu-generic-expression wks-imenu-generic-expression)

  ;; Outline mode
  (setq-local outline-regexp "^\\s-*\\([^[:space:]]+\\)\\s-+\"[^\"]+\"\\s-*{")
  (setq-local outline-level (lambda () 1))

  ;; Completion
  (add-hook 'completion-at-point-functions #'wks-completion-at-point nil t)

  ;; Flymake
  (when (executable-find wks-command)
    (add-hook 'flymake-diagnostic-functions #'wks-flymake nil t))

  ;; Electric pairs
  (setq-local electric-pair-pairs
              '((?{ . ?})
                (?\[ . ?\])
                (?\( . ?\))
                (?\" . ?\)))))

;; Associate `.wks' extension with `wks-mode'.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wks\\'" . wks-mode))

;; Add the mode to the `features' list
(provide 'wks-mode)

;;; wks-mode.el ends here
