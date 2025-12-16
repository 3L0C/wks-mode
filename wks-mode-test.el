;;; wks-mode-test.el --- Tests for wks-mode -*- lexical-binding: t; -*-

;; Copyright © 2025, 3L0C

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Test suite for wks-mode using ERT (Emacs Regression Test).
;;
;; Run tests with: M-x ert RET t RET
;; Or from command line: emacs -batch -l ert -l wks-mode.el -l wks-mode-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'wks-mode)
(require 'imenu)

;;; Helper Functions

(defmacro wks-test-with-temp-buffer (content &rest body)
  "Create a temporary buffer with CONTENT in wks-mode and execute BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (wks-mode)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; Indentation Tests

(ert-deftest wks-mode-test-indentation-basic ()
  "Test basic indentation for nested blocks."
  (wks-test-with-temp-buffer
   "p \"Prefix\"\n{\na \"Chord\" %{{cmd}}\n}"
   (forward-line 2)
   (wks-mode-indent-line)
   (should (= (current-indentation) 4))))

(ert-deftest wks-mode-test-indentation-closing-bracket ()
  "Test that closing brackets align correctly."
  (wks-test-with-temp-buffer
   "p \"Prefix\"\n{\na \"Chord\" %{{cmd}}\n}"
   (goto-char (point-max))
   (wks-mode-indent-line)
   (should (= (current-indentation) 0))))

(ert-deftest wks-mode-test-indentation-comment ()
  "Test comment indentation follows previous line."
  (wks-test-with-temp-buffer
   "p \"Prefix\"\n{\n    a \"Chord\" %{{cmd}}\n# comment\n}"
   (search-forward "# comment")
   (beginning-of-line)
   (wks-mode-indent-line)
   (should (= (current-indentation) 4))))

(ert-deftest wks-mode-test-indentation-nested ()
  "Test nested block indentation."
  (wks-test-with-temp-buffer
   "p \"Prefix\"\n{\n    q \"Nested\"\n    {\n        r \"Deep\" %{{cmd}}\n    }\n}"
   (search-forward "r \"Deep\"")
   (beginning-of-line)
   (wks-mode-indent-line)
   (should (= (current-indentation) 8))))

;;; Syntax Highlighting Tests

(ert-deftest wks-mode-test-font-lock-hook ()
  "Test that hooks are highlighted correctly."
  (wks-test-with-temp-buffer
   "a \"Test\" ^before +keep %{{cmd}}"
   (font-lock-ensure)
   (search-forward "before")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-keyword-face))))

(ert-deftest wks-mode-test-font-lock-flag ()
  "Test that flags are highlighted correctly."
  (wks-test-with-temp-buffer
   "a \"Test\" +keep %{{cmd}}"
   (font-lock-ensure)
   (search-forward "keep")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-keyword-face))))

(ert-deftest wks-mode-test-font-lock-builtin-interpolation ()
  "Test that builtin interpolations have delimiters and content with different faces."
  (wks-test-with-temp-buffer
   "a \"Test %(key)\" %{{echo %(desc)}}"
   (font-lock-ensure)
   (search-forward "%(key)")
   ;; Check %( has builtin face
   (let ((paren-face (get-text-property (match-beginning 0) 'face)))
     (should (or (eq paren-face font-lock-builtin-face)
                 (and (listp paren-face) (memq font-lock-builtin-face paren-face)))))
   ;; Check 'key' has constant face (position after "%(")
   (let ((content-face (get-text-property (+ (match-beginning 0) 2) 'face)))
     (should (or (eq content-face font-lock-constant-face)
                 (and (listp content-face) (memq font-lock-constant-face content-face)))))
   ;; Check ) has builtin face
   (let ((close-face (get-text-property (1- (match-end 0)) 'face)))
     (should (or (eq close-face font-lock-builtin-face)
                 (and (listp close-face) (memq font-lock-builtin-face close-face)))))))

(ert-deftest wks-mode-test-font-lock-user-var-interpolation ()
  "Test that user variable interpolations have delimiters and content with different faces."
  (wks-test-with-temp-buffer
   ":var \"myvar\" \"value\"\na \"Test %(myvar)\" %{{cmd}}"
   (font-lock-ensure)
   (search-forward "%(myvar)")
   ;; Check %( has builtin face
   (let ((paren-face (get-text-property (match-beginning 0) 'face)))
     (should (or (eq paren-face font-lock-builtin-face)
                 (and (listp paren-face) (memq font-lock-builtin-face paren-face)))))
   ;; Check 'myvar' has variable-name face (position after "%(")
   (let ((content-face (get-text-property (+ (match-beginning 0) 2) 'face)))
     (should (or (eq content-face font-lock-variable-name-face)
                 (and (listp content-face) (memq font-lock-variable-name-face content-face)))))
   ;; Check ) has builtin face
   (let ((close-face (get-text-property (1- (match-end 0)) 'face)))
     (should (or (eq close-face font-lock-builtin-face)
                 (and (listp close-face) (memq font-lock-builtin-face close-face)))))))

(ert-deftest wks-mode-test-font-lock-var-macro ()
  "Test that :var macro is highlighted correctly."
  (wks-test-with-temp-buffer
   ":var \"myvar\" \"value\""
   (font-lock-ensure)
   (search-forward "var")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-preprocessor-face))))

(ert-deftest wks-mode-test-font-lock-string ()
  "Test that strings are highlighted."
  (wks-test-with-temp-buffer
   "a \"Description\" %{{cmd}}"
   (font-lock-ensure)
   (search-forward "\"Description\"")
   (should (eq (get-text-property (1+ (match-beginning 0)) 'face)
               font-lock-string-face))))

(ert-deftest wks-mode-test-font-lock-command ()
  "Test that command delimiters are highlighted."
  (wks-test-with-temp-buffer
   "a \"Test\" %{{echo hello}}"
   (font-lock-ensure)
   (search-forward "%{{")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-builtin-face))))

(ert-deftest wks-mode-test-font-lock-command-double-paren ()
  "Test that %((...)) command delimiter is not treated as interpolation."
  (wks-test-with-temp-buffer
   "a \"Test\" %((echo hello))"
   (font-lock-ensure)
   ;; The %(( should be builtin face (command delimiter), not interpolation
   (search-forward "%((")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-builtin-face))
   ;; The )) should also be builtin face
   (search-forward "))")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-builtin-face))))

(ert-deftest wks-mode-test-font-lock-command-hash-inside ()
  "Test that # inside command is not treated as comment."
  (wks-test-with-temp-buffer
   "a \"Test\" %{{echo # text}}"
   (font-lock-ensure)
   ;; Force syntax-propertize to run
   (syntax-propertize (point-max))
   (font-lock-ensure)
   ;; The # inside the command should NOT have comment face
   (search-forward "#")
   (should-not (eq (get-text-property (match-beginning 0) 'face)
                   font-lock-comment-face))))

(ert-deftest wks-mode-test-font-lock-command-hash-delimiter ()
  "Test that %##cmd## delimiter works correctly."
  (wks-test-with-temp-buffer
   "a \"Test\" %##echo hello##"
   (font-lock-ensure)
   (syntax-propertize (point-max))
   (font-lock-ensure)
   ;; The %## should be builtin face
   (search-forward "%##")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-builtin-face))
   ;; The ## at end should also be builtin face
   (search-forward "##")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-builtin-face))))

(ert-deftest wks-mode-test-font-lock-chord-array ()
  "Test that chord arrays have brackets and keys with different faces."
  (wks-test-with-temp-buffer
   "[abc] \"Test %(index)\" %{{cmd}}"
   (font-lock-ensure)
   ;; Opening bracket should be builtin face (position 1)
   (should (eq (get-text-property 1 'face) font-lock-builtin-face))
   ;; Keys should be constant face (positions 2, 3, 4)
   (should (eq (get-text-property 2 'face) font-lock-constant-face))
   ;; Closing bracket should be builtin face (position 5)
   (should (eq (get-text-property 5 'face) font-lock-builtin-face))))

(ert-deftest wks-mode-test-font-lock-special-key ()
  "Test that special keys are highlighted."
  (wks-test-with-temp-buffer
   "TAB \"Tab key\" %{{cmd}}"
   (font-lock-ensure)
   (search-forward "TAB")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-constant-face))))

(ert-deftest wks-mode-test-font-lock-special-key-bs ()
  "Test that BS (Backspace) special key is highlighted."
  (wks-test-with-temp-buffer
   "BS \"Backspace\" %{{cmd}}"
   (font-lock-ensure)
   (search-forward "BS")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-constant-face))))

(ert-deftest wks-mode-test-font-lock-color-aliases ()
  "Test that color alias macros are highlighted."
  (wks-test-with-temp-buffer
   ":fg-color \"#ffffff\"\n:bg-color \"#000000\"\n:bd-color \"#333333\""
   (font-lock-ensure)
   (search-forward "fg-color")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-preprocessor-face))
   (search-forward "bg-color")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-preprocessor-face))
   (search-forward "bd-color")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-preprocessor-face))))

(ert-deftest wks-mode-test-font-lock-keep-delay ()
  "Test that :keep-delay macro is highlighted."
  (wks-test-with-temp-buffer
   ":keep-delay 100"
   (font-lock-ensure)
   (search-forward "keep-delay")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-preprocessor-face))))

(ert-deftest wks-mode-test-font-lock-unsorted ()
  "Test that :unsorted switch macro is highlighted."
  (wks-test-with-temp-buffer
   ":unsorted\na \"Test\" %{{cmd}}"
   (font-lock-ensure)
   (search-forward "unsorted")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-preprocessor-face))))

(ert-deftest wks-mode-test-font-lock-goto ()
  "Test that @goto meta command is highlighted (only 'goto', not '@')."
  (wks-test-with-temp-buffer
   "g \"Goto\" @goto \"w m\""
   (font-lock-ensure)
   (search-forward "@goto")
   ;; After search, point is at position 15 (after @goto)
   ;; '@' is at position 10, 'g' in 'goto' is at position 11
   ;; Check that 'goto' is highlighted as keyword
   (let ((goto-face (get-text-property (- (point) 4) 'face)))  ; 'g' at pos 11
     (should (or (eq goto-face font-lock-keyword-face)
                 (and (listp goto-face) (memq font-lock-keyword-face goto-face)))))
   ;; Check that '@' is NOT highlighted as keyword
   (let ((at-face (get-text-property (- (point) 5) 'face)))  ; '@' at pos 10
     (should-not (eq at-face font-lock-keyword-face)))))

(ert-deftest wks-mode-test-font-lock-modifier-implicit-array ()
  "Test that modifier+implicit array (C-..., M-...) is highlighted."
  (wks-test-with-temp-buffer
   "C-... \"Ctrl+%(index+1)\" %{{cmd}}"
   (font-lock-ensure)
   (search-forward "C-...")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-keyword-face))))

(ert-deftest wks-mode-test-font-lock-chord-expression ()
  "Test that chord expressions inside arrays are highlighted."
  (wks-test-with-temp-buffer
   "[\n    (b \"Brave\" +keep %{{brave}})\n    x\n]"
   (font-lock-ensure)
   (search-forward "(b")
   (should (eq (get-text-property (1+ (match-beginning 0)) 'face)
               font-lock-constant-face))))

(ert-deftest wks-mode-test-font-lock-bare-key-in-array ()
  "Test that bare keys inside multi-line arrays are highlighted."
  (wks-test-with-temp-buffer
   "[\n    (1 \"Terminal 1\" %{{kitty}})\n    3\n    4\n] \"desc\" %{{cmd}}"
   (font-lock-ensure)
   ;; Find the bare key '3'
   (search-forward "3")
   (should (eq (get-text-property (1- (point)) 'face)
               font-lock-constant-face))
   ;; Find the bare key '4'
   (search-forward "4")
   (should (eq (get-text-property (1- (point)) 'face)
               font-lock-constant-face))))

(ert-deftest wks-mode-test-font-lock-modifier-unified ()
  "Test that C-a has unified highlighting (both C- and a as constant)."
  (wks-test-with-temp-buffer
   "C-a \"Ctrl+A\" %{{cmd}}"
   (font-lock-ensure)
   ;; Both C- and a should be constant face as one unit
   (should (eq (get-text-property 1 'face) font-lock-constant-face))  ; C
   (should (eq (get-text-property 2 'face) font-lock-constant-face))  ; -
   (should (eq (get-text-property 3 'face) font-lock-constant-face)))) ; a

(ert-deftest wks-mode-test-font-lock-escaped-trigger-key ()
  "Test that escaped chars as trigger keys are highlighted as constants only."
  (wks-test-with-temp-buffer
   "\\# \"Hash\" %{{cmd}}"
   (font-lock-ensure)
   ;; The escaped character \# should have ONLY constant face (no warning face)
   (should (eq (get-text-property 1 'face) font-lock-constant-face))
   (should (eq (get-text-property 2 'face) font-lock-constant-face))))

(ert-deftest wks-mode-test-font-lock-escaped-bracket-key ()
  "Test that escaped bracket as trigger key is highlighted as constant only."
  (wks-test-with-temp-buffer
   "\\[ \"Bracket\" %{{cmd}}"
   (font-lock-ensure)
   (should (eq (get-text-property 1 'face) font-lock-constant-face))))

(ert-deftest wks-mode-test-ignore-sort-not-highlighted ()
  "Test that +ignore-sort is NOT highlighted as a valid flag."
  (wks-test-with-temp-buffer
   "a \"Test\" +ignore-sort %{{cmd}}"
   (font-lock-ensure)
   (search-forward "ignore-sort")
   ;; Should not be highlighted as keyword-face since it's removed from spec
   (should-not (eq (get-text-property (match-beginning 0) 'face)
                   font-lock-keyword-face))))

;;; Interactive Command Tests

(ert-deftest wks-mode-test-insert-chord ()
  "Test that wks-insert-chord inserts correct template."
  (wks-test-with-temp-buffer
   ""
   (wks-insert-chord)
   (should (string= (buffer-string) "KEY \"Description\" %{{command}}"))))

(ert-deftest wks-mode-test-insert-prefix ()
  "Test that wks-insert-prefix inserts correct template."
  (wks-test-with-temp-buffer
   ""
   (wks-insert-prefix)
   (should (string-match-p "KEY \"Prefix\"" (buffer-string)))
   (should (string-match-p "{" (buffer-string)))
   (should (string-match-p "}" (buffer-string)))))

;;; Completion Tests

(ert-deftest wks-mode-test-completion-after-colon ()
  "Test completion after : suggests macros."
  (wks-test-with-temp-buffer
   ":v"
   (goto-char (point-max))
   (let* ((result (wks-completion-at-point))
          (candidates (nth 2 result)))
     (should (member "var" candidates))
     (should (member "include" candidates)))))

(ert-deftest wks-mode-test-completion-after-plus ()
  "Test completion after + suggests flags."
  (wks-test-with-temp-buffer
   "+keep"
   (goto-char (1+ (point-min)))  ; Position after the +
   (let* ((result (wks-completion-at-point))
          (candidates (nth 2 result)))
     (should (member "keep" candidates))
     (should (member "close" candidates)))))

(ert-deftest wks-mode-test-completion-after-caret ()
  "Test completion after ^ suggests hooks."
  (wks-test-with-temp-buffer
   "^b"
   (goto-char (point-max))
   (let* ((result (wks-completion-at-point))
          (candidates (nth 2 result)))
     (should (member "before" candidates))
     (should (member "after" candidates)))))

(ert-deftest wks-mode-test-completion-interpolation ()
  "Test completion after %( suggests interpolations."
  (wks-test-with-temp-buffer
   "%(k"
   (goto-char (point-max))
   (let* ((result (wks-completion-at-point))
          (candidates (nth 2 result)))
     (should (member "key" candidates))
     (should (member "index" candidates)))))

(ert-deftest wks-mode-test-completion-after-at ()
  "Test completion after @ suggests meta commands."
  (with-temp-buffer
    (wks-mode)
    (insert "a @goto")  ; Add prefix so there's content before @
    (goto-char 5)  ; Position after @g (at position 5: "a @g|oto")
    (let* ((result (wks-completion-at-point))
           (candidates (nth 2 result)))
      (should (member "goto" candidates)))))

(ert-deftest wks-mode-test-completion-new-macros ()
  "Test that new macros appear in completion."
  (wks-test-with-temp-buffer
   ":u"
   (goto-char (point-max))
   (let* ((result (wks-completion-at-point))
          (candidates (nth 2 result)))
     (should (member "unsorted" candidates))
     (should (member "fg-color" candidates))
     (should (member "keep-delay" candidates)))))

(ert-deftest wks-mode-test-completion-flags-updated ()
  "Test that flag completion has title, wrap, unwrap but not ignore-sort."
  (wks-test-with-temp-buffer
   "+t"
   (goto-char (point-max))
   (let* ((result (wks-completion-at-point))
          (candidates (nth 2 result)))
     (should (member "title" candidates))
     (should (member "wrap" candidates))
     (should (member "unwrap" candidates))
     (should-not (member "ignore-sort" candidates)))))

;;; Imenu Tests

(ert-deftest wks-mode-test-imenu-chord ()
  "Test that Imenu finds chords."
  (wks-test-with-temp-buffer
   "a \"Chord\" %{{cmd}}\nb \"Another\" %{{cmd}}"
   (let ((index (imenu--make-index-alist t)))
     (should (assoc "Chords" index)))))

(ert-deftest wks-mode-test-imenu-prefix ()
  "Test that Imenu finds prefixes."
  (wks-test-with-temp-buffer
   "p \"Prefix\"\n{\n}"
   (let ((index (imenu--make-index-alist t)))
     (should (assoc "Prefixes" index)))))

(ert-deftest wks-mode-test-imenu-variable ()
  "Test that Imenu finds variables."
  (wks-test-with-temp-buffer
   ":var \"myvar\" \"value\""
   (let ((index (imenu--make-index-alist t)))
     (should (assoc "Variables" index)))))

;;; Syntax Table Tests

(ert-deftest wks-mode-test-comment-syntax ()
  "Test that # starts a comment."
  (wks-test-with-temp-buffer
   "# This is a comment"
   (goto-char (point-min))
   (should (eq (syntax-class (syntax-after (point))) 11)))) ; 11 is comment-start

(ert-deftest wks-mode-test-string-syntax ()
  "Test that quotes delimit strings."
  (wks-test-with-temp-buffer
   "\"test string\""
   (goto-char (1+ (point-min)))
   (should (nth 3 (syntax-ppss))))) ; In a string

(ert-deftest wks-mode-test-comment-highlighting ()
  "Test that syntax elements in comments have comment face dominant."
  (wks-test-with-temp-buffer
   "# Comment with :var %(key) +flag ^hook %{{cmd}}\n"
   (font-lock-ensure)
   ;; Check that comment face is present (and typically first in list)
   (goto-char 3) ; Inside comment after "# "
   (let ((face (get-text-property (point) 'face)))
     (should (or (eq face 'font-lock-comment-face)
                 (and (listp face) (memq 'font-lock-comment-face face)))))
   ;; Check at :var position
   (search-forward ":var")
   (let ((face (get-text-property (1- (point)) 'face)))
     (should (or (eq face 'font-lock-comment-face)
                 (and (listp face) (memq 'font-lock-comment-face face)))))
   ;; Check at interpolation
   (search-forward "%(key)")
   (let ((face (get-text-property (match-beginning 0) 'face)))
     (should (or (eq face 'font-lock-comment-face)
                 (and (listp face)
                      (memq 'font-lock-comment-face face)
                      ;; Comment face should be first (dominant)
                      (eq (car face) 'font-lock-comment-face)))))))

;;; Customization Tests

(ert-deftest wks-mode-test-indent-offset-customization ()
  "Test that indent offset can be customized."
  (let ((wks-indent-offset 2))
    (wks-test-with-temp-buffer
     "p \"Prefix\"\n{\na \"Chord\" %{{cmd}}\n}"
     (forward-line 2)
     (wks-mode-indent-line)
     (should (= (current-indentation) 2)))))

;;; Edge Case Tests

(ert-deftest wks-mode-test-empty-buffer ()
  "Test that mode works with empty buffer."
  (wks-test-with-temp-buffer
   ""
   (should (eq major-mode 'wks-mode))))

(ert-deftest wks-mode-test-multiline-string ()
  "Test handling of strings (they should not be multiline in wks)."
  (wks-test-with-temp-buffer
   "a \"Description\n\" %{{cmd}}"
   (should (eq major-mode 'wks-mode))))

(ert-deftest wks-mode-test-escaped-quote ()
  "Test that escaped quotes in strings work."
  (wks-test-with-temp-buffer
   "a \"Test \\\" quote\" %{{cmd}}"
   (font-lock-ensure)
   (search-forward "quote")
   (should (nth 3 (syntax-ppss))))) ; Still in string after escaped quote

;;; Regression Tests

(ert-deftest wks-mode-test-no-catastrophic-backtracking ()
  "Test that arbitrary delimiter pattern doesn't hang on malformed input."
  (wks-test-with-temp-buffer
   "%||this is a very long line without closing delimiter and should not hang emacs or cause catastrophic backtracking"
   (font-lock-ensure)
   ;; If this test completes, backtracking is bounded
   (should t)))

(ert-deftest wks-mode-test-user-var-not-cross-newline ()
  "Test that user variable interpolation doesn't cross newlines."
  (wks-test-with-temp-buffer
   "a \"Test %(unclosed\" %{{cmd}}\nb \"Next line\" %{{cmd}}"
   (font-lock-ensure)
   ;; The unclosed interpolation should not highlight the entire rest of file
   (goto-char (point-max))
   (backward-char 5)
   (should-not (eq (get-text-property (point) 'face)
                   font-lock-variable-name-face))))

;;; Integration Tests

(ert-deftest wks-mode-test-full-file ()
  "Test a complete wks file example."
  (wks-test-with-temp-buffer
   ":var \"greeting\" \"hello\"

# Comment
p \"Prefix\" ^before
{
    a \"Chord %(greeting)\" +keep %{{echo %(desc)}}
    [xyz] \"Array %(index+1)\" %{{cmd}}
}

TAB \"Tab key\" %{{special}}"
   (font-lock-ensure)
   ;; Just ensure no errors occur
   (should (eq major-mode 'wks-mode))))

(provide 'wks-mode-test)

;;; wks-mode-test.el ends here
