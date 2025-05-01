;;; deduce-mode.el --- sample major mode for editing deduce. -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Matei Cloteaux <matei.cloteaux@gmail.com>
;; URL: https://github.com/mateidragony/deduce-mode
;; Version: 0.0.1
;; Package-Requires ((emacs "24") (el-indent))

;; This file is NOT part of GNU Emacs

;; Copyright (c) 2025, Matei Cloteaux
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; This major mode provides syntax highlighting for deduce programs in emacs
;;
;; To enable: call `deduce-mode'.

;;; Code:

;; Require statements for deduce-mode
(require 'prog-mode)       ; For deriving from prog-mode
(require 'font-lock)       ; For syntax highlighting

;; Syntax Highlighting
(defvar deduce-operators nil "deduce operators")
(setq deduce-operators '("->" "++" "/" "|" "&" "[+]" "[o]" "(=" "<=" ">=" "/=" "≠" "⊆" "≤" "<" "≥" "∈" "∪" "+" "%" "*" "⨄" "-" "∩" "∘" ">"))

(defvar deduce-prims-syms nil "deduce primitives")
(setq deduce-prims-syms '("0" "∅" "[0]" "?"))

(defvar deduce-prims nil "deduce primitives words")
(setq deduce-prims '("true" "false" "in" "fun"))

(defvar deduce-types nil "deduce primitive types")
(setq deduce-types '("int" "bool" "fn" "type"))

(defvar deduce-lib-types nil "deduce library types")
(setq deduce-lib-types '("MultiSet" "Option" "Pair" "Set" "List" "Int" "Nat" "Pos"))

(defvar deduce-keywords nil "deduce keywords")
(setq deduce-keywords '("define" "function" "fun" "recursive" "switch" "case" "union" "if" "then" "else" "import" "generic" "assert" "associative" "have" "and" "or" "print" "private" "opaque" "not" "some" "all" "terminates" "recfun" "measure"))

(defvar deduce-proof-keywords nil "deduce proof keywords")

(setq deduce-proof-keywords '("conclude" "suffices" "show" "enough" "by" "replace" "conjunct" "induction" "where" "suppose" "with" "expand" "apply" "to" "cases" "obtain" "enable" "stop" "equations" "of" "arbitrary" "choose" "term" "from" "assume" "for" "recall" "transitive" "symmetric" "extensionality" "reflexive" "injective" "sorry" "help" "evaluate" "theorem" "lemma" "proof" "end"))


(defun deduce--word-regex (word &optional no-boundaries)
  "Construct deduce word regex"
  (if no-boundaries
      (concat "\\([ \t]+\\)?" word "\\([ \t]+\\)?")
    (concat "^\\([^ \t]*[ \t]+\\)?" word "\\([ \t:]+.*\\)?$")))

(defvar deduce-fontlock nil "list for font-lock-defaults")
(setq deduce-fontlock
      (let  ((operators-regex        (regexp-opt deduce-operators 'symbols))
	     (prims-syms-regex       (regexp-opt deduce-prims-syms 'symbols))
	     (prims-regex            (regexp-opt deduce-prims 'symbols))
	     (types-regex            (regexp-opt deduce-types 'symbols))
	     (lib-types-regex        (regexp-opt deduce-lib-types 'words))
	     (keywords-regex         (regexp-opt deduce-keywords 'symbols))
	     (proof-keywords-regex   (regexp-opt deduce-proof-keywords 'symbols)))

        (list
         (cons "function\\(.+?\\)("    (list 1 'font-lock-function-name-face))
	 (cons "recursive\\(.+?\\)("   (list 1 'font-lock-function-name-face))
	 (cons "recfun\\(.+?\\)("      (list 1 'font-lock-function-name-face))
	 (cons "fun\\(.+?\\)("         (list 1 'font-lock-function-name-face))
         (cons "define\\([^:=]+?\\):"  (list 1 'font-lock-function-name-face))
         (cons "define\\([^:=]+?\\)="  (list 1 'font-lock-function-name-face))
	 (cons "theorem\\([^:]+?\\):"  (list 1 'font-lock-function-name-face))
	 (cons "lemma\\([^:]+?\\):"    (list 1 'font-lock-function-name-face))

         (cons keywords-regex                'font-lock-keyword-face)
         (cons proof-keywords-regex          'font-lock-keyword-face)

         (cons prims-syms-regex              'font-lock-constant-face)
	 (cons prims-regex                   'font-lock-constant-face)
         (cons "\\<[0-9]+\\>"                'font-lock-constant-face)

         (cons types-regex                   'font-lock-builtin-face)
         (cons lib-types-regex               'font-lock-type-face)
         )))

(defun deduce-comment-syntax-table ()
  "Set local syntax table, and re-color buffer."
  (interactive)
  (let ((synTable (make-syntax-table)))
    (modify-syntax-entry ?\/ ". 12b" synTable)
    (modify-syntax-entry ?\n "> b" synTable)
    (set-syntax-table synTable)
    (font-lock-fontify-buffer)))


;; Indentation
(defun deduce--previous-k-non-empty-line (k)
  "Find the kth previous non-empty line"
  (if (equal 0 k) 0
    (let ((n -1))
      (save-excursion
	(beginning-of-line)
	(if (bobp) 0
	  (forward-line -1)
	  (while (and (not (bobp))
                      (string-empty-p
		       (string-trim-right
			(thing-at-point 'line t))))
	    (setq n (- n 1))
	    (forward-line -1))
	  (+ n (deduce--previous-k-non-empty-line (- k 1))))))))

(defun deduce--previous-line-match (str)
  "Find the prvious line that matches str"
  (save-excursion
    (let ((n -1))
      (forward-line -1)
      (while (and (not (bobp))
		  (not (string-match-p str (thing-at-point 'line t))))
	(setq n (- n 1))
	(forward-line -1))
      (if (and (bobp)
	       (not (string-match-p str (thing-at-point 'line t))))
	  0
	n))))

(defun deduce--line-n-away (n)
  "Go forward n lines and return the value of that line"
  (save-excursion
    (forward-line n)
    (thing-at-point 'line t)))

(defun deduce--indentation-of-line-n-away (n)
  "Go forward n lines and return the tabbing of that line"
  (save-excursion
    (forward-line n)
    (current-indentation)))

(defun deduce--count-seps (sep str)
  "Count the number of occurencses of sep in str"
  (- (length (split-string str sep)) 1))

(defun deduce--syms-at-line (syms n)
  "Count the sum of occurences of each sym in syms at line n away"
  (save-excursion
    (forward-line n)
    (beginning-of-line)
    (let ((ln (thing-at-point 'line t)))
      (apply #'+ (mapcar (lambda (x) (deduce--count-seps x ln))  syms)))))

(defun deduce--string-contains-only-chars (str allowed-chars)
  "Check if the trimmed string STR contains only characters in ALLOWED-CHARS."
  (let ((trimmed-str (string-trim str)))
    (cl-every (lambda (char) (memq char allowed-chars)) trimmed-str)))

(defun deduce--ls-string-prefix-p (line strs)
  (if (null strs) nil (or (string-prefix-p (car strs) line)  (deduce--ls-string-prefix-p line (cdr strs)))))

(defun deduce--ls-string-match-p (line strs)
  (if (null strs) nil (or (string-match-p (car strs) line)  (deduce--ls-string-match-p line (cdr strs)))))

(defun deduce--max-non-zero (numbers)
  "Return the maximum non-zero value from the given list of numbers.
   If all values are zero or the list is empty, return nil."
  (let ((filtered (seq-remove (lambda (x) (= x 0)) numbers)))
    (if filtered
        (apply #'max filtered)
      nil)))

;; TODO: by in equations
(defun deduce-calculate-indentation ()
  "Calculates indentation of current line based off of text in current line and previous line"
  (let* ((OPEN_BRACKET            '("{" "("))
	 (CLOSE_BRACKET           '("}" ")"))
	 (cur-line                 (string-trim-left (thing-at-point 'line t)))
	 (to-prev-line             (deduce--previous-k-non-empty-line 1))
	 (prev-line                (if (equal to-prev-line 0) "" (string-trim-right (deduce--line-n-away to-prev-line))))
	 (prev-indent              (if (equal to-prev-line 0) 0  (deduce--indentation-of-line-n-away to-prev-line)))
	 (prev-opens               (if (equal to-prev-line 0) 0  (deduce--syms-at-line OPEN_BRACKET to-prev-line)))
	 (prev-closes              (if (equal to-prev-line 0) 0  (deduce--syms-at-line CLOSE_BRACKET to-prev-line)))
	 (cur-opens                (deduce--syms-at-line OPEN_BRACKET  0))
	 (cur-closes               (deduce--syms-at-line CLOSE_BRACKET 0))
	 (cur-close-start          (deduce--ls-string-prefix-p cur-line CLOSE_BRACKET))
	 (prev-close-start         (deduce--ls-string-prefix-p (string-trim-left prev-line) CLOSE_BRACKET))
	 (prev-only-close-brackets (deduce--string-contains-only-chars prev-line (mapcar (lambda (x) (aref x 0)) CLOSE_BRACKET)))
	 (to-prev-prev-line        (deduce--previous-k-non-empty-line 2)) ;; used for checking equations
	 (prev-prev-line           (if (equal to-prev-prev-line 0) "" (string-trim-right (deduce--line-n-away to-prev-prev-line))))

	 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 ;; Indentation adjustment (solely based off of two previous lines)
	 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 (adjustment
	  (cond
	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tabbing rules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   ((string-match-p (deduce--word-regex "equations") prev-line)                     ;; line after equations double tabbed
	    2)
	   ((string-match-p (deduce--word-regex "\\.\\.\\.") cur-line)                      ;; ... tab to align = signs
	    0)
	   ((and (or (string-match-p (deduce--word-regex "theorem") prev-line)              ;; theorem/lemma/proof introduce new tabbing
		     (string-match-p (deduce--word-regex "lemma")   prev-line)              ;;
		     (string-match-p (deduce--word-regex "proof")   prev-line))             ;;
		 (not (string-match-p (deduce--word-regex "proof")  cur-line)))             ;;
	    1)
	   ((and (or  (string-match-p (deduce--word-regex "suffices") prev-line)
		      (string-match-p (deduce--word-regex "conclude") prev-line)
		      (string-match-p (deduce--word-regex "have") prev-line))
		 (not (string-match-p (deduce--word-regex "by" t) prev-line)))
	    1)
	   ((and (string-prefix-p "and" cur-line)
		 (string-prefix-p "by" (string-trim-left prev-line)))
	    0)
	   ((and (string-prefix-p "and" cur-line)
		 (string-match-p (deduce--word-regex "by" t) prev-line))
	    1)
	   ((string-suffix-p "=" (string-trim prev-line))                                   ;; tab after trailing =
	    1)
	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; untabbing rules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   ((or (string-match-p (deduce--word-regex "proof")   cur-line)                    ;; proof and end should tab back
		(string-match-p (deduce--word-regex "end")     cur-line))                   ;;
	    -1)
	   ((string-prefix-p "by" (string-trim-left prev-line))
	    -1)
	   ((and (string-prefix-p "and" (string-trim-left prev-line))
		 (string-match-p (deduce--word-regex "by" t) prev-prev-line))
	    -1)
	   ((string-suffix-p "=" (string-trim prev-prev-line))                              ;; untab after previous trailing =
	    -1)
	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; brackets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   ((and cur-close-start (> prev-opens prev-closes))                                ;; starts with close but more previous opens
	    0)
	   ((and cur-close-start prev-only-close-brackets)                                  ;; closes in a row on multiple lines so back 1
	    -1)
	   (cur-close-start                                                                 ;; random close so back hoever necessary
	    (- (- prev-opens prev-closes) 1))
	   ((and (equal prev-opens 0) (equal prev-closes 0))                                ;; nothing above so keep indent
	    0)
	   (prev-close-start                                                                ;; untab one less than close diff
	    (+ 1 (- prev-opens prev-closes)))
	   (t                                                                               ;; otherwise adjust by difference
	    (- prev-opens prev-closes))))

	 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 ;; Absolute indentation 
	 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 (equations-line          (deduce--previous-line-match (deduce--word-regex "equations")))
	 (equations-delimiters   '("\\.\\.\\."))
	 (equations-delim-prev    (deduce--ls-string-match-p prev-line equations-delimiters))
	 (equations-delim-cur     (deduce--ls-string-match-p cur-line  equations-delimiters))	 
	 (absolute-indent
	  (cond
	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; equations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   ((and (string-match-p (deduce--word-regex "equations") prev-prev-line)           ;; untab after first equations statement
		 (not (or (string-match-p "\\.\\.\\." cur-line)                             ;; (just adjust, no absolution)
			  (string-match-p "=" cur-line))))                                  ;;
	    (setq adjustment (- adjustment 2))
	    nil)
	   ((and (string-match-p (deduce--word-regex "equations") prev-prev-line)           ;; Jeremy style equations with first equals on new line
		 (not (string-match-p "=" prev-line))                                       ;; (just adjust, no absolution)
		 (string-match-p "=" cur-line))                                             ;;
	    (setq adjustment (+ adjustment 1))
	    nil)
	   ((and equations-delim-prev (not equations-delim-cur))                            ;; end of equations (...)
	    (deduce--indentation-of-line-n-away equations-line))
	   ((and equations-delim-prev equations-delim-cur)                                  ;; middle of equations (...)
	    (setq adjustment 0) 
	    nil)
	   (equations-delim-cur                                                             ;; start of equations (...)
	    (let ((eq-line (deduce--previous-line-match "=")))
	      (setq adjustment 0)
	      (+ (string-match-p "=" (string-trim-left (deduce--line-n-away eq-line)))     
		 -4 (deduce--indentation-of-line-n-away eq-line))))
	   (t                                                                             ;; else no absolution
	    nil)))

	 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 ;; Final indentation calculation 
	 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 (new-indent
	  (if absolute-indent
	      (+ absolute-indent (* tab-width adjustment))
	    (+ prev-indent (* tab-width adjustment)))))
    new-indent))

(defun deduce-indent-line ()
  "Indent the current line according to Deduce language rules."
  (interactive)
  (let* ((indent (max (deduce-calculate-indentation) 0))
	 (n (max (- (current-column) (current-indentation)) 0)))
    (cond
     (indent (indent-line-to indent)
	     (forward-char n))
     (t (indent-line-to 0)))))

;; Load extension
(defun deduce-load ()
  (interactive)
  (save-buffer)
  (unless (boundp 'deduce-path)
    (error "Deduce: I can't find Deduce!"))
  (let ((python-path (if (boundp 'python-shell-interpreter) python-shell-interpreter "python3")))
    (compile (format "%s %s %s" python-path deduce-path buffer-file-name))))

(defvar deduce-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-l") 'deduce-load)
    map))

;;;###autoload
(define-derived-mode deduce-mode prog-mode "Deduce"
  "Major mode for editing the Deduce proof languge
 Special commands: \\{deduce-mode-map}"
  (use-local-map deduce-mode-map)

  (setq font-lock-defaults '((deduce-fontlock)))

  (let ((synTable (make-syntax-table)))
    (modify-syntax-entry ?\/ ". 124" synTable)
    (modify-syntax-entry ?* ". 23b" synTable)
    (modify-syntax-entry ?\n ">" synTable)
    (set-syntax-table synTable)
    (font-lock-fontify-buffer))

  (setq tab-width 2)
  (setq default-tab-width 2)
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local indent-line-function #'deduce-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pf\\'"  . deduce-mode))


;; autocomplete
(defconst company-deduce-keywords
  (append deduce-keywords deduce-proof-keywords))

;; Set up company-mode if it is available
(when (featurep 'company)
  (require 'company) ; Ensure company is loaded
  ;; Define company backend
  (defun company-deduce-backend (command &optional arg &rest ignored)
    "Company backend for Deduce mode."
    (interactive (list 'interactive))
    (case command
	  (interactive (company-begin-backend 'company-deduce-backend))
	  (prefix (and (eq major-mode 'deduce-mode)
                       (company-grab-symbol)))
	  (candidates (all-completions arg deduce-keywords))
	  (meta (format "This value is named %s" arg))))

  (add-to-list 'company-backends 'company-deduce-backend)

  ;; Enable company-mode when deduce-mode is active
  (add-hook 'deduce-mode-hook 'company-mode))

(provide 'deduce-mode)

;;; deduce-mode.el ends here
