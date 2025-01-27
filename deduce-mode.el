;;; deduce-mode.el --- sample major mode for editing deduce. -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Matei Cloteaux <matei.cloteaux@gmail.com>
;; URL: https://github.com/mateidragony/deduce-mode
;; Version: 0.0.1
;; Package-Requires ((emacs "24") (el-indent))

;; This file is NOT part of GNU Emacs

;; Copyright (c) 2024, Matei Cloteaux
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

(defvar deduce-operators nil "deduce operators")
(setq deduce-operators "->\\|++\\|/\\||\\|&\\|\\\[+\\\]\\|\\\[o\\\]\\|(=\\|<=\\|>=\\|/=\\|≠\\|⊆\\|≤\\|<\\|≥\\|∈\\|∪\\|+\\|%\\|*\\|⨄\\|-\\|∩\\|∘\\|>")

(defvar deduce-prims nil "deduce primitives")
(setq deduce-prims '("0" "true" "false" "∅" "\\[0\\]" "\\?"))

(defvar deduce-proof-hi nil "deduce proof his???")
(setq deduce-proof-hi '("transitive" "symmetric" "extensionality" "reflexive" "injective" "sorry" "help"))

(defvar deduce-types nil "deduce primitive types")
(setq deduce-types '("int" "bool" "fn" "type"))

(defvar deduce-lib-types nil "deduce library types")
(setq deduce-lib-types '("MultiSet" "Option" "Pair" "Set" "List" "Int" "Nat"))

(defvar deduce-functions nil "deduce functions")
(setq deduce-functions '("in" "and" "or" "print" "not" "some" "all"))

(defvar deduce-keywords nil "deduce keywords")
(setq deduce-keywords '("define" "function" "fun" "switch" "case" "union" "if" "then" "else" "import" "generic" "assert" "have" "λ" "@" ":" "="))

(defvar deduce-proof-keywords nil "deduce proof keywords")
(setq deduce-proof-keywords '("conclude" "suffices" "enough" "by" "rewrite" "conjunct" "induction" "where" "suppose" "\\.\\.\\." "with" "definition" "apply" "to" "cases" "obtain" "enable" "stop" "equations" "of" "arbitrary" "choose" "term" "from" "assume" "for" "recall"))

(defvar deduce-theorem-keywords nil "deduce theorem keywords")
(setq deduce-theorem-keywords '("theorem" "lemma" "proof" "end"))


(defvar deduce-fontlock nil "list for font-lock-defaults")
(setq deduce-fontlock
      (let (dprims-regex dtypes-regex dfunctions-regex dlibtypes-regex
                         dkeywords-regex dpfkeywords-regex dthmkeywords-regex)
        (setq dpfkeywords-regex (regexp-opt deduce-proof-keywords 'words))
        (setq dthmkeywords-regex (regexp-opt deduce-theorem-keywords 'words))
        (setq dprims-regex (regexp-opt deduce-prims 'words))
        (setq dtypes-regex (regexp-opt deduce-types 'words))
        (setq dlibtypes-regex (regexp-opt deduce-lib-types 'words))
        (setq dfunctions-regex (regexp-opt deduce-functions 'words))
        (setq dkeywords-regex (regexp-opt deduce-keywords 'words))

        (list
         (cons "function\\(.+?\\)\(" (list 1 'font-lock-function-name-face))
         (cons "define\\([^:]+?\\):" (list 1 'font-lock-function-name-face))
         (cons "define\\([^:]+?\\)=" (list 1 'font-lock-function-name-face))

         (cons dkeywords-regex            'font-lock-keyword-face)
         (cons dthmkeywords-regex         'font-lock-keyword-face)
         (cons dpfkeywords-regex          'font-lock-keyword-face)

         (cons dprims-regex               'font-lock-constant-face)
         (cons deduce-operators           'font-lock-constant-face)
         (cons "\\<[0-9]+\\>"             'font-lock-constant-face)

         (cons dtypes-regex               'font-lock-builtin-face)
         (cons dlibtypes-regex            'font-lock-type-face)
         (cons dfunctions-regex           'font-lock-function-name-face)
         ))
      )

(defun deduce-tab ()
  "Insert tab character for tab key"
  (interactive)
  (insert "  "))

(defun deduce-comment-syntax-table ()
  "Set local syntax table, and re-color buffer."
  (interactive)
  (let ((synTable (make-syntax-table)))
    (modify-syntax-entry ?\/ ". 12b" synTable)
    (modify-syntax-entry ?\n "> b" synTable)
    (set-syntax-table synTable)
    (font-lock-fontify-buffer)))

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
  (setq indent-line-function nil)
  (local-set-key (kbd "TAB") 'deduce-tab))

;; (setq indent-line-function 'deduce-indent-line)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pf\\'" . deduce-mode))

(provide 'deduce-mode)

;;; deduce-mode.el ends here
