;;; deduce-mode.el --- sample major mode for editing deduce. -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Matei Cloteaux <matei.cloteaux@gmail.com>
;; Version: 0.0.1
;; Package-Requires ((emacs "24") (parent-mode "2.0")) 

;; This file is NOT part of GNU Emacs

;;; Code: 

(defvar deduce-prims nil "deduce primitives")
(setq deduce-prims '("0" "true" "false" "∅" "[0]" "?"))

(defvar deduce-proof-hi nil "deduce proof his???")
(setq deduce-proof-hi '("transitive" "symmetric" "extensionality" "reflexive" "injective" "sorry" "help"))

(defvar deduce-types nil "deduce primitive types")
(setq deduce-types '("int" "bool" "fn" "type"))

(defvar deduce-functions nil "deduce functions")
(setq deduce-functions '("in" "and" "or" "print" "not" "some" "all"))

(defvar deduce-keywords nil "deduce keywords")
(setq deduce-keywords '("define" "function" "λ" "fun" "switch" "case" "union" "if" "then" "else" "@" ":" "=" "import" "generic" "assert" "have"))

(defvar deduce-proof-keywords nil "deduce proof keywords")
(setq deduce-proof-keywords '("conclude" "suffices" "by" "rewrite" "conjunct" "induction" "where" "suppose" "..." "with" "definition" "apply" "to" "cases" "obtain" "enable" "stop" "equations" "of" "arbitrary" "choose" "term" "from" "assume" "for"))

(defvar deduce-theorem-keywords nil "deduce theorem keywords")
(setq deduce-theorem-keywords '("theorem" "lemma" "proof" "end"))


(defvar deduce-fontlock nil "list for font-lock-defaults")
(setq deduce-fontlock
      (let (dprims-regex dpfhi-regex dtypes-regex dfunctions-regex dkeywords-regex dpfkeywords-regex dthmkeywords-regex)
            (setq dprims-regex (regexp-opt deduce-prims 'words))
            (setq dpfhi-regex  (regexp-opt deduce-proof-hi 'words))
            (setq dtypes-regex (regexp-opt deduce-types 'words))
            (setq dfunctions-regex (regexp-opt deduce-functions 'words))
            (setq dkeywords-regex (regexp-opt deduce-keywords 'words))
            (setq dpfkeywords-regex (regexp-opt deduce-proof-keywords 'words))
            (setq dthmkeywords-regex (regexp-opt deduce-theorem-keywords 'words))
	    
            (list
              (cons dkeywords-regex       'font-lock-keyword-face)
              (cons dpfkeywords-regex     'font-lock-keyword-face)
              (cons dpfhi-regex           'font-lock-constant-face)
              (cons dthmkeywords-regex    'font-lock-keyword-face)
              (cons dtypes-regex          'font-lock-type-face)
              (cons dprims-regex          'font-lock-constant-face)
              (cons dfunctions-regex      'font-lock-function-name-face)
	      (cons "->\\|++\\|/\\||\\|&\\|\\\[+\\\]\\|\\\[o\\\]\\|(=\\|<=\\|>=\\|/=\\|≠\\|⊆\\|≤\\|<\\|≥\\|∈\\|∪\\|+\\|%\\|*\\|⨄\\|-\\|∩\\|∘\\|>" 'font-lock-constant-face))))

(define-derived-mode deduce-mode haskell-mode "deduce mode"
      "Major mode for editing Deduce languge"
      (setq font-lock-defaults '((deduce-fontlock))))

(add-to-list 'auto-mode-alist '("\\.\\(pf\\|thm\\)\\'" . deduce-mode))

(provide 'deduce-mode)

;;; deduce-mode.el ends here
