;;; deduce-mode.el --- sample major mode for editing deduce. -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Matei Cloteaux <matei.cloteaux@gmail.com>
;; URL: https://github.com/mateidragony/deduce-mode
;; Version: 0.0.1
;; Package-Requires ((emacs "24") (haskell-mode "1")) 

;; This file is NOT part of GNU Emacs

;; Copyright (c) 2013-2016, Matei Cloteaux
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

(require 'haskell-mode)

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
(setq deduce-proof-keywords '("conclude" "suffices" "enough" "by" "rewrite" "conjunct" "induction" "where" "suppose" "..." "with" "definition" "apply" "to" "cases" "obtain" "enable" "stop" "equations" "of" "arbitrary" "choose" "term" "from" "assume" "for"))

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

;;;###autoload
(define-derived-mode deduce-mode haskell-mode "deduce mode"
      "Major mode for editing Deduce languge"
      (setq font-lock-defaults '((deduce-fontlock))))

(provide 'deduce-mode)

;;; deduce-mode.el ends here
