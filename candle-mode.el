(defvar candle-mode-hook nil)

(defun sml-syms-re (syms)
  (concat "\\_<" (regexp-opt syms t) "\\_>"))

(defconst sml-module-head-syms
  '("signature" "structure" "functor" "abstraction"))

(defconst sml-=-starter-syms
  `("|" "val" "fun" "and" "datatype" "type" "abstype" "eqtype"
    . ,sml-module-head-syms)
  "Symbols that can be followed by a `='.")

(defconst sml-=-starter-re
  (concat "\\S.|\\S.\\|" (sml-syms-re (cdr sml-=-starter-syms)))
  "Symbols that can be followed by a `='.")

(defconst sml-non-nested-of-starter-re
  (sml-syms-re '("datatype" "abstype" "exception"))
  "Symbols that can introduce an `of' that shouldn't behave like a paren.")

(defconst sml-starters-syms
  (append sml-module-head-syms
	  '("abstype" "datatype" "exception" "fun"
	    "local" "infix" "infixr" "sharing" "nonfix"
	    "open" "type" "val" "and"
	    "withtype" "with"))
  "The starters of new expressions.")

(defconst sml-pipeheads
  '("|" "of" "fun" "fn" "and" "handle" "datatype" "abstype" "="
    "-" "*" "^" "/" "=" "<>" "not" "!" "(" "{" "[")
   "A `|' corresponds to one of these.")

(defconst sml-keywords
  '("abstraction" "abstype" "and" "andalso" "as" "before" "case"
    "datatype" "else" "end" "eqtype" "exception" "do" "fn"
    "fun" "functor" "handle" "if" "in" "include" "infix"
    "infixr" "let" "local" "nonfix" "o" "of" "op" "open" "orelse"
    "overload" "raise" "rec" "sharing" "sig" "signature"
    "struct" "structure" "then" "type" "val" "where" "while"
    "with" "withtype")
  "A regexp that matches any and all keywords of SML.")

(eval-and-compile
  (defconst sml-id-re "\\sw\\(?:\\sw\\|\\s_\\)*"))

(defconst sml-tyvarseq-re
  (concat "\\(?:\\(?:'+" sml-id-re "\\|(\\(?:[,' \t\n]+" sml-id-re
          "\\)+)\\)\\s-+\\)?"))

;; based out of http://xahlee.info/emacs/emacs/elisp_syntax_coloring.html
(setq candle-font-lock-keywords
      (let* (
             ;; define several category of keywords
             ;; (candle-keywords '("fun" "val" "end" "let"))
             ;; (candle-types '("int" "unit" "string"))

             ;; generate regex string for each category of keywords
             ;; (candle-keywords-regexp (regexp-opt candle-keywords 'words))
             ;; (candle-types-regexp (regexp-opt candle-types 'words))
	     (candle-sml-pipeheads-regexp (regexp-opt sml-pipeheads 'words))
	     (candle-starters-syms-regexp (regexp-opt sml-starters-syms 'words))
	     ;; (candle-non-nested-of-starter-re-regexp (regexp-opt sml-non-nested-of-starter-re 'words))
	     (candle-keywords-regexp (regexp-opt sml-keywords 'words)))

        `((,candle-keywords-regexp . 'font-lock-keyword-face)
	  (,candle-sml-pipeheads-regexp . 'font-lock-operator-face)
	  (,candle-starters-syms-regexp . 'font-lock-keyword-face)
	  ;; (,candle-non-nested-of-starter-re-regexp . 'font-lock-type-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

;;;###autoload
(define-derived-mode candle-mode prog-mode "candle mode"
  "Major mode for editing ML (Meta Language)"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((candle-font-lock-keywords))))

(defvar candle-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for the 'candle' major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sml\\'" . candle-mode))

(provide 'candle-mode)
