(defvar candle-mode-hook nil)

;; based out of http://xahlee.info/emacs/emacs/elisp_syntax_coloring.html
(setq candle-font-lock-keywords
      (let* (
             ;; define several category of keywords
             (candle-keywords '("fun" "val" "end" "let"))
             (candle-types '("int" "unit" "string"))

             ;; generate regex string for each category of keywords
             (candle-keywords-regexp (regexp-opt candle-keywords 'words))
             (candle-types-regexp (regexp-opt candle-types 'words)))

        `(
          (,candle-types-regexp . 'font-lock-type-face)
          (,candle-keywords-regexp . 'font-lock-keyword-face)
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
