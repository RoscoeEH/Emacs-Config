;;; latex-config.el starts here

(with-eval-after-load 'flycheck
  (flycheck-define-checker latex-latexmk
    "A LaTeX checker using latexmk."
    :command ("latexmk" "-pdf" "-interaction=nonstopmode" source)
    :error-patterns
    ((warning line-start (file-name) ":" line ": " (message) line-end)
     (error line-start (file-name) ":" line ": " (message) line-end))
    :modes (latex-mode))

  (add-to-list 'flycheck-checkers 'latex-latexmk))


;; LaTeX setup
(use-package tex
:ensure auctex)

(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "M-c c") #'TeX-command-run-all))


;;; latex-config.el ends here
