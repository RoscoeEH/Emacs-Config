;;; vertico-config.el starts here

(use-package vertico
:ensure t
:init
(vertico-mode))
(with-eval-after-load 'vertico
(define-key vertico-map (kbd "TAB") #'minibuffer-complete))

(setq completion-styles '(partial-completion basic substring))
(setq completion-category-overrides
      '((consult-grep (styles basic))
        (consult-ripgrep (styles basic))))


;;; vertico-config.el ends here
