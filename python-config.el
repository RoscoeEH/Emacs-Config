;;; python-config.el starts here


;; Ensure Python mode specifically only uses dabbrev
(with-eval-after-load 'python
  (setq-local company-backends '(company-dabbrev)))

(add-hook 'python-mode-hook 'eglot-ensure)

;;; python-config.el ends here
