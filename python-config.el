;;; python-config.el starts here


;; Ensure Python mode specifically only uses dabbrev
(with-eval-after-load 'python
  (setq-local company-backends '(company-dabbrev)))

;;; python-config.el ends here
