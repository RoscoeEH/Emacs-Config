;;; pdf-config.el starts here

(use-package pdf-tools
:ensure t
:config
(pdf-tools-install))

(setq-default pdf-view-display-size 'fit-page)
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))


;;; pdf-config.el ends here
