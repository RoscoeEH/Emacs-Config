;;; pdf-config.el starts here

(use-package pdf-tools
:ensure t
:config
(pdf-tools-install))

(setq-default pdf-view-display-size 'fit-page) 

;;; pdf-config.el ends here
