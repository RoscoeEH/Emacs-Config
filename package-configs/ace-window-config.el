;;; ace-window-config.el starts here


(unless (package-installed-p 'ace-window)
    (package-refresh-contents)
    (package-install 'ace-window))

(global-set-key (kbd "M-w") 'ace-window)

;; Set ace-window to use home row keys
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;;; ace-window-config.el ends here
