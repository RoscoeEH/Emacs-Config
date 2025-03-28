;;; vterm-config.el starts here

;; Ensure vterm is installed
(unless (package-installed-p 'vterm)
  (package-refresh-contents)
  (package-install 'vterm))

;; Load vterm
(use-package vterm
  :ensure t)

(global-set-key (kbd "C-x v") 'vterm)

;;; vterm-config.el ends here
