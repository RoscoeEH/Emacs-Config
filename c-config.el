;;; c-config.el

(add-hook 'c++-mode-hook #'flycheck-mode)

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)


;;; c-config.el
