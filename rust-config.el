;;; rust-config.el starts here

(use-package flycheck-rust
  :ensure t)

(add-hook 'rust-mode-hook #'flycheck-mode)

(require 'rust-mode)
(add-hook 'rust-mode-hook
        (lambda ()
            (rust-enable-format-on-save)))  ; Optional: enable formatting on save

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)


;;; rust-config.el ends here
