;;; epa-config.el starts here

(use-package epa-file
    :ensure nil
    :config
    (epa-file-enable)
    (setq epa-pinentry-mode 'loopback)
    (setq epa-file-select-keys nil)
    (setq epa-file-encrypt-to nil)
    (setq auto-mode-alist (append '(("\\.gpg\\'" . epa-file)) auto-mode-alist))
    (setq epa-file-cache-passphrase-for-symmetric-encryption t))


(global-set-key (kbd "C-x C-k l") 'epa-list-keys)
(global-set-key (kbd "C-x C-k e") 'epa-encrypt-file)
(global-set-key (kbd "C-x C-k d") 'epa-decrypt-file)




;;; epa-config.el ends here
