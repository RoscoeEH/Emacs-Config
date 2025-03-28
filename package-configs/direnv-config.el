;;; direnv-config.el starts here

(use-package direnv
    :ensure t
    :config
    (direnv-mode)

    ;; Refresh direnv when opening files
    (add-hook 'find-file-hook
            (lambda ()
                (when (file-exists-p (expand-file-name ".envrc" default-directory))
                (direnv-update-environment)))))


;;; direnv-config.el ends here
