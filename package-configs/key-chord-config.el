;;; key-chord-config.el starts here

;; key-chord to bind jk to escape
(use-package key-chord
    :ensure t
    :config
    (key-chord-mode 1))


(key-chord-define-global "sd" 'save-buffer)

;;; key-chord-config.el ends here


