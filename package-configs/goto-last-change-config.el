;;; goto-last-change-config.el starts here


;; Load goto-last-change package
(use-package goto-last-change
    :ensure t
    :bind (("C-c z" . goto-last-change)))

(define-key evil-normal-state-map (kbd "SPC z") 'goto-last-change)

;;; goto-last-change-config.el ends here
