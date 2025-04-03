;;; flycheck-config.el starts here

(use-package flycheck
  :ensure t
  :hook (merlin-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))  ;; Check on save or mode enable

;; For not annoying me when setting up new rust files
(defun flycheck-rust-ignore-unused-file (errors)
  "Filter out 'this file is not included anywhere in the module tree' warnings."
  (seq-remove (lambda (err)
                (and (flycheck-error-message err)
                     (string-match-p "this file is not included anywhere in the module tree"
                                     (flycheck-error-message err))))
              errors))

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-error-filter-functions #'flycheck-rust-ignore-unused-file))


;; Enable Flycheck in all programming modes
(add-hook 'prog-mode-hook #'flycheck-mode)


;;; flycheck-config.el ends here

