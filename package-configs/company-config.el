;; company-config.el starts

;; Enable company mode globally
(add-hook 'after-init-hook 'global-company-mode)

;; Company mode configuration for tab completion
(use-package company
  :ensure t
  :config
  ;; Use TAB for partial completion
  (define-key company-active-map (kbd "TAB") 'company-complete-common)
  (define-key company-active-map [tab] 'company-complete-common)
  
  ;; Make RET just insert a newline, not complete
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<return>") nil)

  
  ;; Don't automatically apply completions
  (setq company-require-match nil)
  
  ;; Change delay before showing completions ;impacts performance
  (setq company-idle-delay 0.2)
  
  ;; Allow partial completion
  (setq company-completion-finish-function nil)

  ;; Minimum prefix length before showing completions
  (setq company-minimum-prefix-length 2)

  ;; Only use dabbrev (buffer wods) backend
  (setq company-backends '(company-dabbrev))
  (setq-default company-backends '(company-dabbrev))

  ;; Configure dabbrev backend to strictly limit to the current buffer
  (setq company-dabbrev-other-buffers nil) ; Disable searching other buffers
  (setq company-dabbrev-code-other-buffers nil) ; Disable in code mode as well
  
  ;; Case-sensitive matching
  (setq company-dabbrev-ignore-case nil)     ; Case-sensitive search; change this to have fuzzy case searching
  (setq company-dabbrev-downcase nil)      ; Preserve the completion case
  
  ;; Treat underscores and hyphens as part of words
  (setq company-dabbrev-char-regexp "\\sw\\|_\\|-"))

;; Disable LSP completions
(setq lsp-enable-completion nil)

;; Disable Eglot completions
(setq eglot-stay-out-of '(company))


;; company-config.el ends here
