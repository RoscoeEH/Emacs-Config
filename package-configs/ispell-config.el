;;; ispell-config.el starts here

(setq ispell-program-name "aspell")

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode) ;; Comments & strings only
(add-hook 'markdown-mode-hook 'flyspell-mode) ;; Explicitly enable for Markdown

;; Disable annoying messages but keep flyspell active
(add-hook 'flyspell-mode-hook
          (lambda ()
            (setq flyspell-issue-message-flag nil)))

(add-hook 'before-save-hook
          (lambda ()
            (when flyspell-mode
              (flyspell-buffer))))

(define-key evil-normal-state-map (kbd "SPC c c") 'ispell-word) ;; Correct word under cursor
(define-key evil-normal-state-map (kbd "SPC c b") 'ispell-buffer) ;; Check entire buffer
(define-key evil-visual-state-map (kbd "SPC c r") 'ispell-region) ;; Check selected region


;;; ispell-config.el ends here
