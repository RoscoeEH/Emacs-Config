;;; ispell-config.el starts here

(setq ispell-program-name "aspell")
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode) ;; Comments & strings only

(add-hook 'flyspell-mode-hook
        (lambda ()
            (setq flyspell-issue-message-flag nil)
            (flyspell-mode-off))) ;; Prevent on-the-fly checks

(add-hook 'before-save-hook
        (lambda ()
            (when flyspell-mode
            (flyspell-buffer))))

(define-key evil-normal-state-map (kbd "SPC c c") 'ispell-word) ;; Correct word under the cursor
(define-key evil-normal-state-map (kbd "SPC c b") 'ispell-buffer) ;; Check entire buffer
(define-key evil-visual-state-map (kbd "SPC c r") 'ispell-region) ;; Check selected region





;;; ispell-config.el ends here
