;; magit-config.el starts here

;; Ensure Magit is installed
(unless (package-installed-p 'magit)
  (package-refresh-contents)
  (package-install 'magit))


;; Bind Magit status to a key (e.g., C-x g s)
(global-set-key (kbd "C-x g s") 'magit-status)

;; Added command for cloning a repo with magit
(global-set-key (kbd "C-x g c") 'magit-clone)

;; Command for magit-init
(global-set-key (kbd "C-x g i") 'magit-init)


(setq magit-git-executable "/usr/bin/git")
(setenv "SSH_AUTH_SOCK" (getenv "SSH_AUTH_SOCK"))

(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "M-p") 'magit-section-backward-sibling)
  (define-key magit-mode-map (kbd "M-n") 'magit-section-forward-sibling)
  (define-key magit-mode-map (kbd "^")   'magit-section-up))


;; Prevent Magit from inheriting direnv environment
(with-eval-after-load 'magit
(remove-hook 'magit-status-mode-hook #'direnv-update-environment)
(remove-hook 'magit-process-mode-hook #'direnv-update-environment))

;; Display a merge graph in log
(setq magit-log-arguments '("--graph" "--oneline" "--decorate" "--color"))
(custom-set-faces
 '(magit-hash ((t (:foreground "green")))))


;; magit-config.el ends here
