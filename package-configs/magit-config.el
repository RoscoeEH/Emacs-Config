;; magit-config.el starts here

;; Ensure Magit is installed
(unless (package-installed-p 'magit)
  (package-refresh-contents)
  (package-install 'magit))


;; Bind Magit status to a key (e.g., C-x g s)
(global-set-key (kbd "C-x g s") 'magit-status)



;; Added command for cloning a repo with magit
(global-set-key (kbd "C-x g c") 'magit-clone)


(setq magit-git-executable "C:/Program Files/Git/bin/git.exe")
(setq magit-credential 'osxkeychain)
(setenv "SSH_AUTH_SOCK" (getenv "SSH_AUTH_SOCK"))

;; Prevent Magit from inheriting direnv environment
(with-eval-after-load 'magit
(remove-hook 'magit-status-mode-hook #'direnv-update-environment)
(remove-hook 'magit-process-mode-hook #'direnv-update-environment))


(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "M-p") 'magit-section-backward-sibling)
  (define-key magit-mode-map (kbd "M-n") 'magit-section-forward-sibling)
  (define-key magit-mode-map (kbd "^")   'magit-section-up))



;; Display a merge graph in log
(setq magit-log-arguments '("--graph" "--oneline" "--decorate" "--color"))
(custom-set-faces
 '(magit-hash ((t (:foreground "green")))))


;; magit-config.el ends here
