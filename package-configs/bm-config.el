;;; bm-config.el starts here

(use-package bm
  :ensure t
  :config
  ;; Persistent bookmarks between sessions
  (setq bm-repository-file (concat EMACS_PATH "bm-repository"))
  (setq bm-restore-repository-on-load t)

  ;; Save bookmarks on killing the buffer or exiting Emacs
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'bm-repository-save)

  ;; Load bookmarks when opening a file
  (add-hook 'find-file-hook #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; Fringes are supported for graphical display of bookmarks
  (setq bm-highlight-style 'bm-highlight-only-fringe)

  ;; Make sure the repository is loaded when Emacs starts
  (bm-repository-load)

  (global-set-key (kbd "C-c b t") 'bm-toggle)

  (global-set-key (kbd "C-c .") 'bm-next)

  (global-set-key (kbd "C-c ,") 'bm-previous)
  (global-set-key (kbd "C-c b r") 'bm-bookmark-regexp)
  (global-set-key (kbd "C-c b d") 'bm-remove-all-current-buffer))





;;; bm-config.el ends here
