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

  ;; Bind C-c b to set a bookmark
  (global-set-key (kbd "C-c b") 'bm-toggle)

  ;; Bind C-c m to go to the next bookmark
  (global-set-key (kbd "C-c .") 'bm-next)

  ;; Bind C-c n to go to the previous bookmark
  (global-set-key (kbd "C-c ,") 'bm-previous))





;;; bm-config.el ends here
