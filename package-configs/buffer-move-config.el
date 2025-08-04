;;; Buffer-move-config.el starts here

(use-package buffer-move
  :ensure t)

(global-set-key (kbd "C-c b h")  'buf-move-left)
(global-set-key (kbd "C-c b l") 'buf-move-right)
(global-set-key (kbd "C-c b k")    'buf-move-up)
(global-set-key (kbd "C-c b j")  'buf-move-down)



;;; Buffer-move-config.el ends here
