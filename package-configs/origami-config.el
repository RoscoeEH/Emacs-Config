;;; origami-config.el starts here


(use-package origami
:ensure t
:hook (prog-mode . origami-mode)
:config
(setq origami-parser-alist
        '((python-mode . origami-python-parser)
        (rust-mode . origami-c-style-parser)
        (c-mode . origami-c-style-parser)
        (c++-mode . origami-c-style-parser)
        (emacs-lisp-mode . origami-lisp-parser)))

(evil-define-key 'normal origami-mode-map
    (kbd "z a") 'origami-toggle-node
    (kbd "z A") 'origami-recursively-toggle-node
    (kbd "z o") 'origami-open-all-nodes
    (kbd "z c") 'origami-close-all-nodes))

;;; origami-config.el ends here
