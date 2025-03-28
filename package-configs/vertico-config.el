;;; vertico-config.el starts here

(use-package vertico
:ensure t
:init
(vertico-mode))
(with-eval-after-load 'vertico
(define-key vertico-map (kbd "TAB") #'minibuffer-complete))

(setq completion-styles '(partial-completion orderless basic))
(setq completion-category-overrides
      '((consult-grep (styles basic))
        (consult-ripgrep (styles basic))))


(defun find-file-without-vertico ()
  "Temporarily disable Vertico mode while invoking find-file.
This allows you to create new directories without Vertico auto-completing to an existing one."
  (interactive)
  (let ((vertico-active (bound-and-true-p vertico-mode)))
    (when vertico-active
      (vertico-mode -1))
    (unwind-protect
        (call-interactively #'find-file)
      (when vertico-active
        (vertico-mode 1)))))

(global-set-key (kbd "C-x C-f") #'find-file-without-vertico)





;;; vertico-config.el ends here
