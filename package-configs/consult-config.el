;;; consult-config.el starts here

(use-package consult
    :ensure t
    :config
    (require 'consult-imenu) ;; Force it to be loaded
    (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "SPC i") 'consult-imenu)))

(when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

(setq merlin-command "/Users/roscoeelings-haynie/.opam/4.13.1/bin/ocamlmerlin")


(defun ocaml-imenu-setup ()
    (setq imenu-generic-expression
            '(
            ;; Match functions: after the name, require at least one non-space, non-= character
            ("Functions" 
                "^let[[:space:]]+\\(rec[[:space:]]+\\)?\\([a-zA-Z0-9_]+\\)[[:space:]]+\\([^ =].*\\)[[:space:]]*="
                2)
            ;; Match simple values (bindings with no parameter)
            ("Values"
                "^let[[:space:]]+\\(rec[[:space:]]+\\)?\\([a-zA-Z0-9_]+\\)[[:space:]]*=[[:space:]]*"
                2)
            ("Types" "^type[[:space:]]+\\([a-zA-Z0-9_]+\\)" 1)
            ("Modules" "^module[[:space:]]+\\([a-zA-Z0-9_]+\\)" 1)
            ("Classes" "^class[[:space:]]+\\([a-zA-Z0-9_]+\\)" 1))))



(add-hook 'tuareg-mode-hook 'ocaml-imenu-setup)

(global-set-key (kbd "M-g c") 'consult-ripgrep)

;;; consult-config.el ends here
