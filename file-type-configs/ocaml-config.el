;;; ocaml-config.el starts here

;; Ensure environment is set for PATH and OPAM
(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-shell-name "zsh")
  :config
  (exec-path-from-shell-initialize))


;; Disable Merlin completions
(with-eval-after-load 'merlin
  (remove-hook 'merlin-mode-hook #'company-mode)
  (setq merlin-completion-with-doc nil))


(require 'opam-user-setup (concat EMACS_PATH "opam-user-setup.el"))

;; Tuareg mode for OCaml
(use-package tuareg
  :ensure t
  :mode ("\\.ml\\'" "\\.mli\\'")
  :config
  (setq tuareg-indent-align-with-first-arg t)
  (setq tuareg-match-patterns-aligned t))

;; Merlin for OCaml
(use-package merlin
  :ensure t
  :hook ((tuareg-mode . merlin-mode)
         (caml-mode . merlin-mode))
  :config
  (setq merlin-command (concat (getenv "OPAM_SWITCH_PREFIX") "/bin/ocamlmerlin")))

;; OCamlFormat integration
(use-package ocamlformat
  :ensure t
  :custom
  (ocamlformat-enable 'enable-outside-detected-project)
  :hook (tuareg-mode . (lambda ()
                         (add-hook 'before-save-hook 'ocamlformat-before-save nil t))))

;; Load OPAM environment
;; (when (executable-find "opam")
;;   (let ((opam-env (shell-command-to-string "opam env --shell=sh")))
;;     (dolist (env (split-string opam-env "\n"))
;;       (when (string-match "\\([^=]+\\)=\\(.*\\)" env)
;;         (setenv (match-string 1 env) (match-string 2 env))))))



(when (memq window-system '(mac ns x))
  (exec-path-from-shell-copy-env "PATH")
  (exec-path-from-shell-copy-env "OPAM_SWITCH_PREFIX"))

;; Ensure `merlin-mode` is enabled for OCaml files
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)

;; Auto-mode setup for OCaml files
(add-to-list 'auto-mode-alist '("\\.ml\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.mli\\'" . tuareg-mode))


(use-package opam-switch-mode
  :ensure t)
;;; ocaml-config.el ends here
