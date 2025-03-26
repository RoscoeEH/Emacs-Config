;;; init.el starts here

;; Constants
(defconst EMACS_PATH "~/.emacs.d/" "Path to .emacs.d")
(defconst ELPA_PATH (concat EMACS_PATH "elpa") "Path to the elpa directory.")
(defconst CONFIG_PATH "~/.config/emacs/" "Path to config directory.")


;; Loads elpa for package-install
(load (concat CONFIG_PATH "elpa-setup.el"))



;; Enable company mode globally
(add-hook 'after-init-hook 'global-company-mode)

;; Company mode configuration for tab completion
(use-package company
  :ensure t
  :config
  ;; Use TAB for partial completion
  (define-key company-active-map (kbd "TAB") 'company-complete-common)
  (define-key company-active-map [tab] 'company-complete-common)
  
  ;; Don't automatically apply completions
  (setq company-require-match nil)
  
  ;; Change delay before showing completions ;impacts performance
  (setq company-idle-delay 0.2)
  
  ;; Allow partial completion
  (setq company-completion-finish-function nil)

  ;; Minimum prefix length before showing completions
  (setq company-minimum-prefix-length 2)

  ;; Only use dabbrev (buffer wods) backend
  (setq company-backends '(company-dabbrev))
  (setq-default company-backends '(company-dabbrev))

  ;; Configure dabbrev backend to strictly limit to the current buffer
  (setq company-dabbrev-other-buffers nil) ; Disable searching other buffers
  (setq company-dabbrev-code-other-buffers nil) ; Disable in code mode as well
  
  ;; Case-sensitive matching
  (setq company-dabbrev-ignore-case nil)     ; Case-sensitive search; change this to have fuzzy case searching
  (setq company-dabbrev-downcase nil)      ; Preserve the completion case
  
  ;; Treat underscores and hyphens as part of words
  (setq company-dabbrev-char-regexp "\\sw\\|_\\|-"))

;; Disable LSP completions
(setq lsp-enable-completion nil)

;; Disable Eglot completions
(setq eglot-stay-out-of '(company))

;; Disable Merlin completions
(with-eval-after-load 'merlin
  (remove-hook 'merlin-mode-hook #'company-mode)
  (setq merlin-completion-with-doc nil))

;; Ensure Python mode specifically only uses dabbrev
(with-eval-after-load 'python
  (setq-local company-backends '(company-dabbrev)))



;; Ensure vterm is installed
(unless (package-installed-p 'vterm)
  (package-refresh-contents)
  (package-install 'vterm))

;; Load vterm
(use-package vterm
  :ensure t)

(global-set-key (kbd "C-x v") 'vterm)

;; OCaml setup

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
  (setq merlin-command "ocamlmerlin"))

;; OCamlFormat integration
(use-package ocamlformat
  :ensure t
  :custom
  (ocamlformat-enable 'enable-outside-detected-project)
  :hook (tuareg-mode . (lambda ()
                         (add-hook 'before-save-hook 'ocamlformat-before-save nil t))))

;; Load OPAM environment
(when (executable-find "opam")
  (let ((opam-env (shell-command-to-string "opam env --shell=sh")))
    (dolist (env (split-string opam-env "\n"))
      (when (string-match "\\([^=]+\\)=\\(.*\\)" env)
        (setenv (match-string 1 env) (match-string 2 env))))))

;; Flycheck with Merlin
(use-package flycheck
  :ensure t
  :hook (merlin-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))  ;; Check on save or mode enable

;; For not annoying me when setting up new rust files
(defun flycheck-rust-ignore-unused-file (errors)
  "Filter out 'this file is not included anywhere in the module tree' warnings."
  (seq-remove (lambda (err)
                (string-match-p "this file is not included anywhere in the module tree" 
                                (flycheck-error-message err)))
              errors))

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-process-error-functions #'flycheck-rust-ignore-unused-file))


;; Ensure environment is set for PATH and OPAM
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-copy-env "PATH")
  (exec-path-from-shell-copy-env "OPAM_SWITCH_PREFIX"))

;; Ensure `merlin-mode` is enabled for OCaml files
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)

;; Auto-mode setup for OCaml files
(add-to-list 'auto-mode-alist '("\\.ml\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.mli\\'" . tuareg-mode))


;; Ensure Magit is installed
(unless (package-installed-p 'magit)
  (package-refresh-contents)
  (package-install 'magit))


;; Bind Magit status to a key (e.g., C-x g s)
(global-set-key (kbd "C-x g s") 'magit-status)




(unless (package-installed-p 'evil-collection)
  (package-refresh-contents)
  (package-install 'evil-collection))

(setq evil-want-keybinding nil)
(use-package evil
  :ensure t
  :init
  (evil-mode 1))  ;; Enable evil-mode globally

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))  ;; Initialize evil-collection after evil is loaded


;; Evil special keys
(with-eval-after-load 'evil
  ;; bind normal mode n to add new line
  (define-key evil-normal-state-map (kbd "n") (lambda ()
                                               (interactive)
                                               (end-of-line)
                                               (newline-and-indent)))
  (define-key evil-normal-state-map (kbd "N") (lambda ()
                                               (interactive)
                                               (beginning-of-line)
                                               (open-line 1)
                                               (indent-according-to-mode)
                                               (next-line)))
  ;; Add half-page-up/down to arrow keys in normal/visual mode
  (define-key evil-normal-state-map (kbd "<up>") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "<down>") 'evil-scroll-down)
  (define-key evil-visual-state-map (kbd "<up>") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "<down>") 'evil-scroll-down)
  ;; Bind occur to Space-f in normal and visual states
  (define-key evil-normal-state-map (kbd "SPC f") 'occur)
  (define-key evil-visual-state-map (kbd "SPC f") 'occur)
  (define-key evil-normal-state-map (kbd "SPC c c") 'ispell-word) ;; Correct word under the cursor
  (define-key evil-normal-state-map (kbd "SPC c b") 'ispell-buffer) ;; Check entire buffer
  (define-key evil-visual-state-map (kbd "SPC c r") 'ispell-region) ;; Check selected region
  (define-key evil-normal-state-map (kbd "SPC z") 'goto-last-change)) 



  (unless (package-installed-p 'ace-window)
    (package-refresh-contents)
    (package-install 'ace-window))

(global-set-key (kbd "M-w") 'ace-window)

;; Set ace-window to use home row keys
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))



(use-package evil-snipe
  :ensure t
  :after evil
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))


  ;; Use Space-s
  (with-eval-after-load 'evil-snipe
    (define-key evil-normal-state-map (kbd "SPC s") 'evil-snipe-s)
    (define-key evil-visual-state-map (kbd "SPC s") 'evil-snipe-s)
    (define-key evil-normal-state-map (kbd "SPC S") 'evil-snipe-S)
    (define-key evil-visual-state-map (kbd "SPC S") 'evil-snipe-S))

  ;; Enable wrapping
  (setq evil-snipe-scope 'whole-buffer) ;; Search the entire buffer and wrap around
  (setq evil-snipe-repeat-scope 'whole-buffer) ;; Wrapping for repeated searches







  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)



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


  ;; Enable Flycheck in programming modes
  (use-package flycheck
  :ensure t)

(use-package flycheck-rust
  :ensure t)

  ;; Set Flycheck to only check syntax on save
  (setq flycheck-check-syntax-automatically '(save))

  ;; Enable Flycheck in Rust and C++ modes
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'c++-mode-hook #'flycheck-mode)

  ;; Enable Flycheck in all programming modes
  (add-hook 'prog-mode-hook #'flycheck-mode)


(with-eval-after-load 'flycheck
  (flycheck-define-checker latex-latexmk
    "A LaTeX checker using latexmk."
    :command ("latexmk" "-pdf" "-interaction=nonstopmode" source)
    :error-patterns
    ((warning line-start (file-name) ":" line ": " (message) line-end)
     (error line-start (file-name) ":" line ": " (message) line-end))
    :modes (latex-mode))

  (add-to-list 'flycheck-checkers 'latex-latexmk))



  ;; Load goto-last-change package
  (use-package goto-last-change
    :ensure t
    :bind (("C-c z" . goto-last-change)))





  (require 'rust-mode)
  (add-hook 'rust-mode-hook
            (lambda ()
              (rust-enable-format-on-save)))  ; Optional: enable formatting on save

  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))


  ;; key-chord to bind jk to escape
  (use-package key-chord
    :ensure t
    :config
    (key-chord-mode 1))


  (key-chord-define-global "sd" 'save-buffer)





(use-package rainbow-delimiters
  :ensure t)


;; Optionally change cursor colors by Evil state if using evil-mode
(setq evil-normal-state-cursor '("hot pink" box))
(setq evil-insert-state-cursor '("green" bar))
(setq evil-visual-state-cursor '("red" box))





;; Define M-b as a prefix key
(define-prefix-command 'bookmark-prefix-map)
(global-set-key (kbd "M-b") 'bookmark-prefix-map)


(global-set-key (kbd "M-b m") 'bookmark-set)
(global-set-key (kbd "M-b j") 'bookmark-jump)
(global-set-key (kbd "M-b l") 'bookmark-bmenu-list)
(global-set-key (kbd "M-b d") 'bookmark-delete)


;; Added command for cloning a repo with magit
(global-set-key (kbd "C-x g c") 'magit-clone)



(setq ispell-program-name "aspell")  ;; Or "hunspell" or "ispell"
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


(setq magit-git-executable "/usr/bin/git")
(setq magit-credential 'osxkeychain)
(setenv "SSH_AUTH_SOCK" (getenv "SSH_AUTH_SOCK"))

;; Prevent Magit from inheriting direnv environment
(with-eval-after-load 'magit
(remove-hook 'magit-status-mode-hook #'direnv-update-environment)
(remove-hook 'magit-process-mode-hook #'direnv-update-environment))



(use-package direnv
    :ensure t
    :config
    (direnv-mode)

    ;; Refresh direnv when opening files
    (add-hook 'find-file-hook
            (lambda ()
                (when (file-exists-p (expand-file-name ".envrc" default-directory))
                (direnv-update-environment)))))


(global-set-key (kbd "M-g c") 'consult-ripgrep)


;; enables consult-ripgrep
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



;; markdown list quality changes
(use-package markdown-mode
  :ensure t)

(defun markdown-list-dwim ()
  "Continue a Markdown list item or checklist based on the current line."
  (interactive)
 (message "markdown-list-dwim called") 
    (let* ((current-line (thing-at-point 'line t))
           ;; Capture leading whitespace.
           (indent (if (string-match "^[ \t]*" current-line)
                       (match-string 0 current-line)
                     ""))
           new-prefix)
      (cond
       ;; Checklist: "- [ ]" or "- [x]"
       ((string-match "^[ \t]*-\\s-*\\[\\([xX ]\\)\\]\\s-+" current-line)
        (setq new-prefix (concat indent "- [ ] ")))
       ;; Ordered list with parenthesi
       ((string-match "^[ \t]*\\([0-9]+\\))\\s-+" current-line)
        (let ((num (string-to-number (match-string 1 current-line))))
          (setq new-prefix (concat indent (number-to-string (1+ num)) ") "))))
       ;; Ordered list with period
       ((string-match "^[ \t]*\\([0-9]+\\)\\.\\s-+" current-line)
        (let ((num (string-to-number (match-string 1 current-line))))
          (setq new-prefix (concat indent (number-to-string (1+ num)) ". "))))
       ;; Bullet list
       ((string-match "^[ \t]*\\([-*]\\)\\s-+" current-line)
        (setq new-prefix (concat indent (match-string 1 current-line) " ")))

       (t (setq new-prefix nil)))
      (newline)
      (when new-prefix
        (insert new-prefix))))


  (defun markdown-outdent ()
    "Outdent the current Markdown list item."
    (interactive)
    (when (eq major-mode 'markdown-mode)
      (let* ((current-indent (current-indentation))
             (cursor-offset (- (current-column) current-indent))
             (outdent-step 2)
             (new-indent (max 0 (- current-indent outdent-step))))
        (indent-line-to new-indent)
        (move-to-column (+ new-indent cursor-offset)))))


 (add-hook 'markdown-mode-hook
          (lambda ()
            (define-key evil-insert-state-local-map (kbd "RET") 'markdown-list-dwim)
            (define-key evil-insert-state-local-map (kbd "<backtab>") 'markdown-outdent)))


  (use-package epa-file
      :ensure nil
      :config
      (epa-file-enable)
      (setq epa-pinentry-mode 'loopback)
      (setq epa-file-select-keys nil)
      (setq epa-file-encrypt-to nil)
      (setq auto-mode-alist (append '(("\\.gpg\\'" . epa-file)) auto-mode-alist))
      (setq epa-file-cache-passphrase-for-symmetric-encryption t))


  (global-set-key (kbd "C-x C-k l") 'epa-list-keys)
  (global-set-key (kbd "C-x C-k e") 'epa-encrypt-file)
  (global-set-key (kbd "C-x C-k d") 'epa-decrypt-file)


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

  ;; imenu setup
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



  ;; LaTeX setup
  (use-package tex
    :ensure auctex)

  (use-package pdf-tools
    :ensure t
    :config
    (pdf-tools-install))

  (setq-default pdf-view-display-size 'fit-page)  ;; Fit one page at a time

  (with-eval-after-load 'latex
    (define-key LaTeX-mode-map (kbd "M-c c") #'TeX-command-run-all))


;; Load homescreen
(load (concat CONFIG_PATH "homescreen.el"))

;; Load system setup
(load (concat CONFIG_PATH "setup.el"))

;; Load custom commands that are un-associated with packages
(load (concat CONFIG_PATH "custom-commands.el"))


;;; init.el ends here
