(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e410458d3e769c33e0865971deb6e8422457fad02bf51f7862fa180ccc42c032" "0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850" default))
 '(package-selected-packages
   '(lsp-mode key-chord free-keys aggressive-indent goto-last-change flycheck bm minimap rainbow-delimiters ace-window evil-collection evil magit vterm company ## rust-mode modus-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(load-theme 'modus-vivendi)
(setq mac-option-modifier 'meta
      mac-option-key-is-meta t
      mac-command-key-is-meta nil)

;; Performance changes

;; Alter jit-lock time 
(setq jit-lock-defer-time 0.2)

;; Diable cursor blinking
(blink-cursor-mode -1)

;; Diable auto-updating of files based on the disk as it is not relevant to my use case
(global-auto-revert-mode -1)

;; diable garbage collection on start-up
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))



;; Enable company mode globally
(add-hook 'after-init-hook 'global-company-mode)


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Ensure vterm is installed
(unless (package-installed-p 'vterm)
  (package-refresh-contents)
  (package-install 'vterm))

;; Load vterm
(require 'vterm)
(global-set-key (kbd "C-x v") 'vterm)

;; OCaml setup

(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")

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
  :ensure nil  ;; Not on MELPA; installed via OPAM
  :if (executable-find "ocamlformat")
  :hook (tuareg-mode . (lambda ()
                         (when (executable-find "ocamlformat")
                           (add-hook 'before-save-hook #'ocamlformat-before-save nil t))))
  :custom
  (ocamlformat-enable 'enable-outside-detected-project))

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


(tool-bar-mode -1)


(unless (package-installed-p 'evil-collection)
  (package-refresh-contents)
  (package-install 'evil-collection))

(setq evil-want-keybinding nil)
(require 'evil)
(require 'evil-collection)
(evil-mode 1)
(evil-collection-init)

(add-hook 'prog-mode-hook 'display-line-numbers-mode) ;; Enable line numbers
(setq display-line-numbers-type 'relative) ;; Set relative line numbers

;; Tabbing sections in visual mode
(define-key evil-visual-state-map (kbd "TAB") 'maintain/evil-shift-right-visual)
(define-key evil-visual-state-map (kbd "<backtab>") 'maintain/evil-shift-left-visual)

(defun maintain/evil-shift-left-visual ()
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun maintain/evil-shift-right-visual ()
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))



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




(require 'evil-snipe)
(evil-snipe-mode 1)
(evil-snipe-override-mode 1)

;; Use Space-s
(with-eval-after-load 'evil-snipe
  (define-key evil-normal-state-map (kbd "SPC s") 'evil-snipe-s)
  (define-key evil-visual-state-map (kbd "SPC s") 'evil-snipe-s)
  (define-key evil-normal-state-map (kbd "SPC S") 'evil-snipe-S)
  (define-key evil-visual-state-map (kbd "SPC S") 'evil-snipe-S))

;; Enable wrapping
(setq evil-snipe-scope 'whole-buffer) ;; Search the entire buffer and wrap around
(setq evil-snipe-repeat-scope 'whole-buffer) ;; Wrapping for repeated searches





(defun open-init-file ()
  "Open the Emacs init file."
  (interactive)
  (find-file "~/.config/emacs/init.el"))


;; You can bind this function to a key for quick access
(global-set-key (kbd "C-c I") 'open-init-file)


(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)



(require 'bm)

;; Enable `bm` in all buffers
(global-set-key (kbd "<f2>") 'bm-toggle)    ;; Toggle bookmark with F2
(global-set-key (kbd "<f3>") 'bm-next)      ;; Go to next bookmark with F3
(global-set-key (kbd "<f4>") 'bm-previous)  ;; Go to previous bookmark with F4

;; Persistent bookmarks between sessions
(setq bm-repository-file "~/.emacs.d/bm-repository")
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
(global-set-key (kbd "C-c ,") 'bm-previous)

;; Enable Flycheck in programming modes
(require 'flycheck)
(require 'flycheck-rust)

;; Set Flycheck to only check syntax on save
(setq flycheck-check-syntax-automatically '(save))

;; Enable Flycheck in Rust and C++ modes
(add-hook 'rust-mode-hook #'flycheck-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'c++-mode-hook #'flycheck-mode)

;; Enable Flycheck in all programming modes
(add-hook 'prog-mode-hook #'flycheck-mode)




;; Load goto-last-change package
(use-package goto-last-change
  :ensure t
  :bind (("C-c z" . goto-last-change)))



(global-set-key (kbd "M-<up>") 'scroll-down-line)
(global-set-key (kbd "M-<down>") 'scroll-up-line)


(require 'rust-mode)
(add-hook 'rust-mode-hook
          (lambda ()
            (rust-enable-format-on-save)))  ; Optional: enable formatting on save

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Custom compile command
(defun improved-compile-command ()
  "Set `compile-command` dynamically based on the major mode."
  (interactive)
  (let ((cmd (cond
              ((eq major-mode 'python-mode)
               "python3 ")
              ((eq major-mode 'rust-mode)
               "cargo build")
              ((eq major-mode 'tuareg-mode)
               "dune build")
              (t "make k"))))
    (setq-local compile-command cmd)))

;; Remove from find-file-hook as it might be too early
(remove-hook 'find-file-hook #'improved-compile-command)

;; Add only to tuareg-mode-hook
(add-hook 'tuareg-mode-hook #'improved-compile-command)

;; Set the default compile-command to nil to ensure our function takes effect
(setq-default compile-command nil)

;; Unbind M-c from capitalize-word
(global-unset-key (kbd "M-c"))

;; Skip eol chars on evil-end-of-line
(defun evil-end-of-line-non-whitespace ()
  "Move to the last character before the newline, ignoring trailing whitespace."
  (interactive)
  (move-end-of-line 1)
  (skip-chars-backward " \t"))

(define-key evil-normal-state-map (kbd "$") 'evil-end-of-line-non-whitespace)
(define-key evil-visual-state-map (kbd "$") 'evil-end-of-line-non-whitespace)

;; Define M-c as a prefix key
(define-prefix-command 'compile-prefix-map)
(global-set-key (kbd "M-c") 'compile-prefix-map)

;; new commands for compile and recompile
(global-set-key (kbd "M-c c") 'compile)
(global-set-key (kbd "M-c m") 'recompile)


(global-set-key (kbd "M-c n") 'next-error)
(global-set-key (kbd "M-c b") 'previous-error)


(defun toggle-fullscreen ()
  "Toggle fullscreen mode on macOS."
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

;; Bind Command-Control-f to toggle fullscreen
(global-set-key (kbd "s-C-f") 'toggle-fullscreen)


;; key-chord to bind jk to escape
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1))

(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)


;; xref find def
(define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
(define-key evil-motion-state-map (kbd "M-.") 'xref-find-definitions)


(electric-pair-mode 1)


(require 'rainbow-delimiters)


;; Optionally change cursor colors by Evil state if using evil-mode
(setq evil-normal-state-cursor '("hot pink" box))
(setq evil-insert-state-cursor '("green" bar))
(setq evil-visual-state-cursor '("red" box))



(defun convert-tabs-to-spaces ()
  "Convert all tabs to spaces."
  (untabify (point-min) (point-max)))

(add-hook 'before-save-hook 'convert-tabs-to-spaces)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)  ;; Set the number of spaces for a tab, change 4 to your preference


(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))


;; Define M-b as a prefix key
(define-prefix-command 'bookmark-prefix-map)
(global-set-key (kbd "M-b") 'bookmark-prefix-map)


(global-set-key (kbd "M-b m") 'bookmark-set)
(global-set-key (kbd "M-b j") 'bookmark-jump)
(global-set-key (kbd "M-b l") 'bookmark-bmenu-list)
(global-set-key (kbd "M-b d") 'bookmark-delete)


;; Added command for cloning a repo with magit
(global-set-key (kbd "C-x g c") 'magit-clone)




;; Enable eglot for supported languages
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)


(evil-define-key 'normal 'global (kbd "SPC d") 'xref-find-definitions-other-window)

;; Remove scroll bars
(scroll-bar-mode -1)


(fset 'yes-or-no-p 'y-or-n-p)

(setq use-short-answers t)


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


(setq magit-git-executable "/usr/bin/git")  ; or the correct path to your git binary
(setq magit-credential 'osxkeychain)
(setenv "SSH_AUTH_SOCK" (getenv "SSH_AUTH_SOCK"))



;; Function to wrap selected text in parentheses
(defun wrap-with-parens ()
  "Wrap selected text with parentheses."
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert ")")
    (goto-char start)
    (insert "(")
    (evil-normal-state)))

;; Bind it to SPC ( in visual mode
(define-key evil-visual-state-map (kbd "SPC (") 'wrap-with-parens)

;; Function to wrap selected text in square brackets
(defun wrap-with-square-brackets ()
  "Wrap selected text with square brackets."
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert "]")
    (goto-char start)
    (insert "[")
    (evil-normal-state)))

;; Bind it to SPC [ in visual mode
(define-key evil-visual-state-map (kbd "SPC [") 'wrap-with-square-brackets)

;; Function to wrap selected text in curly braces
(defun wrap-with-curly-braces ()
  "Wrap selected text with curly braces."
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert "}")
    (goto-char start)
    (insert "{")
    (evil-normal-state)))

;; Bind it to SPC { in visual mode
(define-key evil-visual-state-map (kbd "SPC {") 'wrap-with-curly-braces)

;; Function to wrap selected text in double quotes
(defun wrap-with-double-quotes ()
  "Wrap selected text with double quotes."
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert "\"")
    (goto-char start)
    (insert "\"")
    (evil-normal-state)))

;; Bind it to SPC " in visual mode
(define-key evil-visual-state-map (kbd "SPC \"") 'wrap-with-double-quotes)

;; Function to wrap selected text in single quotes
(defun wrap-with-single-quotes ()
  "Wrap selected text with single quotes."
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert "'")
    (goto-char start)
    (insert "'")
    (evil-normal-state)))

;; Bind it to SPC ' in visual mode
(define-key evil-visual-state-map (kbd "SPC '") 'wrap-with-single-quotes)



;; Make visual mode 'd' delete without yanking
(define-key evil-visual-state-map (kbd "d") (lambda ()
                                             (interactive)
                                             (evil-delete (region-beginning) (region-end) nil ?_)))

;; Make visual mode 'p' and 'P' delete selection without yanking and then paste
(define-key evil-visual-state-map (kbd "p") (lambda ()
                                             (interactive)
                                             (evil-delete (region-beginning) (region-end) nil ?_)
                                             (evil-paste-after 1)))

(define-key evil-visual-state-map (kbd "P") (lambda ()
                                             (interactive)
                                             (evil-delete (region-beginning) (region-end) nil ?_)
                                             (evil-paste-before 1)))



;; Window management bindings
(global-set-key (kbd "C-c s") 'split-window-horizontally)
(global-set-key (kbd "C-c d") 'split-window-vertically) 
(global-set-key (kbd "C-c f") 'delete-window)
(global-set-key (kbd "C-c a") 'delete-other-windows)
(global-set-key (kbd "C-c o") 'balance-windows)
(global-set-key (kbd "M-o") 'other-window)

;; init.el ends here
