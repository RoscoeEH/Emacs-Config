(custom-set-variables ;; custom-set-variables was added by Custom.
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
;; Non-interactive shell loading
(setq exec-path-from-shell-arguments '("-l"))

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
  (let* ((file-path (buffer-file-name))
         (mina-path "~/Documents/Projects/mina/")
         (in-mina-project (and file-path
                               (string-prefix-p (expand-file-name mina-path)
                                                (expand-file-name file-path))))
         (cmd (cond
               (in-mina-project
                "dune build src/app/cli/src/mina.exe")
               ((and (eq major-mode 'python-mode) file-path)
                (format "python3 %s" (file-name-nondirectory file-path)))
               ((eq major-mode 'rust-mode)
                "cargo build")
               ((eq major-mode 'tuareg-mode)
                "dune build")
               (t "make"))))
    (when cmd
      (setq-local compile-command cmd))))

(add-hook 'python-mode-hook #'improved-compile-command)
(add-hook 'python-ts-mode-hook #'improved-compile-command)
(add-hook 'rust-mode-hook #'improved-compile-command)
(add-hook 'tuareg-mode-hook #'improved-compile-command)
(add-hook 'c-mode-hook #'improved-compile-command)
(add-hook 'c++-mode-hook #'improved-compile-command)



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


(key-chord-define-global "sd" 'save-buffer)


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


(setq magit-git-executable "/usr/bin/git")
(setq magit-credential 'osxkeychain)
(setenv "SSH_AUTH_SOCK" (getenv "SSH_AUTH_SOCK"))

;; Prevent Magit from inheriting direnv environment
(with-eval-after-load 'magit
  (remove-hook 'magit-status-mode-hook #'direnv-update-environment)
  (remove-hook 'magit-process-mode-hook #'direnv-update-environment))



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




;; paste preserves the spacing of the initial line
(defun my-evil-paste-after-dwim (count &optional register yank-handler)
  (interactive "p")
  (let* ((start (point))
         (initial-whitespace (save-excursion
                               (goto-char (line-beginning-position))
                               (when (looking-at "^[[:space:]]*")
                                 (match-string 0)))))
    ;; Paste the text
    (evil-paste-after count register yank-handler)
    ;; Align pasted text by adding initial whitespace to each subsequent line
    (when initial-whitespace
      (save-excursion
        (goto-char start)
        (while (re-search-forward "\n\\([^\n]\\)" nil t)
          (replace-match (concat "\n" initial-whitespace "\\1")))))))


(defun my-evil-paste-before-dwim (count &optional register yank-handler)
  (interactive "p")
  (let* ((start (point))
         (initial-whitespace (save-excursion
                               (goto-char (line-beginning-position))
                               (when (looking-at "^[[:space:]]*")
                                 (match-string 0)))))
    ;; Paste before the cursor
    (evil-paste-before count register yank-handler)
    ;; Align pasted text by adding initial whitespace to each subsequent line
    (when initial-whitespace
      (save-excursion
        (goto-char start)
        (while (re-search-forward "\n\\([^\n]\\)" nil t)
          (replace-match (concat "\n" initial-whitespace "\\1")))))))



(define-key evil-normal-state-map (kbd "p") 'my-evil-paste-after-dwim)
(define-key evil-normal-state-map (kbd "P") 'my-evil-paste-before-dwim)


 ;; Window management bindings
 (global-set-key (kbd "C-c s") 'split-window-horizontally)
 (global-set-key (kbd "C-c d") 'split-window-vertically) 
 (global-set-key (kbd "C-c f") 'delete-window)
 (global-set-key (kbd "C-c a") 'delete-other-windows)
 (global-set-key (kbd "C-c o") 'balance-windows)
 (global-set-key (kbd "M-o") 'other-window)


 ;; Case change commands

 (defun upcase-single-letter ()
   "Convert the character at point to uppercase."
   (interactive)
   (let ((char (char-after)))
     (when char
       (save-excursion
         (delete-char 1)
         (insert (upcase char))))))

 (defun downcase-single-letter ()
   "Convert the character at point to lowercase."
   (interactive)
   (let ((char (char-after)))
     (when char
       (save-excursion
         (delete-char 1)
         (insert (downcase char))))))

 (define-key evil-normal-state-map (kbd "C-x u") 'upcase-single-letter)
 (define-key evil-normal-state-map (kbd "C-x l") 'downcase-single-letter)
 (define-key evil-visual-state-map (kbd "C-x u") 'upcase-region)
 (define-key evil-visual-state-map (kbd "C-x l") 'downcase-region)


 ;; Delete char and enter insert mode
 (define-key evil-normal-state-map (kbd "q") (lambda ()
                                              (interactive)
                                              (delete-char 1)
                                              (evil-insert-state)))


 (use-package dashboard
   :ensure t
   :config
   (dashboard-setup-startup-hook)
   (setq dashboard-startup-banner "/Users/roscoeelings-haynie/.config/emacs/emacs_image.png")
   ;; Method 1: Using custom face
   (custom-set-faces
    '(dashboard-banner-logo-title ((t (:foreground "#2957b0" :weight bold)))))
   
   ;; Method 2: Using both propertize and setting the face explicitly
   (setq dashboard-banner-logo-title 
         (let ((title "
  ******** ****     ****     **       ******   ********
 /**///// /**/**   **/**    ****     **////** **////// 
 /**      /**//** ** /**   **//**   **    // /**       
 /******* /** //***  /**  **  //** /**       /*********
 /**////  /**  //*   /** **********/**       ////////**
 /**      /**   /    /**/**//////**//**    **       /**
 /********/**        /**/**     /** //******  ******** 
 //////// //         // //      //   //////  ////////  
 "))
           (propertize title 'face '(:foreground "red" :weight bold))))
   
   (setq dashboard-center-content t)
   (setq dashboard-items '((recents  . 12)
                          (bookmarks . 12)))
   (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))


 (use-package direnv
   :ensure t
   :config
   (direnv-mode)
   
   ;; Refresh direnv when opening files
   (add-hook 'find-file-hook
             (lambda ()
               (when (file-exists-p (expand-file-name ".envrc" default-directory))
                 (direnv-update-environment)))))


 ;; grep command bindings
 (global-set-key (kbd "M-g g")
                 (lambda ()
                   (interactive)
                   (grep (read-from-minibuffer "Run grep: " "git grep -rni "))))

 (global-set-key (kbd "M-g l")
                 (lambda ()
                   (interactive)
                   (grep (read-from-minibuffer "Run grep: " "grep -rni "))))



 (with-eval-after-load 'dired
   (define-key dired-mode-map [mouse-1] 'dired-single-buffer)
   (define-key dired-mode-map [mouse-2] 'dired-single-buffer))


 ;; GNU plot setup
 (require 'org-plot)
 (use-package gnuplot
   :ensure t
   :config
   (setq gnuplot-inline-image-mode t))


 (use-package grip-mode
   :ensure t
   :config
   (setq grip-update-after-change t)
   (setq grip-binary-path "grip"))

 ;; Bind a key for previewing Markdown
 (global-set-key (kbd "C-x g p") 'grip-mode)

 (defun my/minibuffer-up-one-dir ()
   (interactive)
   ;; If the character immediately before point is '/', delete it
   (when (and (> (point) (point-min)) (eq (char-before) ?/))
     (delete-char -1))
   ;; Save the current point after deletion
   (let ((end (point)))
     ;; Search backward for the last "/"
     (if (search-backward "/" nil t)
         ;; Delete from one character after the found slash to 'end'
         (delete-region (1+ (point)) end)
       (message "No preceding slash found.")))
   ;; Move the cursor one character to the right, if possible.
   (when (< (point) (point-max))
     (forward-char 1)))


 (define-key minibuffer-local-filename-completion-map (kbd "M-DEL") 'my/minibuffer-up-one-dir)


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


 (setq make-backup-files nil)

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


(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "SPC i") 'imenu))

        

;;; init.el ends here
