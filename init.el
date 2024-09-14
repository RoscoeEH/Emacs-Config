(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e410458d3e769c33e0865971deb6e8422457fad02bf51f7862fa180ccc42c032" "0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850" default))
 '(package-selected-packages
   '(aggressive-indent goto-last-change flycheck bm minimap rainbow-delimiters ace-window evil-collection evil magit vterm company ## rust-mode modus-themes)))
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



;; Ensure Magit is installed
(unless (package-installed-p 'magit)
  (package-refresh-contents)
  (package-install 'magit))

;; Bind Magit status to a key (e.g., C-x g)
(global-set-key (kbd "C-x g") 'magit-status)


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

(with-eval-after-load 'evil
  (define-key evil-motion-state-map (kbd ")") 'evil-end-of-line)
  ;; Bind occur to Space-f in normal and visual states
  (define-key evil-normal-state-map (kbd "SPC f") 'occur)
  (define-key evil-visual-state-map (kbd "SPC f") 'occur))



(unless (package-installed-p 'ace-window)
  (package-refresh-contents)
  (package-install 'ace-window))

(global-set-key (kbd "M-o") 'ace-window)

;; Set ace-window to use home row keys
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))




(require 'evil-snipe)
(evil-snipe-mode 1)
(evil-snipe-override-mode 1)

;; Use Space-s
(with-eval-after-load 'evil-snipe
  (define-key evil-normal-state-map (kbd "SPC s") 'evil-snipe-s)
  (define-key evil-visual-state-map (kbd "SPC s") 'evil-snipe-s))

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
(global-set-key (kbd "C-c m") 'bm-next)

;; Bind C-c n to go to the previous bookmark
(global-set-key (kbd "C-c n") 'bm-previous)


;; Enable Flycheck in programming modes
(add-hook 'prog-mode-hook #'flycheck-mode)

(require 'flycheck)
(require 'flycheck-rust)

(add-hook 'rust-mode-hook #'flycheck-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)


;; Load goto-last-change package
(use-package goto-last-change
  :ensure t
  :bind (("C-c z" . goto-last-change)))


;; Load and enable aggressive-indent-mode globally
(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))


(global-set-key (kbd "M-<up>") 'scroll-down-line)
(global-set-key (kbd "M-<down>") 'scroll-up-line)


