(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e410458d3e769c33e0865971deb6e8422457fad02bf51f7862fa180ccc42c032" "0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850" default))
 '(package-selected-packages
   '(ace-window evil-collection evil magit vterm company ## rust-mode modus-themes)))
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
  (define-key evil-motion-state-map (kbd ")") 'evil-end-of-line))

(unless (package-installed-p 'ace-window)
  (package-refresh-contents)
  (package-install 'ace-window))
(global-set-key (kbd "M-o") 'ace-window)

(require 'evil-snipe)
(evil-snipe-mode 1)
(evil-snipe-override-mode 1
			  )
(with-eval-after-load 'evil-snipe
  (define-key evil-normal-state-map (kbd "M-g") 'evil-snipe-s)
  (define-key evil-visual-state-map (kbd "M-g") 'evil-snipe-s))
(setq evil-snipe-scope 'buffer) ;; Search within the entire buffer, no wrap.
(defun open-init-file ()
  "Open the Emacs init file."
  (interactive)
  (find-file "~/.config/emacs/init.el"))

;; You can bind this function to a key for quick access
(global-set-key (kbd "C-c I") 'open-init-file)
