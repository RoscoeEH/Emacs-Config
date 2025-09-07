;;; setup.el starts here

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)


;; for some reason emacs was not loading a bunch of packages so this is my fix
(add-to-list 'load-path ELPA_PATH)
(dolist (dir (directory-files ELPA_PATH t "^[^.]"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))


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

;; Font
(set-frame-font "Fira Code-13" nil t)


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


;; remove tool bar
(tool-bar-mode -1)

(global-display-line-numbers-mode t)  ;; Enable line numbers globally
(setq display-line-numbers-type 'relative) ;; Set relative line numbers


(electric-pair-mode 1)

; manage tab spacing
(defun convert-tabs-to-spaces ()
  "Convert all tabs to spaces, except in Makefile modes."
  (unless (or (derived-mode-p 'makefile-mode)
              (derived-mode-p 'makefile-bsdmake-mode))
    (untabify (point-min) (point-max))))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(add-hook 'before-save-hook 'convert-tabs-to-spaces)

(dolist (hook '(makefile-mode-hook makefile-bsdmake-mode-hook))
  (add-hook hook (lambda () (setq indent-tabs-mode t))))


;; Remove scroll bars
(scroll-bar-mode -1)

;; Short answers
(fset 'yes-or-no-p 'y-or-n-p)

(setq use-short-answers t)


;; Do not store temp files
(setq make-backup-files nil)

;; visual line wrap in all modes
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (visual-line-mode 1)
            (setq word-wrap t)))


;; compilation buffer printing
(require 'ansi-color)

(defun my-ansi-colorize-compilation-buffer ()
  "Colorize ANSI sequences in compilation buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(add-hook 'compilation-filter-hook #'my-ansi-colorize-compilation-buffer)
(setq ansi-color-for-comint-mode t) ; Enables support for 256-color ANSI


(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Disable super key
(setq x-super-keysym nil)

;; SSH setup
(setq tramp-verbose 10)



;;; setup.el ends here
