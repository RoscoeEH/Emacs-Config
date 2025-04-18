;;; evil-config.el starts here

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
  (define-key evil-visual-state-map (kbd "SPC f") 'occur))



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

;; Optionally change cursor colors by Evil state if using evil-mode
(setq evil-normal-state-cursor '("hot pink" box))
(setq evil-insert-state-cursor '("green" bar))
(setq evil-visual-state-cursor '("red" box))



;;; evil-config.el ends here
