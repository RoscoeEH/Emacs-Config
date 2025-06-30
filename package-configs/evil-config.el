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


(defun insert-line-below ()
  "Insert a new line below the current line without moving the cursor."
  (interactive)
  (save-excursion
    (end-of-line)
    (newline)))

(defun insert-line-above (universal)
  "Insert an empty line above the current line.
The behaviour change if you pass the default UNIVERSAL argument.  Without it, a new line 
above the current one will be created, but the point will not change its location.  With 
the default UNIVERSAL argument, the point will change to the beginning of the new line created."
  (interactive "P")
  (if (equal universal '(4))
      (progn
        (end-of-line 0)
        (open-line 1)
        (forward-line))
    (save-excursion
      (end-of-line 0)
      (open-line 1))))




;; Evil special keys
(with-eval-after-load 'evil
  ;; bind normal mode n to add new line
  (define-key evil-normal-state-map (kbd "N") 'insert-line-below)
  (define-key evil-normal-state-map (kbd "n") 'insert-line-above)
  ;; Add half-page-up/down to arrow keys in normal/visual mode
  (define-key evil-normal-state-map (kbd "M-<up>") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "M-<down>") 'evil-scroll-down)
  (define-key evil-visual-state-map (kbd "M-<up>") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "M-<down>") 'evil-scroll-down)

  (define-key evil-normal-state-map (kbd "<up>") 'evil-scroll-line-up)
  (define-key evil-normal-state-map (kbd "<down>") 'evil-scroll-line-down)
  (define-key evil-visual-state-map (kbd "<up>") 'evil-scroll-line-up)
  (define-key evil-visual-state-map (kbd "<down>") 'evil-scroll-line-down)
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
