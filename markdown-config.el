;;; markdown-config.el starts here

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

(add-hook 'markdown-mode-hook
          (lambda ()
            ;; Change the colors for Markdown headers
            (set-face-foreground 'markdown-header-face-1 "DeepSkyBlue1")
            (set-face-foreground 'markdown-header-face-2 "DarkOrange1")
            (set-face-foreground 'markdown-header-face-3 "MediumOrchid1")
            (set-face-foreground 'markdown-header-face-4 "YellowGreen")
            (set-face-foreground 'markdown-header-face-5 "SpringGreen")
            (set-face-foreground 'markdown-header-face-6 "SlateBlue1")))


;;; markdown-config.el ends here
