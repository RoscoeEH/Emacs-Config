;;; markdown-config.el starts here

;; markdown list quality changes
(use-package markdown-mode
  :ensure t)

(defun markdown-list-dwim ()
  "Continue a Markdown list item or checklist based on the current line,
or erase the delimiter if the line is empty.
Handles bullets ('- ' or '* '), numbered items (\"1) \" or \"1. \"),
and checklists ('- [ ] ')."
  (interactive)
  (message "markdown-list-dwim called")
  (let* ((current-line (thing-at-point 'line t))
         ;; Capture leading whitespace.
         (indent (if (string-match "^[ \t]*" current-line)
                     (match-string 0 current-line)
                   ""))
         new-prefix)
    ;; If the current line is empty (only the list delimiter), delete it.
    (if (or (string-match "^[ \t]*[-*]\\s-*$" current-line)
            (string-match "^[ \t]*[0-9]+[)][ \t]*$" current-line)
            (string-match "^[ \t]*[0-9]+[.][ \t]*$" current-line)
            (string-match "^[ \t]*-\\s-*\\[[xX ]\\][ \t]*$" current-line))
        (delete-region (line-beginning-position) (line-end-position))
      (progn
        (cond
         ;; Checklist: "- [ ]" or "- [x]"
         ((string-match "^[ \t]*-\\s-*\\[\\([xX ]\\)\\]\\s-+" current-line)
          (setq new-prefix (concat indent "- [ ] ")))
         ;; Ordered list with parentheses (e.g., "1)" or "2)")
         ((string-match "^[ \t]*\\([0-9]+\\)[)][ \t]+"
                         current-line)
          (let ((num (string-to-number (match-string 1 current-line))))
            (setq new-prefix (concat indent (number-to-string (1+ num)) ") "))))
         ;; Ordered list with period (e.g., "1." or "2.")
         ((string-match "^[ \t]*\\([0-9]+\\)\\.[ \t]+"
                         current-line)
          (let ((num (string-to-number (match-string 1 current-line))))
            (setq new-prefix (concat indent (number-to-string (1+ num)) ". "))))
         ;; Bullet list (using "-" or "*")
         ((string-match "^[ \t]*\\([-*]\\)\\s-+"
                         current-line)
          (setq new-prefix (concat indent (match-string 1 current-line) " ")))
         ;; Fallback: no prefix.
         (t (setq new-prefix nil)))
        (newline)
        (when new-prefix
          (insert new-prefix))))))



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
