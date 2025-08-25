;;; copy-refs.el starts here
(defun sync-te-refs ()
  "Overwrites references YAML headerin TEs with lower references block"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (not (re-search-forward "^## Assessment" nil t))
        (message "No Assessment section found block found")
      ;; advance to second occurrence
      (if (not (re-search-forward "^References:" nil t))
          (message "No References found. Nothing to sync.")
        (let ((bottom-lines '()))
          (forward-line 1)
          ;; collect lines
          (while (and (not (eobp))
                      (looking-at "^[ \t]*--"))
            (let* ((raw (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))
                   ;; format
                   (clean (replace-regexp-in-string "^[ \t]*--[ \t]*" "" raw)))
              (push clean bottom-lines))
            (forward-line 1))
          (setq bottom-lines (nreverse bottom-lines))

          (if (null bottom-lines)
              (message "Found 'References:' but no '--'.")
            ;; Find YAML references
            (goto-char (point-min))
            (if (not (re-search-forward "^references:" nil t))
                (message "No lowercase '^references:' block found at the top.")
              (let ((insert-pos (progn (forward-line 1) (point)))
                    top-end)
                (while (and (not (eobp)) (looking-at "^[ \t]*-"))
                  (forward-line 1))
                (setq top-end (point))
                (when (> top-end insert-pos)
                  (delete-region insert-pos top-end))
                (goto-char insert-pos)
                (dolist (line bottom-lines)
                  (insert "   - " line "\n"))
                (message "Top 'references:' synchronized from second 'References:'. Completed.")))))))))

(defun sync-te-refs-on-save ()
  "If current buffer is a TE*.md file, sync top references from bottom."
  (when (and buffer-file-name
             (string-match-p "^TE.*\\.md$" (file-name-nondirectory buffer-file-name)))
    (sync-te-refs)))

(defun sync-te-refs-hook ()
  "Add buffer-local hook to sync references in TE*.md files on save."
  (add-hook 'before-save-hook #'sync-te-refs-on-save nil t))

;; Attach to all future markdown buffers
(add-hook 'markdown-mode-hook #'sync-te-refs-hook)

(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when (derived-mode-p 'markdown-mode)
      (sync-te-refs-hook))))

;;; copy-refs.el ends here
