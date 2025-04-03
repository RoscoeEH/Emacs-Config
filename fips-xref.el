;;; fips-xref.el starts here

(require 'xref)

;;; Find the reportvault directory using .xref-path.md
(defun find-reportvault-root ()
  "Search upward until a directory containing '.xref-path.md' is found.
Then, read '.xref-path.md' to extract the reportvault path. The file should contain:
  reportvault : \"path/to/reportvault\"
The returned path is expanded relative to the directory containing '.xref-path.md'."
  (let ((base (locate-dominating-file default-directory ".xref-path.md")))
    (if base
        (let ((paths-file (expand-file-name ".xref-path.md" base)))
          (if (file-exists-p paths-file)
              (with-temp-buffer
                (insert-file-contents paths-file)
                (if (re-search-forward "^reportvault\\s-*:\\s-*\"\\([^\"]+\\)\"" nil t)
                    (expand-file-name (match-string 1) base)
                  (error "reportvault path not found in %s" paths-file)))
            (error "Paths file not found: %s" paths-file)))
      (error "Could not locate a directory containing '.xref-path.md'."))))

;;; Markdown Xref functions
(defun markdown-find-matching-file (root identifier)
  "Search for a Markdown file matching IDENTIFIER in the reportvault directory.
ROOT should be the absolute path to the reportvault directory."
  (let ((search-path root))
    (car (directory-files-recursively search-path
                                      (concat "\\b" (regexp-quote identifier) "\\.md\\b")))))

(defun markdown-find-definitions (identifier)
  "Find the Markdown report matching IDENTIFIER, searching within reportvault."
  (let ((root (find-reportvault-root)))
    (when root
      (let ((match-file (markdown-find-matching-file root identifier)))
        (when match-file
          (list (xref-make match-file (xref-make-file-location match-file 1 0))))))))

(defun markdown-find-references (identifier)
  "Find all Markdown files referencing IDENTIFIER in reportvault."
  (let ((root (find-reportvault-root)))
    (when root
      (let* ((search-path root)
             (grep-results (shell-command-to-string
                            (format "rg -l '%s' %s/**/*.md" identifier search-path))))
        (mapcar (lambda (file)
                  (xref-make (format "Reference in %s" file)
                             (xref-make-file-location file 1 0)))
                (split-string grep-results "\n" t))))))

(defun markdown-identifier-at-point ()
  "Return the Markdown identifier under point."
  (with-syntax-table (copy-syntax-table (syntax-table))
    (modify-syntax-entry ?. "w")
    (let ((word (thing-at-point 'word t)))
      (when word
        (cond
         ;; If the word ends with ".md." remove the extra period.
         ((string-match "\\(\\.md\\)\\.$" word)
          (replace-regexp-in-string "\\(\\.md\\)\\.$" "\\1" word))
         ;; If it ends with a period and doesn't end with .md, remove the trailing period.
         ((and (string-suffix-p "." word)
               (not (string-suffix-p ".md" word)))
          (substring word 0 -1))
         (t word))))))


;;; Register custom xref backend for Markdown
(defun markdown-xref-backend ()
  "Custom Xref backend for Markdown files if a reportvault directory is found."
  (when (and buffer-file-name (find-reportvault-root))
    'markdown))
(add-hook 'xref-backend-functions #'markdown-xref-backend)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql markdown)))
  "Extract full Markdown identifier at point using `markdown-identifier-at-point`."
  (markdown-identifier-at-point))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql markdown)))
  "Disable identifier completion for markdown backend."
  nil)

(cl-defmethod xref-backend-definitions ((_backend (eql markdown)) identifier)
  (markdown-find-definitions identifier))

(cl-defmethod xref-backend-references ((_backend (eql markdown)) identifier)
  (markdown-find-references identifier))

;; jump command altered to use xref so you can go back
(defun jump-to-assertion ()
  "If the current file matches 'TExx.yy.zz.md' or 'VExx.yy.zz.md', jump to 'ASxx.yy.md' in the same directory using xref."
  (interactive)
  (when (buffer-file-name)
    (let* ((filename (file-name-nondirectory (buffer-file-name)))
           (dir (file-name-directory (buffer-file-name)))
           (regex "^\\(TE\\|VE\\)\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.md$")
           (match (string-match regex filename)))
      (if match
          (let ((target-id (format "AS%s.%s" (match-string 2 filename) (match-string 3 filename))))
            (xref-push-marker-stack)  ;; Save current position
            (let ((defs (markdown-find-definitions target-id)))
              (if defs
                  (xref-pop-to-location (car defs))
                (message "No definition found for %s" target-id))))
        (message "Current file name does not match expected pattern")))))


(global-set-key (kbd "C-c j a") 'jump-to-assertion)

(defun jump-to-te ()
  "If the current file is 'ASxx.yy.md' or 'VExx.yy.zz.md', jump to 'TExx.yy.01.md' in the same directory using xref."
  (interactive)
  (when (buffer-file-name)
    (let* ((filename (file-name-nondirectory (buffer-file-name)))
           (dir (file-name-directory (buffer-file-name)))
           (regex-as "^AS\\([0-9]+\\)\\.\\([0-9]+\\)\\.md$")
           (regex-ve "^VE\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.md$"))
      (cond
       ;; If the current file is "ASxx.yy.md"
       ((string-match regex-as filename)
        (let ((target-id (format "TE%s.%s.01" (match-string 1 filename) (match-string 2 filename))))
          (xref-push-marker-stack)
          (let ((defs (markdown-find-definitions target-id)))
            (if defs
                (xref-pop-to-location (car defs))
              (message "No definition found for %s" target-id)))))

       ;; If the current file is "VExx.yy.zz.md"
       ((string-match regex-ve filename)
        (let ((target-id (format "TE%s.%s.01" (match-string 1 filename) (match-string 2 filename))))
          (xref-push-marker-stack)
          (let ((defs (markdown-find-definitions target-id)))
            (if defs
                (xref-pop-to-location (car defs))
              (message "No definition found for %s" target-id)))))

       ;; If the filename doesn't match either pattern
       (t (message "Current file name does not match expected pattern"))))))

(global-set-key (kbd "C-c j t") 'jump-to-te)

(defun jump-to-ve ()
  "If the current file is 'ASxx.yy.md' or 'TExx.yy.zz.md', jump to 'VExx.yy.01.md' in the same directory using xref."
  (interactive)
  (when (buffer-file-name)
    (let* ((filename (file-name-nondirectory (buffer-file-name)))
           (dir (file-name-directory (buffer-file-name)))
           (regex-as "^AS\\([0-9]+\\)\\.\\([0-9]+\\)\\.md$")
           (regex-te "^TE\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.md$"))
      (cond
       ;; If the current file is "ASxx.yy.md"
       ((string-match regex-as filename)
        (let ((target-id (format "VE%s.%s.01" (match-string 1 filename) (match-string 2 filename))))
          (xref-push-marker-stack)
          (let ((defs (markdown-find-definitions target-id)))
            (if defs
                (xref-pop-to-location (car defs))
              (message "No definition found for %s" target-id)))))

       ;; If the current file is "TExx.yy.zz.md"
       ((string-match regex-te filename)
        (let ((target-id (format "VE%s.%s.01" (match-string 1 filename) (match-string 2 filename))))
          (xref-push-marker-stack)
          (let ((defs (markdown-find-definitions target-id)))
            (if defs
                (xref-pop-to-location (car defs))
              (message "No definition found for %s" target-id)))))

       ;; If the filename doesn't match either pattern
       (t (message "Current file name does not match expected pattern"))))))

(global-set-key (kbd "C-c j v") 'jump-to-ve)


;;; fips-xref.el ends here
