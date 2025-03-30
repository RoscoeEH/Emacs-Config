;;; fips-xref.el starts here

;; register custom xref backend
(defun markdown-xref-backend ()
  "Custom Xref backend for Markdown files in reportvault."
  (when (and buffer-file-name
             (string-match "reportvault" buffer-file-name))
    'markdown))

(add-hook 'xref-backend-functions #'markdown-xref-backend)

;; jump to def
(require 'xref)
(require 'f) ;; Provides `f-expand` for file paths

(defun markdown-find-definitions (identifier)
  "Find the Markdown report matching IDENTIFIER."
  (let* ((root (locate-dominating-file default-directory "reportvault"))
         (match-file (markdown-find-matching-file root identifier)))
    (when match-file
      (list (xref-make match-file (xref-make-file-location match-file 1 0))))))

(defun markdown-find-matching-file (root identifier)
  "Search for a Markdown file matching IDENTIFIER in reportvault."
  (let ((search-path (f-expand "reportvault" root)))
    (car (directory-files-recursively search-path
                                      (concat "\\b" (regexp-quote identifier) "\\.md\\b")))))


(defun markdown-find-references (identifier)
  "Find all Markdown files referencing IDENTIFIER."
  (let* ((root (locate-dominating-file default-directory "reportvault"))
         (search-path (f-expand "reportvault" root))
         (grep-results (shell-command-to-string
                        (format "rg -l '%s' %s/**/*.md" identifier search-path))))
    (mapcar (lambda (file)
              (xref-make (format "Reference in %s" file)
                         (xref-make-file-location file 1 0)))
            (split-string grep-results "\n" t))))

(defun markdown-identifier-at-point ()
  "Return the Markdown identifier under point.
Temporarily treat dots as word constituents so that identifiers
like AS12.34.md are returned as one word."
  (with-syntax-table (copy-syntax-table (syntax-table))
    (modify-syntax-entry ?. "w")
    (thing-at-point 'word t)))



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
