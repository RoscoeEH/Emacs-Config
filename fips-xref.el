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



;;; fips-xref.el ends here
