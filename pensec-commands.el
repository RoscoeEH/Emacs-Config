;;; pensec-commands.el starts here


;; Copy paste for windows
(global-set-key (kbd "C-c C-c") 'kill-ring-save)
(global-set-key (kbd "C-c C-v") 'yank)

;; Custom command for FIPS report editing
(defun update-test-status ()
  "Find the first occurrence of 'test-status: ' in the current buffer,
prompt the user for a new value with shortcuts, and replace the existing value."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^test-status: " nil t)
      (let* ((status-options '(("passed" . "passed")
                               ("open" . "open")
                               ("na" . "not applicable")
                               ("waiting" . "waiting")))
             (input (completing-read "Enter new test status (p=passed, o=open, n=not applicable, w=waiting): "
                                     (mapcar #'car status-options) nil nil))
             (new-status (or (cdr (assoc input status-options)) input)))
        (delete-region (point) (line-end-position))
        (insert new-status)))))


(define-key evil-normal-state-map (kbd "C-=") 'update-test-status)

;; grep buffer for test status
(defun search-te-md-files-for-tag (directory tag)
  "Search for TAG in markdown files beginning with 'TE' under DIRECTORY, with results sorted by filename."
  (interactive "DDirectory: \nsTag to search for: ")
  (let ((command
         (format "rg -nH --sort-files -i -e \"test-status:\\s*%s\" --glob \"TE*.md\" \"%s\""
                 tag directory)))
    (grep command)))

(global-set-key (kbd "M-g s") 'search-te-md-files-for-tag)

(defun search-te-md-files-for-qa (directory tag)
  "Search for TAG in markdown files beginning with 'TE' under DIRECTORY, with results sorted by filename, this one finds QA status."
  (interactive "DDirectory: \nsTag to search for: ")
  (let ((command
         (format "rg -nH --sort-files -i -e \"QA:\\s*%s\" --glob \"TE*.md\" \"%s\""
                 tag directory)))
    (grep command)))

(global-set-key (kbd "M-g q") 'search-te-md-files-for-qa)

;; Open a new notes file
(defun create-note-file (name)
  "Prompt for a note NAME (defaults to 'notes' if empty), create a markdown file with NAME, date, and time in the path C:/Users/Roscoe/Documents/Notes."
  (interactive
   (list (let ((input (read-string "Note name (default: 'notes'): ")))
           (if (string-empty-p input) "notes" input))))
  (let* ((base-dir "C:/Users/Roscoe/Documents/Notes/")
         (timestamp (format-time-string "%Y-%m-%d_%H-%M-%S"))
         (filename (format "%s-%s.md" name timestamp))
         (full-path (expand-file-name filename base-dir)))
    ;; Ensure directory exists
    (unless (file-directory-p base-dir)
      (make-directory base-dir t))
    ;; Create and open the file
    (find-file full-path)
    ;; Insert a title in Markdown format
    (insert (format "# %s (%s)\n\n" name (format-time-string "%Y-%m-%d %H:%M")))))

(global-set-key (kbd "C-c n") 'create-note-file)

(defun replace-literal-n-with-newline (start end)
  "Replace all literal '\\n' strings with actual newlines in the selected region, or entire buffer if no region is active."
  (interactive "r")
  (save-excursion
    (let ((search-start (if (use-region-p) start (point-min)))
          (search-end   (if (use-region-p) end   (point-max))))
      (goto-char search-start)
      (while (re-search-forward "\\\\n" search-end t)
        (replace-match "\n" nil t)))))

(global-set-key (kbd "C-*") 'replace-literal-n-with-newline)

(defun jump-to-te-md-references ()
  "Jump to or create 'References:'"
  (interactive)
  (goto-char (point-min))
  (if (not (re-search-forward "^## Assessment" nil t))
      (message "No Assessment section found")
    (let* ((limit (or (save-excursion
                        (when (re-search-forward "^## " nil t)
                          (line-beginning-position)))
                      (point-max))))
      (if (re-search-forward "^References:" limit t)
          (markdown-forward-paragraph)
        (goto-char limit)
        (unless (bolp) (newline)) 
        (insert "References:\n\n")
        (forward-line -1)
        (insert "-- ")
        (evil-append 1 1 nil)
        ))))

;; (defun jump-to-te-md-references ()
;;   "Jump to the end of the first line that contains 'references:'."
;;   (interactive)
;;   (goto-char (point-min))
;;   (when (re-search-forward "^## Assessment" nil t)
;;       (re-search-forward "^references:" nil t)
;;     (end-of-line)
;;     (markdown-forward-paragraph)))

(global-set-key (kbd "C-'") 'jump-to-te-md-references)

(defun alg-testing ()
  "Run the algorithm testing tool"
  (interactive)
  (let ((default-directory "C:/Users/Roscoe/Documents/Tools and Scripts/ACV_Client/"))
    (compile "python3 ProgramFiles/acv_gui_tk.py")))

(use-package openwith
  :ensure t
  :config
  (setq openwith-associations
        '(("\\.pdf\\'" "start" ("\"\"" file))
          ("\\.docx?\\'" "start" ("\"\"" file))
          ("\\.xlsx?\\'" "start" ("\"\"" file))
          ("\\.pptx?\\'" "start" ("\"\"" file))))
  (openwith-mode 1))

;;; pensec-commands.el ends here
