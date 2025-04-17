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
         (format "rg --sort-files -i -e \"test-status:\\s*%s\" --glob \"TE*.md\" \"%s\""
                 tag directory)))
    (grep command)))

(global-set-key (kbd "M-g f") 'search-te-md-files-for-tag)

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



;;; pensec-commands.el ends here



