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
      (let* ((status-options '(("p" . "passed")
                               ("o" . "open")
                               ("n" . "not applicable")
                               ("w" . "waiting")))
             (input (completing-read "Enter new test status (p=passed, o=open, n=not applicable, w=waiting): "
                                     (mapcar #'car status-options) nil nil))
             (new-status (or (cdr (assoc input status-options)) input)))
        (delete-region (point) (line-end-position))
        (insert new-status)))))


(define-key evil-normal-state-map (kbd "C-=") 'update-test-status)

;; grep buffer for test status
(defun search-te-md-files-for-tag (directory tag)
  "Searches for a specific TAG in markdown files that start with 'TE' in DIRECTORY using ripgrep."
  (interactive "DDirectory: \nsTag to search for: ")
  (let ((command (format "rg -i -e 'test-status:\\s*%s' --glob 'TE*.md' %s" tag directory)))
    (grep command)))

(global-set-key (kbd "M-g f") 'search-te-md-files-for-tag)


(defun jump-to-assertion ()
  "If the current file matches 'TExx.yy.zz.md', jump to 'ASxx.yy.md' in the same directory."
  (interactive)
  (when (buffer-file-name)
    (let* ((filename (file-name-nondirectory (buffer-file-name)))
           (dir (file-name-directory (buffer-file-name)))
           (regex "^TE\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.md$")
           (match (string-match regex filename)))
      (if match
          (let ((new-file (concat dir "AS"
                                  (match-string 1 filename) "."
                                  (match-string 2 filename) ".md")))
            (if (file-exists-p new-file)
                (find-file new-file)
              (message "File %s does not exist" new-file)))
        (message "Current file name does not match expected pattern")))))

(global-set-key (kbd "C-c j") 'jump-to-assertion)


;;; pensec-commands.el ends here
