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
  "If the current file matches 'TExx.yy.zz.md' or 'VExx.yy.zz.md', jump to 'ASxx.yy.md' in the same directory."
  (interactive)
  (when (buffer-file-name)
    (let* ((filename (file-name-nondirectory (buffer-file-name)))
           (dir (file-name-directory (buffer-file-name)))
           (regex "^\\(TE\\|VE\\)\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.md$")
           (match (string-match regex filename)))
      (if match
          (let ((new-file (concat dir "AS"
                                  (match-string 2 filename) "."
                                  (match-string 3 filename) ".md")))
            (if (file-exists-p new-file)
                (find-file new-file)
              (message "File %s does not exist" new-file)))
        (message "Current file name does not match expected pattern")))))


(global-set-key (kbd "C-c j a") 'jump-to-assertion)

(defun jump-to-te ()
  "If the current file is 'ASxx.yy.md' or 'VExx.yy.zz.md', jump to 'TExx.yy.01.md' in the same directory."
  (interactive)
  (when (buffer-file-name)
    (let* ((filename (file-name-nondirectory (buffer-file-name)))
           (dir (file-name-directory (buffer-file-name)))
           (regex-as "^AS\\([0-9]+\\)\\.\\([0-9]+\\)\\.md$")
           (regex-ve "^\\(VE\\)\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.md$")
           match)
      (cond
       ;; If the current file is "ASxx.yy.md"
       ((string-match regex-as filename)
        (let ((new-file (concat dir "TE"
                                (match-string 1 filename) "."
                                (match-string 2 filename) ".01.md")))
          (if (file-exists-p new-file)
              (find-file new-file)
            (message "File %s does not exist" new-file))))
       
       ;; If the current file is "VExx.yy.zz.md"
       ((string-match regex-ve filename)
        (let ((new-file (concat dir "TE"
                                (match-string 2 filename) "."
                                (match-string 3 filename) ".01.md")))
          (if (file-exists-p new-file)
              (find-file new-file)
            (message "File %s does not exist" new-file))))
       
       ;; If the filename doesn't match either pattern
       (t (message "Current file name does not match expected pattern"))))))

(global-set-key (kbd "C-c j t") 'jump-to-te)

(defun jump-to-ve ()
  "If the current file is 'ASxx.yy.md' or 'TExx.yy.zz.md', jump to 'VExx.yy.01.md' in the same directory."
  (interactive)
  (when (buffer-file-name)
    (let* ((filename (file-name-nondirectory (buffer-file-name)))
           (dir (file-name-directory (buffer-file-name)))
           (regex-as "^AS\\([0-9]+\\)\\.\\([0-9]+\\)\\.md$")
           (regex-ve "^\\(TE\\)\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.md$")
           match)
      (cond
       ;; If the current file is "ASxx.yy.md"
       ((string-match regex-as filename)
        (let ((new-file (concat dir "VE"
                                (match-string 1 filename) "."
                                (match-string 2 filename) ".01.md")))
          (if (file-exists-p new-file)
              (find-file new-file)
            (message "File %s does not exist" new-file))))
       
       ;; If the current file is "VExx.yy.zz.md"
       ((string-match regex-ve filename)
        (let ((new-file (concat dir "VE"
                                (match-string 2 filename) "."
                                (match-string 3 filename) ".01.md")))
          (if (file-exists-p new-file)
              (find-file new-file)
            (message "File %s does not exist" new-file))))
       
       ;; If the filename doesn't match either pattern
       (t (message "Current file name does not match expected pattern"))))))

(global-set-key (kbd "C-c j v") 'jump-to-ve)






;;; pensec-commands.el ends here
