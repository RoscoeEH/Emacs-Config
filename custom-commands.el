;;; custom-commands.el starts here

;; neighboring files
(defun next-neighbor-file ()
  "Move to the next non-directory, non-image file in the current directory, wrapping around if needed."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (dir (file-name-directory current-file))
         (image-extensions '("png" "jpg" "jpeg" "gif" "bmp" "svg" "webp" "tiff" "ico"))
         (files (seq-filter
                 (lambda (f)
                   (and (not (file-directory-p f))
                        (not (member (file-name-extension f) image-extensions))))
                 (directory-files dir t "^[^.].*")))
         (next-file (or (car (cdr (member current-file files))) (car files)))) ; Wrap around
    (if next-file
        (find-file next-file)
      (message "No non-image files found."))))



(defun previous-neighbor-file ()
  "Move to the previous non-directory, non-image file in the current directory, wrapping around if needed."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (dir (file-name-directory current-file))
         (image-extensions '("png" "jpg" "jpeg" "gif" "bmp" "svg" "webp" "tiff" "ico"))
         (files (seq-filter
                 (lambda (f)
                   (and (not (file-directory-p f))
                        (not (member (file-name-extension f) image-extensions))))
                 (directory-files dir t "^[^.].*")))
         (prev-file (or (car (last (seq-take-while (lambda (f) (not (equal f current-file))) files)))
                        (car (last files)))))  ; Wrap around to last file
    (if prev-file
        (find-file prev-file)
      (message "No previous non-image file."))))


(define-key evil-normal-state-map (kbd "F") 'next-neighbor-file)
(define-key evil-normal-state-map (kbd "B") 'previous-neighbor-file)


;; File navigation commands
(defun jump-next-function-def ()
    "Jump to the beginning of the next function definition."
    (interactive)
    (beginning-of-defun -1))

(defun jump-previous-function-def ()
    "Jump to the beginning of the previous function definition."
    (interactive)
    (beginning-of-defun 1))

(define-key evil-normal-state-map (kbd "}") 'jump-next-function-def)
(define-key evil-normal-state-map (kbd "{") 'jump-previous-function-def)
(define-key evil-visual-state-map (kbd "}") 'jump-next-function-def)
(define-key evil-visual-state-map (kbd "{") 'jump-previous-function-def)

(defun backward-sexp-adjusted ()
    "Jump backward to the matching opening delimiter.
    If the character under point is a closing delimiter, move one char right first."
    (interactive)
    (when (member (char-after) '(?\) ?\] ?\}))
        (forward-char 1))
    (backward-sexp))


(define-key evil-normal-state-map (kbd "]]") 'forward-sexp)
(define-key evil-normal-state-map (kbd "[[") 'backward-sexp-adjusted)
(define-key evil-visual-state-map (kbd "]]") 'forward-sexp)
(define-key evil-visual-state-map (kbd "[[") 'backward-sexp-adjusted)


;; grep command bindings
(global-set-key (kbd "M-g r")
(lambda ()
    (interactive)
    (grep (read-from-minibuffer "Run grep: " "rg "))))


;; Function to wrap selected text in parentheses
(defun wrap-with-parens ()
"Wrap selected text with parentheses."
(interactive)
(let ((start (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert ")")
    (goto-char start)
    (insert "(")
    (evil-normal-state)))

;; Bind it to SPC ( in visual mode
(define-key evil-visual-state-map (kbd "SPC (") 'wrap-with-parens)

;; Function to wrap selected text in square brackets
(defun wrap-with-square-brackets ()
"Wrap selected text with square brackets."
(interactive)
(let ((start (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert "]")
    (goto-char start)
    (insert "[")
    (evil-normal-state)))

;; Bind it to SPC [ in visual mode
(define-key evil-visual-state-map (kbd "SPC [") 'wrap-with-square-brackets)

;; Function to wrap selected text in curly braces
(defun wrap-with-curly-braces ()
"Wrap selected text with curly braces."
(interactive)
(let ((start (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert "}")
    (goto-char start)
    (insert "{")
    (evil-normal-state)))

;; Bind it to SPC { in visual mode
(define-key evil-visual-state-map (kbd "SPC {") 'wrap-with-curly-braces)

;; Function to wrap selected text in double quotes
(defun wrap-with-double-quotes ()
"Wrap selected text with double quotes."
(interactive)
(let ((start (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert "\"")
    (goto-char start)
    (insert "\"")
    (evil-normal-state)))

;; Bind it to SPC " in visual mode
(define-key evil-visual-state-map (kbd "SPC \"") 'wrap-with-double-quotes)

;; Function to wrap selected text in single quotes
(defun wrap-with-single-quotes ()
"Wrap selected text with single quotes."
(interactive)
(let ((start (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert "'")
    (goto-char start)
    (insert "'")
    (evil-normal-state)))

;; Bind it to SPC ' in visual mode
(define-key evil-visual-state-map (kbd "SPC '") 'wrap-with-single-quotes)

(defun remove-wrapping-delimiters ()
  (interactive)
  (when (evil-visual-state-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (first-char (char-after beg))
           (last-char (char-before end))
           (pairs '((?\( . ?\)) (?\{ . ?\}) (?\[ . ?\]) (?\" . ?\") (?\' . ?\'))))
      (when (and first-char last-char (assoc first-char pairs))
        (let ((matching-char (cdr (assoc first-char pairs))))
          (when (eq last-char matching-char)
            (save-excursion
              (goto-char end)
              (delete-char -1)
              (goto-char beg)
              (delete-char 1))))))))

(define-key evil-visual-state-map (kbd "SPC )") 'remove-wrapping-delimiters)




;; Make visual mode 'd' delete without yanking
(define-key evil-visual-state-map (kbd "d") (lambda ()
                                            (interactive)
                                            (evil-delete (region-beginning) (region-end) nil ?_)))

;; Make visual mode 'p' and 'P' delete selection without yanking and then paste
(define-key evil-visual-state-map (kbd "p") (lambda ()
                                            (interactive)
                                            (evil-delete (region-beginning) (region-end) nil ?_)
                                            (evil-paste-after 1)))

(define-key evil-visual-state-map (kbd "P") (lambda ()
                                            (interactive)
                                            (evil-delete (region-beginning) (region-end) nil ?_)
                                            (evil-paste-before 1)))




;; paste preserves the spacing of the initial line
(defun evil-paste-after-dwim (count &optional register yank-handler)
(interactive "p")
(let* ((start (point))
        (initial-whitespace (save-excursion
                                (goto-char (line-beginning-position))
                                (when (looking-at "^[[:space:]]*")
                                (match-string 0)))))
    ;; Paste the text
    (evil-paste-after count register yank-handler)
    ;; Align pasted text by adding initial whitespace to each subsequent line
    (when initial-whitespace
    (save-excursion
        (goto-char start)
        (while (re-search-forward "\n\\([^\n]\\)" nil t)
        (replace-match (concat "\n" initial-whitespace "\\1")))))))


(defun evil-paste-before-dwim (count &optional register yank-handler)
(interactive "p")
(let* ((start (point))
        (initial-whitespace (save-excursion
                                (goto-char (line-beginning-position))
                                (when (looking-at "^[[:space:]]*")
                                (match-string 0)))))
    ;; Paste before the cursor
    (evil-paste-before count register yank-handler)
    ;; Align pasted text by adding initial whitespace to each subsequent line
    (when initial-whitespace
    (save-excursion
        (goto-char start)
        (while (re-search-forward "\n\\([^\n]\\)" nil t)
        (replace-match (concat "\n" initial-whitespace "\\1")))))))



(define-key evil-normal-state-map (kbd "p") 'evil-paste-after-dwim)
(define-key evil-normal-state-map (kbd "P") 'evil-paste-before-dwim)


;; Window management bindings
(global-set-key (kbd "C-c s") 'split-window-horizontally)
(global-set-key (kbd "C-c d") 'split-window-vertically) 
(global-set-key (kbd "C-c f") 'delete-window)
(global-set-key (kbd "C-c a") 'delete-other-windows)
(global-set-key (kbd "C-c o") 'balance-windows)
(global-set-key (kbd "M-o") 'other-window)


;; Case change commands

(defun upcase-single-letter ()
    "Convert the character at point to uppercase."
    (interactive)
    (let ((char (char-after)))
    (when char
        (save-excursion
        (delete-char 1)
        (insert (upcase char))))))

(defun downcase-single-letter ()
    "Convert the character at point to lowercase."
    (interactive)
    (let ((char (char-after)))
    (when char
        (save-excursion
        (delete-char 1)
        (insert (downcase char))))))

(define-key evil-normal-state-map (kbd "C-x u") 'upcase-single-letter)
(define-key evil-normal-state-map (kbd "C-x l") 'downcase-single-letter)
(define-key evil-visual-state-map (kbd "C-x u") 'upcase-region)
(define-key evil-visual-state-map (kbd "C-x l") 'downcase-region)


;; Delete char and enter insert mode
(define-key evil-normal-state-map (kbd "q") (lambda ()
                                            (interactive)
                                            (delete-char 1)
                                            (evil-insert-state)))

(defun toggle-fullscreen ()
    "Toggle fullscreen mode on macOS."
    (interactive)
    (set-frame-parameter nil 'fullscreen
                            (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

;; Bind Command-Control-f to toggle fullscreen
(global-set-key (kbd "s-C-f") 'toggle-fullscreen)


;; Skip eol chars on evil-end-of-line
(defun evil-end-of-line-non-whitespace ()
    "Move to the last character before the newline, ignoring trailing whitespace."
    (interactive)
    (move-end-of-line 1)
    (skip-chars-backward " \t"))

(define-key evil-normal-state-map (kbd "$") 'evil-end-of-line-non-whitespace)
(define-key evil-visual-state-map (kbd "$") 'evil-end-of-line-non-whitespace)


(global-set-key (kbd "M-<up>") 'scroll-down-line)
(global-set-key (kbd "M-<down>") 'scroll-up-line)

(defun open-init-file ()
    "Open the Emacs init file."
    (interactive)
    (find-file (concat CONFIG_PATH "init.el")))


(global-set-key (kbd "C-c I") 'open-init-file)


;; Tabbing sections in visual mode
(define-key evil-visual-state-map (kbd "TAB") 'maintain/evil-shift-right-visual)
(define-key evil-visual-state-map (kbd "<backtab>") 'maintain/evil-shift-left-visual)

(defun maintain/evil-shift-left-visual ()
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun maintain/evil-shift-right-visual ()
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

;; Custom compile command
(defun improved-compile-command ()
    "Set `compile-command` dynamically based on the major mode."
    (interactive)
    (let* ((file-path (buffer-file-name))
            (mina-path "~/Documents/Projects/mina/")
            (in-mina-project (and file-path
                                    (string-prefix-p (expand-file-name mina-path)
                                                    (expand-file-name file-path))))
            (cmd (cond
                    (in-mina-project
                    "dune build src/app/cli/src/mina.exe")
                    ((and (eq major-mode 'python-mode) file-path)
                    (format "python3 %s" (file-name-nondirectory file-path)))
                    ((eq major-mode 'rust-mode)
                    "cargo build")
                    ((eq major-mode 'tuareg-mode)
                    "dune build")
                    (t "make"))))
        (when cmd
        (setq-local compile-command cmd))))


(add-hook 'python-mode-hook #'improved-compile-command)
(add-hook 'python-ts-mode-hook #'improved-compile-command)
(add-hook 'rust-mode-hook #'improved-compile-command)
(add-hook 'tuareg-mode-hook #'improved-compile-command)
(add-hook 'c-mode-hook #'improved-compile-command)
(add-hook 'c++-mode-hook #'improved-compile-command)



;; Remove from find-file-hook as it might be too early
(remove-hook 'find-file-hook #'improved-compile-command)

;; Set the default compile-command to nil to ensure our function takes effect
(setq-default compile-command nil)


;; Unbind M-c from capitalize-word
(global-unset-key (kbd "M-c"))

;; Define M-c as a prefix key
(define-prefix-command 'compile-prefix-map)
(global-set-key (kbd "M-c") 'compile-prefix-map)

;; new commands for compile and recompile
(global-set-key (kbd "M-c c") 'compile)
(global-set-key (kbd "M-c m") 'recompile)


(global-set-key (kbd "M-c n") 'next-error)
(global-set-key (kbd "M-c b") 'previous-error)

;; xref find def
(define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
(define-key evil-motion-state-map (kbd "M-.") 'xref-find-definitions)


(evil-define-key 'normal 'global (kbd "SPC d") 'xref-find-definitions-other-window)

;; dired mode helpfuls
(with-eval-after-load 'dired
    (define-key dired-mode-map [mouse-1] 'dired-single-buffer)
    (define-key dired-mode-map [mouse-2] 'dired-single-buffer))

;; Define M-b as a prefix key
(define-prefix-command 'bookmark-prefix-map)
(global-set-key (kbd "M-b") 'bookmark-prefix-map)


(global-set-key (kbd "M-b m") 'bookmark-set)
(global-set-key (kbd "M-b j") 'bookmark-jump)
(global-set-key (kbd "M-b l") 'bookmark-bmenu-list)
(global-set-key (kbd "M-b d") 'bookmark-delete)



;;; custum-commands.el ends here
