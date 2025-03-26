;;; elpa-setup.el starts here


(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)


;; for some reason emacs was not loading a bunch of packages so this is my fix
(add-to-list 'load-path ELPA_PATH)
(dolist (dir (directory-files ELPA_PATH t "^[^.]"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))


;;; elpa-setup.el ends here
