;;; init.el starts here

;; Constants
(defconst EMACS_PATH "~/.emacs.d/" "Path to .emacs.d")
(defconst ELPA_PATH (concat EMACS_PATH "elpa") "Path to the elpa directory.")
(defconst CONFIG_PATH "~/.config/emacs/" "Path to config directory.")
(defconst PACKAGES_PATH (concat CONFIG_PATH "package-configs/"))
(defconst FILE_TYPE_PATH (concat CONFIG_PATH "file-type-configs/"))


;; Load system setup
(load (concat CONFIG_PATH "setup.el"))

;; Loads packages as a whole
(load (concat CONFIG_PATH "package-list.el"))

;; Load custom commands that are un-associated with packages
(load (concat CONFIG_PATH "custom-commands.el"))

;; Load language modes
(load (concat CONFIG_PATH "file-types.el"))

;; Load homescreen
(load (concat CONFIG_PATH "homescreen.el"))



;;; init.el ends here
