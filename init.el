;;; init.el starts here

;; Constants
(defconst EMACS_PATH "c:/Users/Roscoe/AppData/Roaming/.emacs.d/" "Path to .emacs.d")
(defconst ELPA_PATH (concat EMACS_PATH "elpa") "Path to the elpa directory.")
(defconst CONFIG_PATH "c:/Users/Roscoe/.config/Emacs-Config/" "Path to config directory.")
(defconst PACKAGES_PATH (concat CONFIG_PATH "package-configs/"))


;; Load system setup
(load (concat CONFIG_PATH "setup.el"))

;; Loads packages as a whole
(load (concat CONFIG_PATH "package-list.el"))

;; Load individual language modes
(load (concat CONFIG_PATH "python-config.el"))
(load (concat CONFIG_PATH "c-config.el"))
(load (concat CONFIG_PATH "latex-config.el"))
(load (concat CONFIG_PATH "markdown-config.el"))

;; Load PDF viewing tools
(load (concat CONFIG_PATH "pdf-config.el"))

;; Load homescreen
(load (concat CONFIG_PATH "homescreen.el"))

;; Load custom commands that are un-associated with packages
(load (concat CONFIG_PATH "custom-commands.el"))

;; Load pensec specific commands
(load (concat CONFIG_PATH "pensec-commands.el"))

;;; init.el ends here
