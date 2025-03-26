;;; homescreen.el starts here

(use-package dashboard
    :ensure t
    :config
    (dashboard-setup-startup-hook)
    (setq dashboard-startup-banner (concat CONFIG_PATH "emacs_image.png"))
    ;; Method 1: Using custom face
    (custom-set-faces
    '(dashboard-banner-logo-title ((t (:foreground "#2957b0" :weight bold)))))

    ;; Method 2: Using both propertize and setting the face explicitly
    (setq dashboard-banner-logo-title 
        (let ((title "
******** ****     ****     **       ******   ********
/**///// /**/**   **/**    ****     **////** **////// 
/**      /**//** ** /**   **//**   **    // /**       
/******* /** //***  /**  **  //** /**       /*********
/**////  /**  //*   /** **********/**       ////////**
/**      /**   /    /**/**//////**//**    **       /**
/********/**        /**/**     /** //******  ******** 
//////// //         // //      //   //////  ////////  
"))
            (propertize title 'face '(:foreground "red" :weight bold))))

    (setq dashboard-center-content t)
    (setq dashboard-items '((recents  . 12)
                        (bookmarks . 12)))
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))


;;; homescreen.el ends here
