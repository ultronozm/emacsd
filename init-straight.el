;; (setq straight-use-package-by-default t)
;; (setq straight-vc-git-default-protocol 'ssh)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(load "~/.emacs.d/init-bare.el")

(use-package exec-path-from-shell
  :straight exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package copilot
    :straight (copilot :type git :host github :repo "zerolfx/copilot.el"
                       :files ("*.el" "dist"))
    :bind (:map copilot-completion-map
   		("§" . copilot-accept-completion)))


    

;; (straight-use-package 'use-package)

;; (when nil
;;   (use-package copilot-emacsd
;;     :straight (copilot-emacsd :type git :host github :repo
;; 			      "rksm/copilot-emacsd"
;; 			      ))
;; )

;; (when nil
;;   )
 
