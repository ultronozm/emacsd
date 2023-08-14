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

(use-package exec-path-from-shell
  :straight exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package copilot
    :straight (copilot :type git :host github :repo "zerolfx/copilot.el"
                       :files ("*.el" "dist"))
    :bind (:map copilot-completion-map
   		("§" . copilot-accept-completion)))

(straight-use-package '(auctex :source el-get
                        :files ("*.el" "*.info" "dir"
                                "doc" "etc" "images" "latex" "style")))
;; (require 'tex-site)
;; (require 'preview-latex)

(use-package latex
  ;; :ensure auctex ; makes latex easier to use
  :straight auctex
  :demand t
  :init
  (setq TeX-PDF-mode t) ; compile tex as PDF
  (setq TeX-command-force "LaTex") ; Don’t ask for options like View, Latex etc..
  (setq TeX-view-program-list '(("zathura" "zathura %o"))
            TeX-view-program-selection '((output-pdf "zathura")))
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (setq TeX-source-correlate-start-server t)
  (add-hook 'LaTeX-mode-hook
        (lambda ()
          (add-hook 'after-save-hook 'TeX-command-master nil t)))
  :config
  ;;(my/map-keys `(("C-c l l" ,#'Tex-command-master "pdf")) 'org-mode-map)
)
    

;; (straight-use-package 'use-package)

;; (when nil
;;   (use-package copilot-emacsd
;;     :straight (copilot-emacsd :type git :host github :repo
;; 			      "rksm/copilot-emacsd"
;; 			      ))
;; )

;; (when nil
;;   )
 
