(setq use-package-verbose t)
(load (concat user-emacs-directory "init-bare.el"))
(load (concat user-emacs-directory "init-settings.el"))
;; (load (concat user-emacs-directory "init-without-latex.el"))
;; (load (concat user-emacs-directory "init-main.el"))


;; (load (concat user-emacs-directory "init-test3.el"))

(when nil
  (load (concat user-emacs-directory "init-git.el"))

  ;; (load (concat user-emacs-directory "latex-minimal.el"))
  (unless (eq window-system 'w32)
    (load (concat user-emacs-directory "init-latex.el"))
    (load (concat user-emacs-directory "init-sage.el"))
    (load (concat user-emacs-directory "init-lean4.el")))
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
