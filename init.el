(setq use-package-verbose t)
(load (concat user-emacs-directory "init-bare.el"))
(load (concat user-emacs-directory "init-settings.el"))

(when t
  (load (concat user-emacs-directory "init-main.el"))
  (load (concat user-emacs-directory "init-git.el"))

  ;; (load (concat user-emacs-directory "init-without-latex.el"))
  ;; (load (concat user-emacs-directory "init-test3.el"))

  ;; (load (concat user-emacs-directory "latex-minimal.el"))


  (unless (eq window-system 'w32)
    (load (concat user-emacs-directory "init-latex.el"))
    (load (concat user-emacs-directory "init-sage.el"))
    (load (concat user-emacs-directory "init-lean4.el"))
    ))
(put 'erase-buffer 'disabled nil)
