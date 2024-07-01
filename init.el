(setq use-package-verbose t)
(load (concat user-emacs-directory "init-bare.el"))
(load (concat user-emacs-directory "init-settings.el"))

(setq custom-file (concat user-emacs-directory "init-custom.el"))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa-devel" . "https://elpa.gnu.org/devel/"))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(let ((package-check-signature nil))
  (use-package gnu-elpa-keyring-update
    :ensure t
    :demand t))

(package-install 'auctex)

(setq use-package-vc-prefer-newest t)

(use-package exec-path-from-shell
  :ensure
  :demand
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(when t
  (load (concat user-emacs-directory "init-main.el")))

(when t
  (load (concat user-emacs-directory "init-git.el")))

(when t
  (load (concat user-emacs-directory "init-latex.el")))

(when nil
  (load (concat user-emacs-directory "init-sage.el"))
  (load (concat user-emacs-directory "init-lean4.el")))

(find-file (concat user-emacs-directory "init.el"))
