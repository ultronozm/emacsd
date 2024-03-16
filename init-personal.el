(use-package emacs
  :elpaca nil
  :custom
  (safe-local-variable-values
   '((czm-preview-TeX-master . "~/doit/preview-master-principal_cg.tex")
     (cmake-build-options . "-j 6")
     (eval add-to-list 'LaTeX-indent-begin-exceptions-list "ifs"))))

(use-package erc
  :elpaca nil
  :custom
  (erc-nick '("czM" "czM_"))
  (erc-user-full-name "Paul Nelson"))

(defun czm/connect-znc ()
  (interactive)
  (require 'erc)
  (require 'auth-source)
  (let* ((znc-server "3.77.70.103")
         (znc-port 1337)
         (znc-username "ultrono")
         (znc-network "quakenet")
         (auth-info (auth-source-search :host znc-server :user (concat znc-username "/" znc-network)))
         (znc-password (funcall (plist-get (car auth-info) :secret))))
    (erc-tls :server znc-server
             :port znc-port
             :nick znc-username
             :password (concat znc-username "/" znc-network ":" znc-password))))

(defvar czm-margin-width
  (if (and
       nil
       (equal system-name "d51735"))
      80
    25))

(defun czm-toggle-margins (&optional width)
  (interactive)
  (unless width
    (setq width czm-margin-width))
  (if (eq left-margin-width 0)
      (progn
        (setq left-margin-width czm-margin-width
              right-margin-width czm-margin-width)
        (set-window-margins (selected-window) width width))
    (setq left-margin-width 0)
    (setq right-margin-width 0)
    (set-window-margins (selected-window) 0 0)))

