(defconst czm-init-directory
  (file-name-directory
   (or load-file-name
       (buffer-file-name))))

(load (concat czm-init-directory "init-bare.el"))

(load (concat czm-init-directory "init-package.el"))
;; (load (expand-file-name "init-straight.el"))
;; (load (expand-file-name "init-elpaca.el"))
