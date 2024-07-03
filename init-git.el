;;; -*- lexical-binding: t; -*-

(use-package magit
  :defer t
  :hook
  (magit-status-mode . visual-line-mode))

(use-package repo-scan
  :ensure (:host github :repo "ultronozm/repo-scan.el"
                 :depth nil)
  :defer t)

(defvar czm-repos '(
                    "ai-org-chat"
                    "czm-cpp"
                    "czm-lean4"
                    "czm-misc"
                    "czm-preview"
                    "czm-spell"
                    "czm-tex-compile"
                    "czm-tex-edit"
                    "czm-tex-fold"
                    "czm-tex-jump"
                    "czm-tex-mint"
                    "czm-tex-ref"
                    "czm-tex-util"
                    "dynexp"
                    "eldoc-icebox"
                    "library"
                    "preview-auto"
                    "preview-tailor"
                    "publish"
                    "repo-scan"
                    "spout"
                    "symtex"
                    "auctex-label-numbers"
                    "auctex-cont-latexmk"
                    "tex-parens"
                    "tex-item"
                    ))

(defun czm-repos-uncompiled ()
  (interactive)
  (dolist (name czm-repos)
    (let ((elc-file
           (concat user-emacs-directory
                   (file-name-as-directory "elpaca")
                   (file-name-as-directory "builds")
                   (file-name-as-directory name)
                   name ".elc")))
      (unless (file-exists-p elc-file)
        (message "%s.elc not found" name)))))

(defun czm-pull-my-stuff ()
  (interactive)
  (let* ((repos (append
                 (mapcar
                  (lambda (name)
                    (concat user-emacs-directory
                            (file-name-as-directory "elpaca")
                            (file-name-as-directory "repos")
                            name))
                  czm-repos))))
    (repo-scan-pull repos)))

(defun czm-rebuild-my-stuff ()
  (interactive)
  (dolist (repo czm-repos)
    (let ((repo-symbol (intern repo)))
      (elpaca-rebuild repo-symbol))))



(defun czm-file-is-tex-or-bib (file)
  "Return t if FILE is a .tex or .bib file."
  (or (string-suffix-p ".tex" file)
      (string-suffix-p ".bib" file)))

(use-package publish
  :ensure (:host github :repo "ultronozm/publish.el"
                 :depth nil)
  :defer t
  :custom
  (publish-repo-root "~/math")
  (publish-disallowed-unstaged-file-predicate #'czm-file-is-tex-or-bib))

(defun czm-search-my-repos ()
  (interactive)
  ;; files: all elisp files in my repos
  (let ((files (mapcan
                (lambda (name)
                  (directory-files-recursively
                   (concat user-emacs-directory
                           (file-name-as-directory "elpaca")
                           (file-name-as-directory "repos")
                           name)
                   "\\.el\\'"))
                czm-repos)))
    (consult--grep "Ripgrep" #'consult--ripgrep-make-builder files nil)))
