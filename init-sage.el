;;; -*- lexical-binding: t; -*-

(use-package sage-shell-mode
  :defer t
  :custom
  (sage-shell:use-prompt-toolkit nil)
  (sage-shell:use-simple-prompt t)
  (sage-shell:sage-root my-sage-root)
  :bind
  (:map sage-shell-mode-map
        ("C-c n" . czm-sage-worksheet))
  (:map sage-shell:sage-mode-map
        ("C-c n" . czm-sage-worksheet))
  :hook
  ((sage-shell-mode sage-shell:sage-mode) . eldoc-mode)
  (sage-shell-after-prompt . sage-shell-view-mode))

(defun czm-sage-documentation ()
  (interactive)
  (other-window-prefix)
  (eww-open-file
   (concat sage-shell:sage-root
           "/local/share/doc/sage/html/en/index.html")))

(defun czm-sage-worksheet (arg)
  "Create new sage file in my-tmp-sage-dir.
Given prefix ARG, open dired buffer and highlight most recent
entry."
  (interactive "P")
  (if (not arg)
      (let ((filename (format-time-string "%Y%m%dT%H%M%S.sage")))
	       (find-file
         (expand-file-name filename (file-name-as-directory my-tmp-sage-dir))))
    (progn
      (dired
       (expand-file-name "*.sage" (file-name-as-directory my-tmp-sage-dir))
       "-alt"))))

    (use-package ob-sagemath
      :defer t
      :config
      (setq org-babel-default-header-args:sage '((:session . t)
                                                 (:results . "output")))
      (add-hook 'org-babel-after-execute-hook 'org-display-inline-images))


(defun calcFunc-sage-factor ()
  "Use SAGE to factor the top element of the stack in Emacs Calc."
  (interactive)
  (if (equal (length calc-stack) 0)
      (error "Stack is empty"))
  (let* ((top-of-stack (calc-top))
         (top-of-stack-string (math-format-value top-of-stack))
         (sage-code
	         (format "SR(\"%s\").factor()" top-of-stack-string))
         (modified-string (symtex-evaluate sage-code))
         (modified-value (math-read-exprs modified-string)))
    (if (eq (car-safe modified-value) 'error)
        (error "Parsing error: %s" (nth 1 modified-value))
      (calc-pop 1)
      (calc-push (car modified-value)))))

(use-package mmm-mode
  :defer t
  :custom
  (mmm-global-mode 'maybe)
  :config
  (face-spec-set 'mmm-default-submode-face
                 '((((background light)) (:background "#ddffff"))
                   (((background dark)) (:background "#004444")))
                 'face-defface-spec))

(use-package czm-tex-mint
  :ensure (:host github :repo "ultronozm/czm-tex-mint.el"
                 :depth nil)
  :after latex mmm-mode
  :demand t
  :custom
  (LaTeX-command "latex -shell-escape")
  :config
  (czm-tex-mint--initialize)
  :bind
  (:map czm-tex-mint--mode-map
        ("C-c C-c" . czm-tex-mint-evaluate)
        ("C-c C-l" . czm-tex-mint-evaluate-latex))
  :hook
  (mmm-sage-shell:sage-mode-enter . czm-tex-mint--enable)
  (mmm-sage-shell:sage-mode-exit . czm-tex-mint--disable))

(use-package symtex
  :ensure (:host github
                 :repo "ultronozm/symtex.el"
                 :files ("*.el" "*.py")
                 :depth nil
                 :branch "calc")
  :after latex
  :bind
  (:map global-map
        ("C-c V" . symtex-process))
  (:map LaTeX-mode-map
	       ("C-c v" . symtex-dwim)))
