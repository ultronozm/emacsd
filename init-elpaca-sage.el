(use-package sage-shell-mode
  :custom
  (sage-shell:use-prompt-toolkit nil)
  (sage-shell:use-simple-prompt t)
  (sage-shell:sage-root "~/sage/sage-9.8")
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
  "Create new sage file in ~/doit/sage/.
If given a prefix argument, open a dired buffer to ~/doit/sage
and highlight most recent entry."
  (interactive "P")
  (if (not arg)
      (let ((filename (format-time-string "~/doit/sage/%Y%m%dT%H%M%S.sage")))
	(find-file filename))
    (progn
      (dired "~/doit/sage/*.sage" "-alt"))))



(use-package ob-sagemath
  :config
  ;; Ob-sagemath supports only evaluating with a session.
  (setq org-babel-default-header-args:sage '((:session . t)
                                             (:results . "output")))

  ;; Show images after evaluating code blocks.
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
  :custom
  (mmm-global-mode 'maybe)
  :custom-face
  (mmm-default-submode-face ((t (:background "#ddffff")))))

(use-package sagemintex
  :elpaca (:host github :repo "ultronozm/sagemintex.el"
                 :depth nil)
  :after latex mmm-mode sage-shell-mode
  :custom
  (LaTeX-command "latex -shell-escape")
  :bind
  (:map sagemintex-mode-map
	("C-c C-c" . sagemintex-evaluate)
	("C-c C-l" . sagemintex-evaluate-latex))
  :hook
  (mmm-sage-shell:sage-mode-enter . sagemintex-enable)
  (mmm-sage-shell:sage-mode-exit . sagemintex-disable))

(use-package symtex
  :elpaca (:host github :repo "ultronozm/symtex.el"
                 :depth nil)
  :after latex sage-shell-mode
  :bind
  (:map global-map
        ("C-c V" . symtex-process))
  (:map LaTeX-mode-map
	("C-c v" . symtex-dwim)))


