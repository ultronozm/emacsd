;;; -*- lexical-binding: t; -*-

;; taken from
;; https://github.com/leanprover/lean-mode/commit/b224da9d2b339514c2577e5ee4c675b03c978bcd
(defun czm-lean4-set-imenu-generic-expression ()
  (setq imenu-generic-expression '(("Inductive" "^ *\\(?:@\\[.*\\]\\)? *inductive +\\([^\n ]+\\)" 1)
                                   ("Function" "^ *\\(?:@\\[.*\\]\\)? *def +\\([^\n ]+\\)" 1)
                                   ("Lemma" "^ *\\(?:@\\[.*\\]\\)? *lemma +\\([^\n ]+\\)" 1)
                                   ("Theorem" "^ *\\(?:@\\[.*\\]\\)? *theorem +\\([^\n ]+\\)" 1)
                                   ("Theorem" "^ *\\(?:@\\[.*\\]\\)? *theorem +\\([^\n ]+\\)" 1)
                                   ("Namespace" "^ *\\(?:@\\[.*\\]\\)? *namespace +\\([^\n ]+\\)" 1))))

;; (with-eval-after-load 'lean4-mode
;;   (define-key lean4-mode-map [?\t] #'company-indent-or-complete-common)
;;   (add-hook 'lean4-mode-hook #'company-mode))


;; eglot-sync-connect?

(use-package lean4-mode
  :ensure (:host github :repo "ultronozm/lean4-mode"
                 :files ("*.el" "data"))
  :hook (lean4-mode . czm-lean4-set-imenu-generic-expression)
  :commands (lean4-mode)
  :custom
  (lean4-keybinding-lean4-toggle-info (kbd "C-c C-y"))
  (lean4-info-plain nil)
  :bind (:map lean4-mode-map
              ("RET" . newline)
              ("C-j" . default-indent-new-line)
              ("C-c C-q" . eglot-code-action-quickfix)
              ("C-M-i" . completion-at-point))
  :config
  :defer t)

(use-package czm-lean4
  :ensure (:host github :repo "ultronozm/czm-lean4.el"
                 :depth nil)
  :after lean4-mode czm-preview
  :hook (lean4-mode . czm-lean4-mode-hook)
  :hook (magit-section-mode . czm-lean4-magit-section-mode-hook)
  :bind (:map lean4-mode-map
              ("C-c v" . czm-lean4-show-variables)
              ("C-c C-p C-p" . czm-lean4-toggle-info-pause)
              ("C-c C-m C-m" . czm-lean4-search-mathlib)
              ("C-c C-m C-h" . czm-lean4-search-mathlib-headings)
              ("C-c C-," . czm-lean4-insert-section-or-namespace)
              ("C-c C-." . czm-lean4-insert-comment-block)
              ("C-c C-i" . czm-lean4-toggle-info-split-below)
              ("C-c C-o" . czm-lean4-toggle-info-split-right)
              ("M-]" . czm-lean4-cycle-delimiter-forward)
              ("§" . copilot-accept-completion)
              ("M-§" . copilot-accept-completion-by-word)
              ("C-§" . copilot-accept-completion-by-line)
              ("C-M-§" . copilot-accept-completion-by-paragraph)
              ("`" . copilot-accept-completion)
              ("M-`" . copilot-accept-completion-by-word)
              ("C-`" . copilot-accept-completion-by-line)
              ("C-M-`" . copilot-accept-completion-by-paragraph)
              ("M-[" . czm-lean4-cycle-delimiter-backward))
  :bind (:map czm-lean4-tex-mode-map
              ("s-f" . czm-lean4-preview-fold-block))
  :custom
  (czm-lean4-info-window-height-fraction 0.4)
  (czm-lean4-info-window-width-fraction 0.47)
  :config
  (advice-add 'lean4-info-buffer-redisplay :around #'czm-lean4-info-buffer-redisplay))


(defun czm-colorize-lean4-signature ()
  "Highlights the name of each required variable to a Lean4 theorem."
  (when
      (with-current-buffer eldoc-icebox-parent-buffer
        (or
         (eq major-mode 'lean4-mode)
         (eq (buffer-name) "*Lean Goal*")))
    (save-excursion
      (goto-char (point-min))
      (while (and (< (point) (point-max))
                  (not (eq (char-after (1+ (point))) ?\:)))
        (forward-list)
        (when (eq (char-before) ?\))
          (save-excursion
            (backward-list)
            (let ((inhibit-read-only t)
                  (start (point))
                  (end (save-excursion (forward-list) (point)))
                  (end-first-symbol (save-excursion (forward-word) (point)))
                  (end-symbols (save-excursion (when (search-forward " : " nil t) (- (point) 3)))))
              (when end-symbols
                (put-text-property start end 'face '(underline))
                                        ; shr-mark doesn't work anymore?
                (put-text-property (1+ start) end-symbols 'face '(highlight underline))))))))))

(defun czm-add-lean4-eldoc ()
  (when
      (with-current-buffer eldoc-icebox-parent-buffer
        (or
         (eq major-mode 'lean4-mode)
         (equal (buffer-name)
                "*Lean Goal*")))
    (add-hook 'eldoc-documentation-functions #'lean4-info-eldoc-function
              nil t)
    (eldoc-mode)))

(use-package eldoc-icebox
  :ensure (:host github :repo "ultronozm/eldoc-icebox.el"
                 :depth nil)
  :bind (("C-c C-h" . eldoc-icebox-store)
         ("C-c C-n" . eldoc-icebox-toggle-display))
  :hook
  (eldoc-icebox-post-display . shrink-window-if-larger-than-buffer)
  (eldoc-icebox-post-display . czm-colorize-lean4-signature)
  (eldoc-icebox-post-display . czm-add-lean4-eldoc))
