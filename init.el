;;; -*- lexical-binding: t; -*-

;;; --- Preliminaries ---

(setq use-package-verbose t)
(load (concat user-emacs-directory "init-bare.el"))
(load (concat user-emacs-directory "init-settings.el"))

;; disable customization interface
(setq custom-file (concat user-emacs-directory "init-custom.el"))

(defun czm-dired-downloads ()
  "Open the downloads directory in Dired mode."
  (interactive)
  (dired my-downloads-folder))

(defun czm-find-math-document ()
  "Find a file in the math documents folder."
  (interactive)
  (project-find-file-in nil (list my-math-folder) `(local . ,my-math-folder)))

(use-package emacs
  :ensure nil
  :bind
  ("C-c d" . czm-dired-downloads)
  ("s-d" . czm-find-math-document))

;;; --- Elpaca ---

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(when (eq window-system 'w32)
  (elpaca-no-symlink-mode))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :ensure t unless otherwise specified.
  (setq use-package-always-ensure t))

(use-package emacs
  :ensure nil
  :bind
  (:map global-map
        ("s-r" . elpaca-rebuild)))

(elpaca-wait)

;;; --- Exec Path From Shell ---

(use-package exec-path-from-shell
  :demand
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(elpaca-wait)

;;; --- Repeat ---

(use-package define-repeat-map
  :ensure (:host nil :repo "https://tildegit.org/acdw/define-repeat-map.el")
  :demand t
  :config
  (repeat-mode 1))

;;; --- Paragraph Editing ---

(defun fill-previous-paragraph ()
  "Fill the previous paragraph."
  (interactive)
  (save-excursion
    (previous-line)
    (fill-paragraph)))

(with-eval-after-load 'define-repeat-map
  (define-repeat-map paragraph
    ("]" forward-paragraph
     "}" forward-paragraph
     "[" backward-paragraph
     "{" backward-paragraph)
    (:continue
     "M-h" mark-paragraph
     "h" mark-paragraph
     "k" kill-paragraph
     "w" kill-region
     "M-w" kill-ring-save
     "y" yank
     "C-/" undo
     "t" transpose-paragraphs
     "q" fill-previous-paragraph
     "C-l" recenter-top-bottom)))

;;; --- Mode line ---

(use-package diminish
  :demand t
  :config
  (diminish 'abbrev-mode "Ab")
  (diminish 'visual-line-mode)
  (diminish 'outline-minor-mode)
  (diminish 'buffer-face-mode)
  (diminish 'eldoc-mode)
  (diminish 'reftex-mode)
  (diminish 'whitespace-mode))

;; Remove "%n" from mode-line-modes -- I know when I'm narrowing.
(setq mode-line-modes (delete "%n" mode-line-modes))

(use-package emacs
  :ensure nil
  :hook
  (emacs-lisp-mode . (lambda () (setq mode-name "E")))
  (lisp-interaction-mode . (lambda () (setq mode-name "LI"))))

(with-eval-after-load 'tex-mode
  (add-hook 'LaTeX-mode-hook
            (lambda () (setq TeX-base-mode-name "L"))))

;;; --- Org Mode ---

(require 'org)

(use-package emacs
  :ensure nil
  :hook
  (org-mode . visual-line-mode)
  :custom
  (org-default-notes-file my-todo-file)
  (org-directory "~/")
  (org-agenda-files `(,my-todo-file))
  (org-goto-auto-isearch nil)
  (org-agenda-include-diary t)
  (org-babel-load-languages '((latex . t) (emacs-lisp . t) (python . t)))
  (org-babel-python-command "python3")
  (org-confirm-babel-evaluate nil)
  (org-link-elisp-confirm-function nil)
  (org-enforce-todo-dependencies t)
  (org-file-apps '((auto-mode . emacs) ("\\.x?html?\\'" . default)))
  (org-hide-leading-stars t)
  (org-list-allow-alphabetical t)
  (org-odd-levels-only nil)
  (org-refile-targets
   '((org-agenda-files :regexp . "Notes")
     (my-todo-file :regexp . "Inbox")
     (my-todo-file :regexp . "Reference")
     (my-todo-file :regexp . "Someday")
     (my-todo-file :regexp . "Scheduler")
     (my-todo-file :regexp . "Tasks")))
  (org-refile-use-outline-path t)
  ;; should add to list:  (org-speed-commands '(("B" . org-tree-to-indirect-buffer)))
  (org-src-preserve-indentation t)
  (org-tags-column -70)
  (org-use-speed-commands t)
  (org-capture-templates
   '(("i" "Inbox" entry (file+headline my-todo-file "Inbox")
      "* %?\n  %i")
     ("j" "Journal" entry (file+datetree my-log-file)
      "* %?\nEntered on %U\n")
     ("a" "Inbox (annotated)" entry (file+headline my-todo-file "Inbox")
      "* %?\n%a")
     ("k" "Interruptions" entry (file+headline my-todo-file "Interruptions")
      "* %?\n%U\n" :clock-in t :clock-resume t)
     ("d" "Diary" entry (file+datetree simple-journal-db-file)
      "* %U \n%?%i\n" :tree-type week)))
  (org-src-window-setup 'current-window))

(defun czm-org-edit-src ()
  "Edit source block at point, with some customizations.
- Set fill-column to a large number.
- Set TeX-master to my-preview-master."
  (interactive)
  (let ((src-buffer
         (save-window-excursion
           (org-edit-src-code)
           (setq fill-column 999999) ; should this be in a latex mode hook?
           (setq TeX-master my-preview-master)
           (current-buffer))))
    (switch-to-buffer src-buffer)))

(defun czm-search-log ()
  "Search your log files with `rg'."
  (interactive)
  (let ((log-files `(,my-log-file ,my-old-log-file ,my-todo-file)))
    (consult--grep "Ripgrep" #'consult--ripgrep-make-builder log-files nil)))

(use-package emacs
  :ensure nil
  :after define-repeat-map org
  :hook
  (org-mode . (lambda () (setq fill-column 999999)))
  (org-mode . abbrev-mode)
  :bind
  (:map org-mode-map
        ("C-c 1" .
         (lambda() (interactive)
           (progn
             (insert "#+begin_src latex")
             (newline))))
        ("C-c 2" .
         (lambda() (interactive)
           (progn
             (insert "#+end_src")
             (newline))))
        ("C-c p" . czm-org-edit-src)
        ("M-{" . org-backward-paragraph)
        ("M-}" . org-forward-paragraph))
  :config
  (define-repeat-map org-paragraph
    ("]" org-forward-paragraph
     "}" org-forward-paragraph
     "[" org-backward-paragraph
     "{" org-backward-paragraph)
    (:continue
     "k" kill-paragraph
     "w" kill-region
     "M-w" kill-ring-save
     "y" yank
     "C-/" undo
     "t" transpose-paragraphs
     "q" fill-previous-paragraph))
  (repeat-mode 1)
  (require 'ob-shell))

;;; --- Personal Config ---

(when (file-exists-p (concat user-emacs-directory "init-personal.el"))
  (load (concat user-emacs-directory "init-personal.el")))

;;; --- UI Enhancements ---

(use-package avy
  :custom
  (avy-single-candidate-jump nil)
  :config
  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-embark)
  (setf (alist-get ?w avy-dispatch-alist) 'avy-action-easy-kill)
  :bind
  (:map global-map
        ("C-'" . avy-goto-char-timer)
        ("C-;" . avy-goto-line)
        ("C-c g" . avy-goto-line))
  (:map isearch-mode-map
        ("M-j" . avy-isearch)))

(use-package emacs
  :ensure nil
  :after org
  :bind
  (:map org-mode-map
        ("C-'" . nil) ; disable because it is used above
        ))

(use-package czm-misc
  :ensure (:host github :repo "ultronozm/czm-misc.el"
                 :depth nil)
  :bind (("s-@" . czm-misc-split-window-below-variant)
         ("s-#" . czm-misc-split-window-right-variant)
         ("s-4" . czm-misc-double-split-window-below-and-delete)
         ("s-5" . czm-misc-double-split-window-right-and-delete)
         ("s-6" . czm-misc-delete-indentation-nil)
         ("s-7" . czm-misc-delete-indentation-t)
         ("s-8" . czm-misc-show-overlays-at-pt)
         ("C-w" . czm-misc-kill-or-delete-region)
         ("C-x c" . czm-misc-clone-indirect-buffer-same-window)
         ("M-o" . czm-misc-split-line-below)
         ("C-S-SPC" . czm-misc-delete-horizontal-space-on-line)
         ("s-j" . czm-misc-avy-jump)
         ("s-c" . czm-misc-avy-copy)
         ("C-x j" . czm-misc-dired-popup))
  (:map minibuffer-local-map
        ("C-c d" . czm-misc-insert-date)))

(use-package pulsar
  :bind (("s-l" . pulsar-pulse-line))
  :config
  (dolist (fn '(avy-goto-line
                diff-hunk-prev
                diff-hunk-next
                diff-file-next
                diff-file-prev
                flycheck-previous-error
                flycheck-next-error))
    (add-to-list 'pulsar-pulse-functions fn))
  (pulsar-global-mode))

(use-package vertico
  :demand
  :config
  (vertico-mode))

(use-package marginalia
  :demand
  :config
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package orderless
  :demand
  :custom
  (completion-styles '(orderless basic)))

(use-package consult
  :bind (("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ("C-x M-:" . consult-complex-command)
         ("s-b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ("M-y" . consult-yank-pop)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

(use-package consult-company)

(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  ;; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package info-colors
  :ensure (:host github :repo "ubolonton/info-colors")
  :hook (Info-selection . info-colors-fontify-node))

(use-package ace-window
  :bind
  ("C-x o" . ace-window))

;;; --- Other Utilities ---

(use-package perfect-margin
  :defer t
  :diminish
  :bind ("H-m" . perfect-margin-mode))

(use-package easy-kill
  :bind ([remap kill-ring-save] . easy-kill))

(use-package wgrep) ;; use C-c C-p in embark export following ripgrep

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(use-package ace-link ; activate using 'o' in info/help/(...)
  :config
  (ace-link-setup-default))

(use-package eldoc-box
  :commands (eldoc-box-help-at-point)
  :bind
  (:map global-map ("C-c e" . eldoc-box-help-at-point)))

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

(use-package pos-tip
  :defer t)

(use-package go-translate
  :defer t
  :custom
  gts-translate-list '(("fr" "en"))
  :config
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-google-engine) ;; (gts-bing-engine)
                        )
         :render (gts-buffer-render))))

(use-package rust-mode
  :defer t
  :hook
  (rust-mode . eglot-ensure))

(use-package xr
  :defer t)

;;; --- AI-Powered Tools ---

(use-package copilot
  :ensure (:host github
                 :repo "zerolfx/copilot.el"
                 ;; :repo "ultronozm/copilot.el"
                 :files ("*.el" "dist")
                 :depth nil)
  :diminish " Co"
  :hook
  ((prog-mode LaTeX-mode git-commit-mode) . copilot-mode)
  (emacs-lisp-mode . (lambda () (setq tab-width 1)))
  (lean4-mode . (lambda () (setq tab-width 2)))
  :config
  (add-to-list 'warning-suppress-types '(copilot copilot-exceeds-max-char))
  (copilot--define-accept-completion-by-action
   copilot-accept-completion-by-sentence #'forward-sentence)
  :custom
  (copilot-indent-offset-warning-disable t)
  :bind
  (:map global-map
        ("H-x" . copilot-mode)
        ("§" . copilot-accept-completion))
  (:map copilot-completion-map
        ("§" . copilot-accept-completion)
        ("M-§" . copilot-accept-completion-by-word)
        ("C-§" . copilot-accept-completion-by-line)
        ("s-§" . copilot-accept-completion-by-sentence)
        ("C-M-§" . copilot-accept-completion-by-paragraph)
        ("`" . nil)
        ("M-`" . copilot-accept-completion-by-word)
        ("C-`" . copilot-accept-completion-by-line)
        ("s-`" . copilot-accept-completion-by-sentence)
        ("C-M-`" . copilot-accept-completion-by-paragraph)
        ("C-M-<down>" . copilot-next-completion)
        ("C-M-<up>" . copilot-previous-completion)))

(use-package llm
  :ensure (:host github :repo "ahyatt/llm"
                 :depth nil)
  :init
  (require 'llm-openai)
  (require 'llm-claude)
  (require 'llm-gemini)
  (require 'llm-ollama)
  :custom
  (llm-warn-on-nonfree nil)
  (llm-log t)
  :config
  (add-to-list 'warning-suppress-types '(llm)))

(use-package ai-org-chat
  :ensure (:host github :repo "ultronozm/ai-org-chat.el"
                 :depth nil)
  :defer t
  :bind
  (:map global-map
        ("s-/" . ai-org-chat-new))
  (:map ai-org-chat-minor-mode-map
        ("s-<return>" . ai-org-chat-respond)
        ("C-c n" . ai-org-chat-branch)
        ("C-c e" . ai-org-chat-compare))
  :commands
  (ai-org-chat-setup-buffer ai-org-chat-minor-mode)
  :custom
  (ai-org-chat-user-name my-first-name)
  (ai-org-chat-dir my-scratch-gpt-dir)
  (ai-org-chat-context-style nil)
  :config
  (ai-org-chat-select-model "sonnet 3.5"))

;;; --- Lisp Development ---

(use-package lispy
  :after define-repeat-map
  :commands (lispy-comment
             lispy-multiline
             lispy-split
             lispy-join
             lispy-slurp-or-barf-right
             lispy-slurp-or-barf-left
             lispy-splice
             lispy-clone
             lispy-tab
             lispy-move-up
             lispy-move-down)
  :bind
  (:map emacs-lisp-mode-map
        (";" . czm-lispy-comment-maybe)
        ("M-1" . lispy-describe-inline)
        ("M-2" . lispy-arglist-inline)))

(defun czm-lispy-comment-maybe ()
  "Comment the list at point, or self-insert."
  (interactive)
  (if (looking-at "(")
      (lispy-comment)
    (call-interactively #'self-insert-command)))

(use-package emacs
  :ensure nil
  :after define-repeat-map
  :config
  (define-repeat-map structural-edit
    ("n" forward-list
     "p" backward-list
     "u" backward-up-list
     "M-u" up-list
     "g" down-list)
    (:continue
     "M-g" backward-down-list
     "f" forward-sexp
     "b" backward-sexp
     "a" beginning-of-defun
     "e" end-of-defun
     "k" kill-sexp
     "x" eval-last-sexp
     "m" lispy-multiline
     "j" lispy-split
     "+" lispy-join
     ">" lispy-slurp-or-barf-right
     "<" lispy-slurp-or-barf-left
     "C-/" undo
     "/" lispy-splice
     ";" lispy-comment
     "t" transpose-sexps
     "w" kill-region
     "M-w" kill-ring-save
     "y" yank
     "c" lispy-clone
     "C-M-SPC" mark-sexp
     "RET" newline-and-indent
     "i" lispy-tab
     "<up>" lispy-move-up
     "<down>" lispy-move-down))
  (repeat-mode 1))

(defun czm-edebug-eval-hook ()
  (lispy-mode 0)
  (copilot-mode 0)
  (aggressive-indent-mode 0))

(add-hook 'edebug-eval-mode-hook #'czm-edebug-eval-hook)

;;; --- xref advice for project-only searches ---

(defun czm-xref-restrict-to-project-advice (orig-fun &rest args)
  "Advice to restrict xref searches to the current project root."
  (let ((project-vc-external-roots-function #'ignore))
    (apply orig-fun args)))

(define-minor-mode czm-xref-project-only-mode
  "Toggle xref searches between project-only and including external roots."
  :global t
  :lighter " XPO"
  (if czm-xref-project-only-mode
      (advice-add 'xref-find-references :around #'czm-xref-restrict-to-project-advice)
    (advice-remove 'xref-find-references #'czm-xref-restrict-to-project-advice)))

;;; --- Flycheck / Flymake ---

(use-package flycheck 
  :defer t
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (define-repeat-map flycheck-repeat-map
    ("C-c" flycheck-compile
     "C-w" flycheck-copy-errors-as-kill
     "?" flycheck-describe-checker
     "C" flycheck-clear
     "H" display-local-help
     "V" flycheck-version
     "c" flycheck-buffer
     "e" flycheck-explain-error-at-point
     "f" attrap-flycheck
     "h" flycheck-display-error-at-point
     "i" flycheck-manual
     "l" flycheck-list-errors
     "n" flycheck-next-error
     "p" flycheck-previous-error
     "s" flycheck-select-checker
     "v" flycheck-verify-setup
     "x" flycheck-disable-checker))
  (repeat-mode 1))

(use-package flycheck-package
  :defer t
  :hook
  (emacs-lisp-mode . flycheck-package-setup))

(use-package flymake
  :ensure nil
  :custom
  (flymake-mode-line-lighter "F")
  (flymake-show-diagnostics-at-end-of-line t)
  :after define-repeat-map
  :config
  (define-repeat-map flymake-repeat-map
    ("n" flymake-goto-next-error
     "p" flymake-goto-prev-error
     "f" attrap-flymake
     "M-n" flymake-goto-next-error
     "M-p" flymake-goto-prev-error
     "l" flymake-show-diagnostics-buffer))
  (repeat-mode 1)
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)))

(use-package emacs
  :ensure nil
  :after flymake preview
  :config
  (add-to-list 'preview-auto-reveal-commands #'flymake-goto-next-error)
  (add-to-list 'preview-auto-reveal-commands #'flymake-goto-prev-error))

(use-package emacs
  :ensure nil
  :after flymake tex-fold
  :config
  (add-to-list 'TeX-fold-auto-reveal-commands #'flymake-goto-next-error)
  (add-to-list 'TeX-fold-auto-reveal-commands #'flymake-goto-prev-error))

;;; --- Attrap ---

(use-package attrap
  :defer t
  :after flycheck
  :config
  (setq saved-match-data nil))

(use-package emacs
  :ensure nil
  :after flycheck attrap repeat
  :config
  (define-key flycheck-command-map "f" 'attrap-flycheck))

;;; --- Code Formatting and Indentation ---

(use-package aggressive-indent
  :defer t
  :diminish
  :hook
  ((emacs-lisp-mode LaTeX-mode rust-mode c++-mode) . aggressive-indent-mode))

;;; --- LSP ---

(use-package eglot
  :bind
  (:map eglot-mode-map
        ("C-c C-q" . eglot-code-action-quickfix)
        ("C-c C-a" . eglot-code-actions)))

;;; --- Outline Navigation ---

(defun foldout-exit-fold-without-hiding ()
  (interactive)
  (foldout-exit-fold -1))

(use-package foldout
  :ensure nil)

(use-package outline
  :ensure nil
  :defer t
  :after define-repeat-map
  :config
  (define-repeat-map outline-repeat-map
    ("n" outline-next-heading
     "p" outline-previous-heading
     "u" outline-up-heading
     "f" outline-forward-same-level
     "b" outline-backward-same-level
     "<left>" outline-promote
     "<right>" outline-demote
     "<up>" outline-move-subtree-up
     "<down>" outline-move-subtree-down
     "x" foldout-exit-fold-without-hiding
     "z" foldout-zoom-subtree
     "a" outline-show-all
     "c" outline-hide-entry
     "d" outline-hide-subtree
     "e" outline-show-entry
     "TAB" outline-show-children
     "k" outline-show-branches
     "l" outline-hide-leaves
     "RET" outline-insert-heading
     "o" outline-hide-other
     "q" outline-hide-sublevels
     "s" outline-show-subtree
     "t" outline-hide-body
     "@" outline-mark-subtree)
    (:continue
     "C-M-SPC" outline-mark-subtree
     "w" kill-region
     "M-w" kill-ring-save
     "C-/" undo
     "y" yank))
  (repeat-mode 1))

;;; --- Abbreviations and Spelling ---

;; This could be its own package, accommodating git-friendly abbrev storage?
;; Need a good way to update the source.
(defun modify-abbrev-table (table abbrevs)
  "Define abbreviations in TABLE given by ABBREVS."
  (unless table
    (error "Abbrev table does not exist" table))  ;; Message could be improved
  (dolist (abbrev abbrevs)
    (define-abbrev table (car abbrev) (cadr abbrev) (caddr abbrev))))

(use-package czm-spell
  :ensure (:host github :repo "ultronozm/czm-spell.el"
                 :depth nil)
  :after latex
  :bind ("s-;" . czm-spell-then-abbrev))

;;; --- PDF ---

(use-package doc-view
  :ensure nil
  :bind (:map doc-view-mode-map ("C-c g" . doc-view-goto-page)))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (global-auto-revert-ignore-modes '(pdf-view-mode))
  (pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
  (pdf-annot-tweak-tooltips nil)
  :config
  (pdf-tools-install :no-query)
  (require 'pdf-occur)
  :bind
  (:map pdf-view-mode-map
        ("j" . pdf-view-jump-to-register)
        ("y" . image-previous-line)
        ("<down>" . nil)
        ("<up>" . nil)
        ("<remap> <scroll-up-command>" . pdf-view-scroll-up-or-next-page)
        ("<remap> <scroll-down-command>" . pdf-view-scroll-down-or-previous-page)
        ("C-c g" . pdf-view-goto-page)))

(use-package doc-dual-view
  :ensure (:host github :repo "ultronozm/doc-dual-view.el"
                 :depth nil)
  :commands (doc-dual-view-mode))

;;; --- ERC (IRC Client) ---

(use-package erc
  :ensure nil
  :defer t
  ;; :hook
  ;; (erc-insert-post-hook . erc-save-buffer-in-logs)
  :config
  (erc-timestamp-mode)
  ;; (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  ;;   (save-some-buffers t (lambda () (when (and (eq major-mode 'erc-mode)
  ;;                                              (not (null buffer-file-name)))))))
  (defun oz/escape-applescript (str)
    "Quote \\ and \"."
    (replace-regexp-in-string "\\(\\\\\\|\"\\)" "\\\\\\1" str))
  (defun oz/erc-notifications-notify (orig-fun nick msg)
    "Notify that NICK send some MSG via AppleScript."
    (ns-do-applescript
     (concat "display notification \"" (oz/escape-applescript msg)
             "\" with title \"" (oz/escape-applescript nick) "\"")))
  (advice-add 'erc-notifications-notify :around #'oz/erc-notifications-notify)
  :custom
  (erc-join-buffer 'bury)
  (erc-timestamp-format "[%R-%m/%d]")
  ;; (erc-join-buffer 'window)
  )

;; (use-package erc-join
;;   :after erc
;;   :config
;;   (erc-autojoin-mode)
;;   :custom
;;   ;; (erc-autojoin-channels-alist '(("quakenet.org" "#ultrono")))
;;   )

;; (use-package erc-netsplit
;;   :after erc
;;   :config
;;   (erc-netsplit-mode))


;; logging doesn't seem to be working; not sure what the story is there.
(use-package erc-log
  :ensure nil
  :defer t
  :after erc
  :config
  (erc-log-mode)
  :custom
  (erc-log-channels-directory "~/.erc/logs/")
  (erc-log-insert-log-on-open t)
  (erc-log-write-after-send t)
  (erc-log-write-after-insert t))

;; (use-package erc-ring
;;   :after erc
;;   :config
;;   (erc-ring-mode))
;; (use-package erc-netsplit
;;   :after erc
;;   :config
;;   (erc-netsplit-mode))

(use-package erc-desktop-notifications
  :ensure nil
  :defer t
  :after erc
  ;; https://emacs.stackexchange.com/questions/28896/how-to-get-notifications-from-erc-in-macos
  )

;; (erc-modules
;;    '(autojoin button completion desktop-notifications fill imenu irccontrols list log match menu move-to-prompt netsplit networks noncommands notifications readonly ring stamp track))

;; use erc-select multiple times to connect to multiple IRC servers?

;; TODO: robust form of check-abbrev?

;;; --- C++ ---

(c-add-style
 "llvm4"
 '("gnu"
   (c-basic-offset . 2)     ; Guessed value
   (c-offsets-alist
    (access-label . -)         ; Guessed value
    (block-close . 0)          ; Guessed value
    (class-close . 0)          ; Guessed value
    (defun-block-intro . ++) ; Guessed value
    ;; (defun-block-intro . ++)     ; Guessed value
    (inclass . ++)  ; Guessed value
    (inline-close . 0)      ; Guessed value
    ;; (inline-close . 0)                   ; Guessed value
    (statement . 0)        ; Guessed value
    (statement-block-intro . ++) ; Guessed value
    (statement-cont . llvm-lineup-statement) ; Guessed value
    ;; (statement-cont . ++)                ; Guessed value
    (substatement . ++)        ; Guessed value
    (topmost-intro . nil)      ; Guessed value
    (topmost-intro-cont . +) ; Guessed value
    (annotation-top-cont . 0)
    (annotation-var-cont . +)
    (arglist-close . c-lineup-close-paren)
    (arglist-cont c-lineup-gcc-asm-reg 0)
    ;; (arglist-cont-nonempty c-lineup-gcc-asm-reg 0)
    (arglist-cont-nonempty . c-lineup-arglist)
    (arglist-intro . ++)
    ;; (arglist-intro . c-lineup-arglist-intro-after-paren)
    (block-open . 0)
    (brace-entry-open . 0)
    (brace-list-close . 0)
    (brace-list-entry . c-lineup-string-cont)
    (brace-list-intro first c-lineup-2nd-brace-entry-in-arglist c-lineup-class-decl-init-+ +)
    (brace-list-open . +)
    (c . c-lineup-C-comments)
    (case-label . 0)
    (catch-clause . 0)
    (class-open . 0)
    (comment-intro . c-lineup-comment)
    (composition-close . 0)
    (composition-open . 0)
    (cpp-define-intro c-lineup-cpp-define +)
    (cpp-macro . -1000)
    (cpp-macro-cont . +)
    (defun-close . 0)
    (defun-open . 0)
    (do-while-closure . 0)
    (else-clause . 0)
    (extern-lang-close . 0)
    (extern-lang-open . 0)
    (friend . 0)
    (func-decl-cont . +)
    (incomposition . +)
    (inexpr-class . +)
    (inexpr-statement . +)
    (inextern-lang . +)
    (inher-cont . c-lineup-multi-inher)
    (inher-intro . +)
    (inlambda . 0)
    (inline-open . 0)
    (inmodule . +)
    (innamespace . +)
    (knr-argdecl . 0)
    (knr-argdecl-intro . 5)
    (label . 0)
    (lambda-intro-cont . +)
    (member-init-cont . c-lineup-multi-inher)
    (member-init-intro . 4)
    (module-close . 0)
    (module-open . 0)
    (namespace-close . 0)
    (namespace-open . 0)
    (objc-method-args-cont . c-lineup-ObjC-method-args)
    (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
    (objc-method-intro .
                       [0])
    (statement-case-intro . ++)
    (statement-case-open . +)
    (stream-op . c-lineup-streamop)
    (string . c-lineup-string-cont)
    (substatement-label . 0)
    (substatement-open . 0)
    (template-args-cont c-lineup-template-args +))))

(defun czm-c-mode-common-hook ()
  (c-set-style "llvm4")
  (set-fill-column 120)
  (setq next-error-function #'flymake-goto-next-error))

(use-package emacs
  :ensure nil
  :after cc-mode
  :bind
  ("C-c M-o" . ff-find-other-file)
  :hook
  (c-mode-common . c-toggle-hungry-state)
  (c-mode-common . abbrev-mode)
  (c-mode-common . czm-c-mode-common-hook)
  :config
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)))

;; (use-package clang-format+
;;   :after clang-format
;;   :hook
;;   (c-mode-common . clang-format+-mode))

(use-package cmake-build
  :ensure (:host github :repo "ultronozm/cmake-build.el"
                 :depth nil)
  :bind (("s-m m" . cmake-build-menu)
         ("s-m 1" . cmake-build-set-cmake-profile)
         ("s-m 2" . cmake-build-clear-cache-and-configure)
         ("s-m 3" . cmake-build-set-config)
         ("s-m b" . cmake-build-current)
         ("s-m o" . ff-find-other-file)
         ("s-m r" . cmake-build-run)
         ("s-m c" . cmake-build-clean))
  :custom
  (cmake-build-override-compile-keymap nil)
  (cmake-build-export-compile-commands t)
  (cmake-build-options "-j 1")
  (cmake-build-options "-j 2")
  (cmake-build-options "-j 16")
  (cmake-build-options "-j 8 --verbose"))

(use-package czm-cpp
  :ensure (:host github :repo "ultronozm/czm-cpp.el"
                 :files ("*.el" "template")
                 :depth nil)
  :defer t
  :custom
  (czm-cpp-scratch-directory my-scratch-cpp-dir))

(add-to-list 'auto-mode-alist '("\\.ixx\\'" . c++-mode))

(use-package c-ts-mode
  :ensure nil ;; emacs built-in
  :preface
  (defun my--c-ts-indent-style()
    "Override the built-in BSD indentation style with some additional rules.
         Docs: https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Indentation.html
         Notes: `treesit-explore-mode' can be very useful to see where you're at in the tree-sitter tree,
                especially paired with `(setq treesit--indent-verbose t)' to debug what rules is being
                applied at a given point."
    `(;; do not indent preprocessor statements
      ((node-is "preproc") column-0 0)
      ;; do not indent namespace children
      ((n-p-gp nil nil "namespace_definition") grand-parent 0)
      ;; append to bsd style
      ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))
  :config
  (setq c-ts-mode-indent-offset 2)
  (setq c-ts-mode-indent-style #'my--c-ts-indent-style))

;;; --- Emacs Calc ---

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

;; (defun my-math-read-expr-filter (list-args)
;;   ;; Apply function foo to the first argument of the list
;;   (when (eq calc-language 'latex)
;;     (setcar list-args (symtex--preprocess-for-calc (car list-args))))
;;   ;; Return the modified list
;;   list-args)

;; (advice-add 'math-read-expr :filter-args #'my-math-read-expr-filter)
;; (czm-misc-show-advice #'math-read-expr)
;; (advice-remove 'math-read-expr #'my-math-read-expr-filter)

(defun czm-calc-grab-TeX-region (beg end arg)
  (interactive "r\nP")
  (with-calc-language 'latex
                      (calc-grab-region beg end arg))
  (calc-refresh))

(defmacro with-calc-language (lang &rest body)
  "Execute the forms in BODY with `calc-language` set to LANG.
The value of `calc-language` is restored after BODY has been processed."
  `(let ((old-lang calc-language))
     (unwind-protect
         (progn
           (calc-set-language ,lang)
           ,@body)
       (calc-set-language old-lang))))

;;; --- Git ---

(use-package magit
  :defer t
  :hook
  (magit-status-mode . visual-line-mode))

(use-package repo-scan
  :ensure (:host github :repo "ultronozm/repo-scan.el"
                 :depth nil)
  :defer t)

(defvar czm-repos
  '(
    "ai-org-chat"
    "auto-hide"
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
    "doc-dual-view"
    "dynexp"
    "eldoc-icebox"
    "flymake-overlays"
    "lean4-mode"
    "library"
    "magit-fill-column"
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

(use-package magit-fill-column
  :ensure (:host github :repo "ultronozm/magit-fill-column.el"
                 :depth nil)
  :hook (git-commit-setup . magit-fill-column-set)
  :custom
  (magit-fill-column-alist '(("emacs" . 64)
                             ("auctex" . 64)
                             ("tex-parens" . 64))))

(use-package git-commit
  :ensure nil
  :bind
  (:map git-commit-mode-map
        ("C-c C-l" . magit-generate-changelog)))

(use-package diff-hl
  :defer t)

;;; --- LaTeX ---

(use-package latex
  :ensure (auctex
           :host nil :repo "https://git.savannah.gnu.org/git/auctex.git"
           :depth nil
           :pre-build (("./autogen.sh")
                       ("./configure"
                        "--without-texmf-dir"
                        "--with-packagelispdir=./"
                        "--with-packagedatadir=./"
                        "--with-lispdir=.")
                       ("make"))
           :build (:not elpaca--compile-info) ;; Make will take care of this step
           :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
           :version (lambda (_) (require 'tex-site) AUCTeX-version))
  :demand                             ; otherwise, madness ensues.
  :config
  (setq TeX-data-directory (expand-file-name "elpaca/builds/auctex" user-emacs-directory))
  (setq TeX-lisp-directory TeX-data-directory)
  (add-to-list 'TeX-file-extensions "tex\\.~[0-9a-f]+~")
  (with-eval-after-load 'org-src
    (push '("latex" . LaTeX) org-src-lang-modes))
  (put 'LaTeX-narrow-to-environment 'disabled nil)
  (TeX-source-correlate-mode)
  :hook
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . (lambda ()
                  (apply
                   #'LaTeX-add-environments
                   (mapcar (lambda (env) (list env 'LaTeX-env-label))
                           '("lemma" "exercise" "example" "proposition"
                             "corollary" "remark" "definition" "theorem"
                             "proof")))))
  (LaTeX-mode . (lambda ()
                  (setq buffer-face-mode-face
                        '(:height 216 :width normal :family "Andale Mono"))
                  (buffer-face-mode)))
  (LaTeX-mode . outline-minor-mode)
  (LaTeX-mode . abbrev-mode)
  (LaTeX-mode . visual-line-mode)
  (LaTeX-mode . (lambda () (setq fill-column 999999)))
  (LaTeX-mode . czm-setup-and-activate-tex-fold)
  :bind
  (:map LaTeX-mode-map
        ("s-c" . preview-clearout-at-point)
        ("s-q" . LaTeX-fill-buffer)
        ("C-c m" . latex-math-from-calc)
        ("C-c C-g" . czm-latex-calc-grab)
        ("C-c C-n" . nil) ; TeX-normal-mode
        ("C-c #" . nil)
        ([remap next-error])
        ([remap previous-error])
        ("M-n" . next-error)
        ("M-p" . previous-error))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-outline-extra
   (mapcar
    (lambda (env) (list env 1))
    '("\\\\bibliography\\b" "\\\\printindex\\b" "\\\\begin{thebibliography")))
  (LaTeX-section-hook '(LaTeX-section-heading
                        LaTeX-section-title
                        LaTeX-section-section
                        (lambda ()
                          (forward-line -1)
                          (let ((beg (line-beginning-position))
                                (end (line-end-position)))
                            (font-lock-fontify-region beg end)
                            (TeX-fold-region beg end))
                          (forward-line 1))))
  
  (preview-auto-cache-preamble t)
  (preview-image-type 'dvipng)
  (TeX-fold-quotes-on-insert t)
  (TeX-fold-bib-files (list my-master-bib-file))
  (TeX-ignore-warnings "Package hyperref Warning: Token not allowed in a PDF string")
  ;; (TeX-suppress-ignored-warnings t)
  :custom-face
  (preview-face ((t (:background unspecified)))))

(use-package preview-tailor
  :ensure (:host github :repo "ultronozm/preview-tailor.el"
                 :depth nil)
  :after preview
  :demand
  :config
  (preview-tailor-init)
  :hook
  (kill-emacs . preview-tailor-save)
  :custom
  (preview-tailor-additional-factor-function
   (lambda () (if (string-suffix-p ".lean" (buffer-file-name)) 0.6 0.833))))

(use-package czm-tex-util
  :ensure (:host github :repo "ultronozm/czm-tex-util.el"
                 :depth nil)
  :after latex)

(defun czm-tex-fold-macro-previous-word ()
  (interactive)
  (if TeX-fold-mode
      (save-excursion
        (backward-word)
        (TeX-fold-item 'macro))))

(defun my-yank-after-advice (&rest _)
  "Fold any yanked ref or eqref."
  (when (and (eq major-mode 'LaTeX-mode)
             TeX-fold-mode
             (string-match "\\\\\\(ref\\|eqref\\){\\([^}]+\\)}"
                           (current-kill 0)))
    (czm-tex-fold-macro-previous-word)))

(defun my-latex-fold-current-environment (&rest _)
  "Fold the current LaTeX environment after `LaTeX-environment' is called."
  (when (derived-mode-p 'LaTeX-mode)
    (save-excursion
      (let ((begin (save-excursion (LaTeX-find-matching-begin) (point)))
            (end (save-excursion (LaTeX-find-matching-end) (point))))
        (TeX-fold-region begin end)))))

(advice-add 'LaTeX-environment :after #'my-latex-fold-current-environment)

(defun czm-setup-and-activate-tex-fold ()
  (require 'czm-tex-jump)
  (require 'czm-tex-ref)
  ;; (czm-tex-fold-set-defaults)
  (dolist (item '(("[{2}]||[href]" ("href"))
                  ("🌱" ("documentclass"))
                  ("🌌" ("input"))
                  ("📚" ("bibliography"))
                  ("📖" ("bibliographystyle"))
                  ("✅" ("leanok"))))
    (add-to-list 'TeX-fold-macro-spec-list item))
  (dolist (item '((("🌅" . "🌇") ("document"))
                  (("⚡" . "⚡") ("minted" "minted*"))
                  (("♣" . "♣") ("results" "results*"))
                  ((TeX-fold-format-theorem-environment . "◼")
                   ("idea" "solution"))))
    (add-to-list 'TeX-fold-begin-end-spec-list item))
  (dolist (item (list #'TeX-fold-quotes #'TeX-fold-dashes))
    (add-to-list 'TeX-fold-region-functions item))
  (advice-add 'LaTeX-insert-item :after #'czm-tex-fold-macro-previous-word)
  (advice-add 'yank :after #'my-yank-after-advice)
  (TeX-fold-mode 1)
  (auctex-label-numbers-mode 1)
  (remove-hook 'LaTeX-mode-hook #'czm-setup-and-activate-tex-fold)
  (add-hook 'LaTeX-mode-hook #'TeX-fold-mode))

(use-package czm-tex-jump
  :ensure (:host github :repo "https://github.com/ultronozm/czm-tex-jump.el.git"
                 :depth nil)
  ;; :after avy
  :after latex
  :bind
  (:map LaTeX-mode-map
        ("s-r" . czm-tex-jump))
  :hook (LaTeX-mode . czm-tex-jump-setup))

(use-package czm-tex-ref
  :ensure (:host github :repo "ultronozm/czm-tex-ref.el"
                 :depth nil)
  :after latex
  :custom
  (czm-tex-ref-master-bib-file my-master-bib-file)
  (czm-tex-ref-rearrange-bib-entries t)
  (czm-tex-ref-labelable-environments '("align" "gather" "flalign" "multline" "lemma" "exercise" "example" "proposition" "corollary" "remark" "definition" "theorem" "eqnarray" "equation" "conjecture" "question" "figure" "table" "problem" "fact" "rem" "prop"))
  :bind
  (:map global-map
        ("C-c 0" . czm-tex-ref-bib))
  (:map LaTeX-mode-map
        ("C-c 9" . czm-tex-ref-label)
        ("C-c 0" . czm-tex-ref-bib)))

(defun czm-attrap-LaTeX-fixer-flymake (msg pos end)
  (cond
   ((s-matches? (rx "Use either `` or '' as an alternative to `\"'.")
                msg)
    (list (attrap-option 'fix-open-dquote
                         (delete-region pos (1+ pos))
                         (insert "``"))
          (attrap-option 'fix-close-dquote
                         (delete-region pos (1+ pos))
                         (insert "''"))))
   ((s-matches? (rx "Non-breaking space (`~') should have been used.")
                msg)
    (attrap-one-option 'non-breaking-space
                       (if (looking-at (rx space))
                           (delete-region pos (1+ pos))
                         (delete-region (save-excursion (skip-chars-backward "\n\t ")
                                                        (point))
                                        (point)))
                       (insert "~")))
   ((s-matches? (rx "Interword spacing (`\\ ') should perhaps be used.")
                msg)
    (attrap-one-option 'use-interword-spacing
                       (delete-region (point)
                                      (1+ (point)))
                       (insert "\\ ")))
   ((s-matches? (rx "Intersentence spacing (`\\@') should perhaps be used.")
                msg)
    (attrap-one-option 'use-intersentence-spacing
                       (insert "\\@")))
   ((s-matches? (rx "Delete this space to maintain correct pagereferences.")
                msg)
    ;; not yet fixed
    (attrap-one-option 'fix-space-pageref
                       (if (looking-back (rx bol (* space)))
                           (progn (skip-chars-backward "\n\t ")
                                  (insert "%"))
                         (delete-region (point)
                                        (save-excursion (skip-chars-forward " \t")
                                                        (point)))
                         )))
   ((s-matches? (rx "You should enclose the previous parenthesis with `{}'.")
                msg)
    (attrap-one-option 'enclose-with-braces
                       (forward-char)
                       (insert "}")
                       (save-excursion
                         (backward-char)
                         (backward-sexp)
                         (re-search-backward "[^[:alnum:]\\_\\/]")
                         (forward-char)
                         (insert "{")
                         )))
   ((s-matches? (rx "You should not use punctuation in front of quotes.")
                msg)
    (attrap-one-option 'swap-punctuation-with-quotes
                       (progn
                         (forward-char)
                         (delete-char 2)
                         (backward-char)
                         (insert "''"))))))

(use-package emacs
  :ensure nil
  :after flycheck attrap
  :config
  (add-to-list 'attrap-flycheck-checkers-alist '(tex-chktex . czm-attrap-LaTeX-fixer)))

(use-package latex-flymake
  :ensure nil
  :after latex)

(with-eval-after-load 'attrap
  (setcdr (assoc 'LaTeX-flymake attrap-flymake-backends-alist)
          #'czm-attrap-LaTeX-fixer-flymake))

(use-package dynexp
  :ensure (:host github :repo "ultronozm/dynexp.el"
                 :files ("lisp/dynexp-abbrev.el")
                 :depth nil)
  :after latex
  :hook (LaTeX-mode . dynexp-latex-setup)
  :bind (:map LaTeX-mode-map
              ("SPC" . dynexp-space)
              ("TAB" . dynexp-next))
  :config
  (quietly-read-abbrev-file
   (expand-file-name "dynexp-abbrev.el"
                     (file-name-directory (locate-library "dynexp")))))

(use-package czm-tex-edit
  :ensure (:host github :repo "ultronozm/czm-tex-edit.el"
                 :depth nil)
  :after latex dynexp
  ;; :demand ; should come after latex and dynexp
  :bind
  (:map LaTeX-mode-map
        ("C-c t i" . czm-tex-edit-emphasize)
        ("C-c t a" . czm-tex-edit-alertify)
        ("C-c t b" . czm-tex-edit-bold)
        ("C-c t l" . czm-tex-edit-underline)
        ("C-c t u" . czm-tex-edit-unemphasize)
        ("C-c t e" . czm-tex-edit-external-document-link)
        ("C-c p e" . czm-tex-edit-repeat-most-recent-equation)
        ("C-c p d" . czm-tex-edit-repeat-line-contents)
        ("C-c p r" . czm-tex-edit-repeat-region)
        ("C-c p s" . czm-tex-edit-substackify)
        ("C-c p i" . czm-tex-edit-yank-interior-delete-delim)
        ("C-c p f" . czm-tex-edit-fractionify-region)
        ("C-c p b" . czm-tex-edit-enlarge-parentheses)
        ("C-c p h" . czm-tex-edit-split-equation)
        ("C-c e" . czm-tex-edit-make-equation-numbered)
        ("C-c i" . czm-tex-edit-make-equation-inline)
        ("C-c w" . czm-tex-edit-make-equation-align)
        ("C-c q" . czm-tex-edit-make-equation-multline)
        ("s-<return>" . czm-tex-edit-return)
        ;; ("$" . czm-tex-edit-insert-dollar-or-wrap-region) ; not necessary w/ electric-pair-mode?
        ("\"" . czm-tex-edit-insert-quote-or-wrap-region))
  :config
  (czm-tex-edit-define-color-functions-and-bindings
   "C-c t c"
   (("red" . "r") ("green" . "g") ("blue" . "b") ("yellow" . "y") ("orange" . "o") ("purple" . "p") ("black" . "k") ("white" . "w") ("cyan" . "c") ("magenta" . "m") ("lime" . "l") ("teal" . "t") ("violet" . "v") ("pink" . "i") ("brown" . "n") ("gray" . "a") ("darkgreen" . "d") ("lightblue" . "h") ("lavender" . "e") ("maroon" . "u") ("beige" . "j") ("indigo" . "x") ("turquoise" . "q") ("gold" . "f") ("silver" . "s") ("bronze" . "z"))))

(use-package auctex-cont-latexmk
  :ensure (:host github :repo "ultronozm/auctex-cont-latexmk.el"
                 :depth nil)
  :after latex
  :bind (:map LaTeX-mode-map ("C-c k" . auctex-cont-latexmk-toggle))
  :custom
  (auctex-cont-latexmk-command
   '("latexmk -pvc -shell-escape -pdf -view=none -e "
     ("$pdflatex=q/pdflatex %O -synctex=1 -interaction=nonstopmode %S/"))))

(defun my/set-TeX-master ()
  (setq-local TeX-master "~/doit/preview-master.tex"))

(add-hook 'prog-mode-hook #'my/set-TeX-master)

(use-package preview-auto
  :ensure (:host github :repo "ultronozm/preview-auto.el"
                 :depth nil)
  :after latex
  :hook (LaTeX-mode . preview-auto-setup)
  :config
  (setq preview-protect-point t)
  (setq preview-locating-previews-message nil)
  (setq preview-leave-open-previews-visible t)
  :custom
  (preview-auto-interval 0.1)
  (preview-LaTeX-command-replacements
   '(preview-LaTeX-disable-pdfoutput)))

(use-package auctex-label-numbers
  :ensure (:host github :repo "ultronozm/auctex-label-numbers.el"
                 :depth nil)
  :after latex)

(use-package library
  :after latex czm-tex-util
  :defer t
  :ensure (:host github :repo "ultronozm/library.el"
                 :depth nil)
  :custom
  (library-pdf-directory my-pdf-folder)
  (library-bibtex-file my-master-bib-file)
  (library-download-directory my-downloads-folder)
  (library-org-capture-template-key "j"))

(defun czm-tex-jump-back-with-breadcrumb ()
  (interactive)
  (save-excursion (insert "<++>"))
  (call-interactively #'tex-parens-backward-down-list))

(use-package tex-parens
  :ensure (:host github :repo "ultronozm/tex-parens.el"
                 :depth nil)
  :after latex
  :bind (:map LaTeX-mode-map
              ("M-i" . tex-parens-mark-inner)
              ("s-j" . tex-parens-avy-jump-to-math)
              ("C-M-j" . czm-tex-jump-back-with-breadcrumb)
              ("s-c" . tex-parens-avy-copy-math)
              ("s-e" . tex-parens-end-of-list)
              ("s-a" . tex-parens-beginning-of-list)
              ("s-E" . tex-parens-kill-to-end-of-list)
              ("s-A" . tex-parens-kill-to-beginning-of-list))
  :hook
  (LaTeX-mode . tex-parens-mode)
  :config
  (defun czm-expand-abbrev-advice (orig-fun &rest args)
    (unless (nth 4 (syntax-ppss))
      (apply orig-fun args)))
  (advice-add 'expand-abbrev :around #'czm-expand-abbrev-advice)
  (add-to-list 'preview-auto-reveal-commands #'czm-tex-jump-back-with-breadcrumb)
  (define-repeat-map tex-parens-structural-edit
    ("n" tex-parens-forward-list
     "p" tex-parens-backward-list
     "u" tex-parens-backward-up-list
     "M-u" tex-parens-up-list
     "g" tex-parens-down-list
     "M-g" tex-parens-backward-down-list)
    (:continue
     "f" tex-parens-forward-sexp
     "b" tex-parens-backward-sexp
     "a" beginning-of-defun
     "e" end-of-defun
     "k" kill-sexp
     ">" tex-parens-burp-right
     "<" tex-parens-burp-left
     "C-/" undo
     "r" tex-parens-raise-sexp
     "/" tex-parens-delete-pair
     "t" transpose-sexps
     "w" kill-region
     "M-w" kill-ring-save
     "y" yank
     "c" lispy-clone
     "RET" TeX-newline))
  (repeat-mode 1))

(use-package tex-item
  :ensure (:host github :repo "ultronozm/tex-item.el"
                 :depth nil)
  :after latex
  :config
  (defvar-keymap tex-item-map
    :repeat t
    "n" #'tex-item-forward
    "p" #'tex-item-backward
    "SPC" #'tex-item-mark
    "k" #'tex-item-kill
    "<backspace>" #'tex-item-backward-kill
    "t" #'tex-item-transpose
    "<down>" #'tex-item-move-down
    "<up>" #'tex-item-move-up)
  (define-key LaTeX-mode-map (kbd "M-g M-i") tex-item-map))

(defalias 'czm-setup-tex-file
  (kmacro "l t x SPC s-s s-p z C-n C-n C-c C-p C-a C-c C-p C-f"))

;;; --- Sage ---

(use-package sage-shell-mode
  :defer t
  :custom
  (sage-shell:use-prompt-toolkit nil)
  (sage-shell:use-simple-prompt t)
  (sage-shell:set-ipython-version-on-startup nil)
  (sage-shell:sage-executable my-sage-exe)
  (sage-shell:check-ipython-version-on-startup nil)
  :bind
  (:map sage-shell-mode-map ("C-c n" . czm-sage-worksheet))
  (:map sage-shell:sage-mode-map ("C-c n" . czm-sage-worksheet))
  :hook
  ((sage-shell-mode sage-shell:sage-mode) . eldoc-mode)
  (sage-shell-after-prompt . sage-shell-view-mode))

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
  (mmm-global-mode nil)
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
  (czm-tex-mint-initialize)
  :bind
  (:map czm-tex-mint-mode-map
        ("C-c C-c" . czm-tex-mint-evaluate)
        ("C-c C-l" . czm-tex-mint-evaluate-latex))
  :hook
  (mmm-sage-shell:sage-mode-enter . czm-tex-mint-enable)
  (mmm-sage-shell:sage-mode-exit . czm-tex-mint-disable))

(use-package symtex
  :ensure (:host github
                 :repo "ultronozm/symtex.el"
                 :depth nil)
  :after latex
  :bind
  (:map global-map
        ("C-c V" . symtex-process))
  (:map LaTeX-mode-map
        ("C-c v" . symtex-dwim)))

;;; --- Scratch files ---

(defun czm-create-scratch-file (dir extension &optional setup-fn)
  "Create a new temporary file in DIR with EXTENSION.
Optionally run SETUP-FN after creating the file."
  (let* ((dir (file-name-as-directory dir))
         (filename (format-time-string (concat "%Y%m%dT%H%M%S--scratch." extension)))
         (filepath (expand-file-name filename dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (find-file filepath)
    (save-buffer)
    (when setup-fn (funcall setup-fn))))

(defun czm-create-scratch-org ()
  "Create new scratch org buffer."
  (interactive)
  (czm-create-scratch-file my-scratch-org-dir "org"))

(defun czm-create-scratch-tex ()
  "Create new scratch LaTeX buffer."
  (interactive)
  (czm-create-scratch-file my-scratch-tex-dir "tex" #'czm-setup-tex-file))

(defun czm-create-scratch-sage ()
  "Create new scratch sage file."
  (interactive)
  (czm-create-scratch-file my-scratch-sage-dir "sage"))

;;; --- Lean ---

(defun czm-set-lean4-local-variables ()
  (setq preview-tailor-local-multiplier 0.7)
  (setq TeX-master "~/doit/preview-master.tex"))

(use-package lean4-mode
  :ensure (:host github :repo "ultronozm/lean4-mode"
                 :files ("*.el" "data"))
  :diminish
  :hook
  (lean4-mode . czm-lean4-set-imenu-generic-expression)
  (lean4-mode . czm-set-lean4-local-variables)
  :commands (lean4-mode)
  :custom
  (lean4-idle-delay 0.02)
  (lean4-info-plain nil)
  (lean4-info-refresh-even-if-invisible t)
  :bind (:map lean4-mode-map
              ("RET" . newline)
              ("C-j" . newline-and-indent)
              ("C-M-i" . completion-at-point)
              ("C-c C-k" . quail-show-key))
  :config
  :defer t)

(use-package czm-lean4
  :ensure (:host github :repo "ultronozm/czm-lean4.el"
                 :depth nil)
  :after lean4-mode preview-auto
  :hook (lean4-mode . czm-lean4-mode-hook)
  :hook (magit-section-mode . czm-lean4-magit-section-mode-hook)
  :bind (:map lean4-mode-map
              ("C-c v" . czm-lean4-show-variables)
              ;; ("C-c C-p C-p" . czm-lean4-toggle-info-pause)
              ("C-c C-m C-m" . czm-lean4-search-mathlib)
              ("C-c C-m C-h" . czm-lean4-search-mathlib-headings)
              ("C-c C-m C-d" . flymake-overlays-mode)
              ("C-c C-m C-t" . flymake-overlays-smart-toggle)
              ("C-c C-m C-g" . czm-lean4-toggle-goal-overlay)
              ("C-c C-m C-l" . czm-lean4-live-goal-mode)
              ("C-c C-," . czm-lean4-insert-section-or-namespace)
              ("C-c C-." . czm-lean4-insert-comment-block)
              ("C-c C-i" . czm-lean4-toggle-info-split-below)
              ("C-c C-y" . czm-lean4-toggle-info-split-right)
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
  :bind (:map lean4-mode-map
              ("s-f" . czm-lean4-preview-fold-block))
  :custom
  (czm-lean4-info-window-height-fraction 0.4)
  (czm-lean4-info-window-width-fraction 0.47)
  :config
  (advice-add 'lean4-info-buffer-redisplay :around #'czm-lean4-info-buffer-redisplay))

(defun czm-lean4-maybe-colorize (text)
  "Highlight theorem signatures in the given TEXT for Lean4 buffers."
  (let ((mode major-mode))
    (with-temp-buffer
      (delay-mode-hooks (funcall mode))
      (insert text)
      (font-lock-ensure)
      (when (eq mode 'lean4-mode)
        (czm-lean4-colorize-theorem-signature (point-min) (point-max)))
      (buffer-string))))

(use-package flymake-overlays
  :ensure (:host github :repo "ultronozm/flymake-overlays.el"
                 :depth nil)
  :after flymake
  :bind (:map flymake-mode-map
              ;; ("C-c t" . flymake-overlays-smart-toggle)
              )
  :hook (flymake-mode . flymake-overlays-mode)
  :custom
  (flymake-overlays-fontify-text-function #'czm-lean4-maybe-colorize))

(defun czm-add-lean4-eldoc ()
  (when (with-current-buffer eldoc-icebox-parent-buffer
          (or (eq major-mode 'lean4-mode)
              (equal (buffer-name)
                     "*Lean Goal*")))
    (add-hook 'eldoc-documentation-functions #'lean4-info-eldoc-function
              nil t)
    (eldoc-mode)))

(defun czm-eldoc-icebox-text-processor ()
  "Fontify buffer, possibly with Lean4 theorem signatures.
This function is intended to be used with flymake overlays."
  (let ((text (buffer-string))
        (mode major-mode))
    (let ((newtext
           (with-temp-buffer
             (delay-mode-hooks (funcall mode))
             (insert text)
             (font-lock-ensure)
             (when (eq mode 'lean4-mode)
               (czm-lean4-colorize-theorem-signature (point-min) (point-max)))
             (buffer-string))))
      (delete-region (point-min) (point-max))
      (insert newtext))))

(use-package eldoc-icebox
  :ensure (:host github :repo "ultronozm/eldoc-icebox.el"
                 :depth nil)
  :bind (("C-c C-h" . eldoc-icebox-store)
         ("C-c C-n" . eldoc-icebox-toggle-display))
  :hook
  (eldoc-icebox-post-display . shrink-window-if-larger-than-buffer)
  (eldoc-icebox-post-display . czm-eldoc-icebox-text-processor)
  (eldoc-icebox-post-display . czm-add-lean4-eldoc))

;; https://www.jamescherti.com/emacs-customize-ellipsis-outline-minor-mode/
(defun my-outline-set-global-ellipsis (ellipsis)
  "Apply the ellipsis ELLIPSIS to outline mode globally."
  (let* ((face-offset (* (face-id 'shadow) (ash 1 22)))
         (value (vconcat (mapcar (lambda (c) (+ face-offset c)) ellipsis))))
    (set-display-table-slot standard-display-table 'selective-display value)))

(my-outline-set-global-ellipsis " ▼ ")
