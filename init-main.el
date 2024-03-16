;;; -*- lexical-binding: t; -*-

;;; ------------------------------ ELPACA ------------------------------

(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
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
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
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
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

(use-package exec-path-from-shell
  :demand
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(elpaca-wait)

;; (use-package org
;;   :elpaca `(org
;;           :fork (:host nil
;;                  :repo "https://git.tecosaur.net/tec/org-mode.git"
;;                  :branch "dev"
;;                  :remote "tecosaur")
;;           :files (:defaults "etc")
;;           :build t
;;           :pre-build
;;           (with-temp-file "org-version.el"
;;             (require 'lisp-mnt)
;;             (let ((version
;;                   (with-temp-buffer
;;                     (insert-file-contents "lisp/org.el")
;;                     (lm-header "version")))
;;                   (git-version
;;                   (string-trim
;;                    (with-temp-buffer
;;                      (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
;;                      (buffer-string)))))
;;               (insert
;;                (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
;;                (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
;;                "(provide 'org-version)\n")))
;;           :pin nil))

(elpaca-wait)

;; This was needed for a bit, but suddenly seems to break things.  Hmm.

;; (defun +elpaca-unload-seq (e) "Unload seq before continuing the elpaca build, then continue to build the recipe E."
;;        (and (featurep 'seq) (unload-feature 'seq t))
;;        (elpaca--continue-build e))
;; (elpaca `(seq :build ,(append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
;;                                            elpaca--pre-built-steps
;;                                          elpaca-build-steps))
;;                               (list '+elpaca-unload-seq 'elpaca--activate-package))))

(elpaca-wait)

;;; ------------------------------ GENERAL ------------------------------

(setq custom-file (concat user-emacs-directory "init-custom.el"))
;; (load custom-file)

(when (file-exists-p (concat user-emacs-directory "init-personal.el"))
  (load (concat user-emacs-directory "init-personal.el")))

(use-package avy
  :bind
  (:map global-map
        ("C-;" . avy-goto-line)
        ("C-M-; y" . avy-copy-region)
        ("C-M-; n" . avy-kill-ring-save-region)
        ("C-M-; t" . avy-move-region)
        ("C-M-; x" . avy-kill-region))
  (:map isearch-mode-map
        ("M-j" . avy-isearch)))

(use-package czm-misc
  :elpaca (:host github :repo "ultronozm/czm-misc.el"
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
         ("s-t" . czm-misc-transpose-abc-to-cba)
         ("M-o" . czm-misc-split-line-below)
         ("C-S-SPC" . czm-misc-delete-horizontal-space-on-line)
         ("M-c" . czm-misc-avy-copy-sexp)
         ("s-c" . czm-misc-avy-copy-sexp-t)
         ("M-l" . avy-copy-line)
         ("C-x j" . czm-misc-dired-popup))
  (:map minibuffer-local-map
        ("C-c d" . czm-misc-insert-date)))

(use-package emacs
  :elpaca nil

  :custom
  (safe-local-variable-values
   '((czm-preview-TeX-master . "~/doit/preview-master-principal_cg.tex")
     (cmake-build-options . "-j 6")
     (eval add-to-list 'LaTeX-indent-begin-exceptions-list "ifs")))
  (use-dialog-box nil)
  (show-paren-delay 0)
  (show-paren-style 'parenthesis)
  (ring-bell-function #'ignore)
  (initial-scratch-message nil)
  (inhibit-startup-message t)
  (mark-even-if-inactive nil)
  (tramp-default-method "ssh")
  (tramp-ssh-extra-args (list "-i" "~/.ssh/"))
  (password-cache-expiry nil)
  (compile-command "make")
  (enable-recursive-minibuffers t)
  (max-lisp-eval-depth 12000)
  (bookmark-save-flag 1)
  (dired-create-destination-dirs 'ask)
  (dired-isearch-filenames t)
  (dired-vc-rename-file t)
  (large-file-warning-threshold 20000000)
  (vc-follow-symlinks t)
  (view-read-only t)
  (delete-by-moving-to-trash t)
  (help-window-select t)
  (isearch-allow-scroll t)
  (doc-view-resolution 300)
  (backup-directory-alist
   `(("." . ,(expand-file-name
              (concat user-emacs-directory "backups")))))
  (auto-save-file-name-transforms
   `((".*" ,(expand-file-name
             (concat user-emacs-directory "auto-save/")) t)))
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package emacs
  :elpaca nil

  :config
  (electric-pair-mode)
  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (fset 'yes-or-no-p 'y-or-n-p)
  (minibuffer-depth-indicate-mode)
  (global-auto-revert-mode)
  (setq-default indent-tabs-mode nil)
  ;; (warning-suppress-log-types '((comp))) ; don't care about this anymore?
  (save-place-mode)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (transient-mark-mode 0)
  (line-number-mode)
  (column-number-mode)
  (winner-mode))

(use-package emacs
  :elpaca nil

  :custom
  (display-time-default-load-average nil)

  :config
  (display-time-mode))

(use-package recentf
  :elpaca nil

  :custom
  (recentf-max-saved-items 100)
  :config
  (recentf-mode))



(use-package prog-mode
  :elpaca nil
  :hook
  (prog-mode . outline-minor-mode)
  ;; (prog-mode . hs-minor-mode)
  )

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode)
  (LaTeX-mode . aggressive-indent-mode))


(use-package emacs
  :elpaca nil
  :after outline
  :bind (:map outline-minor-mode-map
              ("C-M-<down-mouse-1>" . nil)
              ("C-M-<down-mouse-2>" . nil)
              ("C-M-<down-mouse-3>" . nil)
              ("<right-margin> S-<mouse-1>" . nil)
              ("<right-margin> <mouse-1>" . nil)
              ("<left-margin> S-<mouse-1>" . nil)
              ("<left-margin> <mouse-1>" . nil)))

;;; ------------------------------ LISP ------------------------------

(use-package emacs
  :elpaca nil

  :custom
  (delete-pair-blink-delay 0)
  :bind
  (:map emacs-lisp-mode-map
        ("M-_" . delete-pair)
        ("M-+" . kill-backward-up-list)))

(use-package lispy
  :config
  (setcdr lispy-mode-map nil)
  (let ((map lispy-mode-map))
    (lispy-define-key map ">" 'lispy-slurp-or-barf-right)
    (lispy-define-key map "<" 'lispy-slurp-or-barf-left)
    (lispy-define-key map "/" 'lispy-splice)
    (lispy-define-key map "+" 'lispy-join)
    (lispy-define-key map "c" 'lispy-clone)
    (lispy-define-key map ";" 'lispy-comment)
    (lispy-define-key map "o" 'lispy-oneline)
    (lispy-define-key map "m" 'lispy-multiline)
    (lispy-define-key map "SPC" 'lispy-space)
    (lispy-define-key map "i" 'lispy-tab)
    (define-key map (kbd "C-M-j") 'lispy-split)
    ;; (define-key map (kbd "M-+") 'lispy-raise)
    (define-key map (kbd "M-+") nil)
    (define-key map (kbd "\"") 'lispy-quotes)
    (define-key map (kbd "M-1") 'lispy-describe-inline)
    (define-key map (kbd "M-2") 'lispy-arglist-inline)

    ;; (lispy-define-key map "w" 'lispy-move-up)
    ;; (lispy-define-key map "s" 'lispy-move-down)
    ;; (lispy-define-key map "r" 'lispy-raise)
    ;; (lispy-define-key map "A" 'lispy-beginning-of-defun)
    ;; (lispy-define-key map "C" 'lispy-convolute)
    ;; (lispy-define-key map "X" 'lispy-convolute-left)
    ;; (lispy-define-key map "q" 'lispy-ace-paren)
    ;; (lispy-define-key map "-" 'lispy-ace-subword)
    ;; (lispy-define-key map "e" 'lispy-eval)
    map)
  (defun czm-conditionally-enable-lispy ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1)))
  :bind
  ;; (("C-M-K" . lispy-kill))
  (("C-M-k" . kill-sexp))
  :hook
  (emacs-lisp-mode  . lispy-mode)
  (minibuffer-setup . czm-conditionally-enable-lispy))

(defun czm-edebug-eval-hook ()
  (lispy-mode 0)
  (copilot-mode 0))
(add-hook 'edebug-eval-mode-hook #'czm-edebug-eval-hook)

;;; ------------------------------ GIT ------------------------------


(use-package magit
  :defer t)

(defun czm/git-update-commit-push-this-file ()
  "Update, commit, and push the current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (cond
     ((not file)
      (message "Abort: Buffer is not visiting a file."))
     ((not (file-exists-p file))
      (message "Abort: File does not exist."))
     ((magit-anything-staged-p)
      (message "Abort: There are staged changes in the repository."))
     (t
      (progn
        ;; stage, commit, and push
        (let* ((default-msg (concat "Update " (file-name-nondirectory file)))
               (msg (read-string "Commit message: " default-msg)))
          (magit-stage-file file)
          (magit-commit-create (list "-m" msg))
          ;; call the following interactively: (magit-push-current-to-upstream nil)
          (call-interactively 'magit-push-current-to-upstream)))))))


;;; ------------------------------ ESSENTIAL PACKAGES ------------------------------

;; (use-package eldoc
;;   :elpaca nil
;;   :custom
;;   ;  (eldoc-echo-area-use-multiline-p truncate-sym-name-if-fiteldoc-echo-area-use-multiline-p)
;;   (eldoc-echo-area-use-multiline-p t)
;;   (eldoc-idle-delay 0.25))

(use-package ef-themes
  :demand
  :config
  ;; (load-theme 'modus-vivendi t)
  ;; (load-theme 'modus-operandi t)
  ;; (load-theme 'ef-frost t)
  ;; (load-theme 'ef-elea-dark t)

  (let ((hour (string-to-number (substring (current-time-string) 11 13))))
    (cond
     ((<= hour 7)
      (load-theme 'modus-vivendi t))
     ((<= hour 19)
      (load-theme 'ef-frost t))
     (t
      (load-theme 'ef-autumn t)))))

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
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("s-b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.

(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))



(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))
               ))

(use-package embark-consult
  ;; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; (use-package savehist
;;   :ensure t
;;   (completion-category-overrides '((file (styles basic partial-completion))))
;;   :init
;;   (savehist-mode))

(use-package company
  ;; :config (global-company-mode 1) ;; doesn't work in magit, for instance
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay 0.5) ;; how long to wait until popup.  Consider changing to 0.0?
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  ;; (company-begin-commands nil) ;; uncomment to disable popup

  ;; not using it at the moment because it doesn't seem to work well
  ;; :config (global-company-mode)
  :bind
  (:map company-active-map
        ("C-n". company-select-next)
        ("C-p". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last))
  ;; (:map company-mode-map
  ;;       ("<tab>". tab-indent-or-complete)
  ;;       ("TAB". tab-indent-or-complete))
  )

(use-package ace-window
  :bind
  ("C-x o" . ace-window))

(use-package which-key
  :config
  (which-key-mode))

(use-package ace-link ; activate using 'o' in info/help/(...)
  :config
  (ace-link-setup-default))

(use-package zzz-to-char
  :bind
  ("s-t" . zzz-to-char-up-to-char)
  ("s-f" . zzz-to-char)
  :custom
  (zzz-to-char-reach 200))

;;; ------------------------------ AI ------------------------------

(use-package copilot
  :elpaca (:host github
                 :repo "zerolfx/copilot.el"
                 ;; :repo "ultronozm/copilot.el"
                 :files ("*.el" "dist")
                 :depth nil)
  :hook
  ((prog-mode LaTeX-mode git-commit-mode) . copilot-mode)
  (emacs-lisp-mode . (lambda () (setq tab-width 1)))
  (lean4-mode . (lambda () (setq tab-width 2)))

  :custom
  (copilot-indent-offset-warning-disable t)
  :bind (:map copilot-completion-map
              ("§" . copilot-accept-completion)
              ("M-§" . copilot-accept-completion-by-word)
              ("C-§" . copilot-accept-completion-by-line)
              ("C-M-§" . copilot-accept-completion-by-paragraph)
              ("`" . copilot-accept-completion)
              ("M-`" . copilot-accept-completion-by-word)
              ("C-`" . copilot-accept-completion-by-line)
              ("C-M-`" . copilot-accept-completion-by-paragraph)
              ("C-M-<down>" . copilot-next-completion)
              ("C-M-<up>" . copilot-previous-completion)))

(use-package gptel
  :after exec-path-from-shell
  :defer t
  :custom
  (gptel-model "gpt-4")
  :config
  (setq gptel-api-key (exec-path-from-shell-getenv "OPENAI_API_KEY"))
  (gptel-make-anthropic "Claude"
    :stream t
    :key (exec-path-from-shell-getenv "CLAUDE_API_KEY")))


(defun czm-gptel-claude-sonnet ()
  (interactive)
  (setq-default
   gptel-model "claude-3-sonnet-20240229"
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t :key (exec-path-from-shell-getenv "CLAUDE_API_KEY"))))

(defun czm-gptel-claude-opus ()
  (interactive)
  (setq-default
   gptel-model "claude-3-opus-20240229"
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t :key (exec-path-from-shell-getenv "CLAUDE_API_KEY"))))

(defun czm-gptel-gpt4 ()
  (interactive)
  (setq-default
   gptel-model "gpt-4"
   gptel-backend gptel--openai
   ))


(use-package ai-org-chat
  :elpaca (:host github :repo "ultronozm/ai-org-chat.el"
                 :depth nil)
  :bind
  (:map global-map
        ("s-/" . ai-org-chat-new))
  (:map ai-org-chat-minor-mode
        ("s-<return>" . ai-org-chat-respond)
        ("C-c n" . ai-org-chat-branch))
  :commands (ai-org-chat-minor-mode) ; for manual activation
  :custom
  (ai-org-chat-user-name "Paul")
  (ai-org-chat-dir "~/gpt")
  (ai-org-chat-system-message nil))
;; (ai-org-chat-prompt-preamble
;;    "You are a brilliant and helpful assistant.

;; You know everything about programming: languages, syntax, debugging techniques, software design, code optimization, documentation.

;; Respond with Emacs org-mode syntax.  For example, when providing code examples, do NOT use triple backticks and markdown, but rather source blocks, e.g.:
;; #+begin_src elisp
;;   (number-sequence 0 9)
;; #+end_src

;; Avoid attempting to give the answer right away.  Instead, begin by breaking any problem down into steps.

;; Don't attempt nontrivial calculations directly.  In particular, you are unable to directly answer any question that requires analyzing text as a sequence of characters (e.g., counting length, reversing strings), counting of more than several items (e.g., words in a sequence or items in a list), or arithmetic that a human could not perform easily in their head.  In such cases, return a source block containing relevant elisp or Python code.  For example, if the question is \\"What is 7 + 19^3\\", you could return either of the following:

;; #+begin_src elisp
;;   (+ 7 (expt 19 3))
;; #+end_src

;; #+begin_src python
;;   return 7 + 19 ** 3
;; #+end_src

;; When faced with a complicated word problem, reduce it first to a problem in algebra, then solve it using elisp or Python.  For example, if the problem reduces to computing the binomial coefficient \\"20 choose 13\\", then you could return:

;; #+begin_src python
;;   from math import factorial
;;   return factorial(20) / ( factorial(13) * factorial(20-13) )
;; #+end_src

;; Or if you need to solve an equation, use something like sympy:

;; #+begin_src python
;;   from sympy import symbols, solve
;;   x = symbols('x')
;;   solution = solve([2*x + x + x + 2 - 26, x>0], x)
;;   return solution
;; #+end_src

;; Never describe the results of running code.  Instead, wait for me to run the code and then ask you to continue.")


;;; ------------------------------ REPEAT ------------------------------

(use-package repeat
  :elpaca nil
  :config
  (setcdr other-window-repeat-map nil)
  (repeat-mode))

;; https://karthinks.com/software/it-bears-repeating/
(defun repeatize (keymap)
  "Add `repeat-mode' support to a KEYMAP."
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map keymap)))
   (symbol-value keymap)))

;;; ------------------------------ FLYCHECK ------------------------------

(use-package flycheck
  :defer t
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))


(use-package flycheck-package
  :defer t
  :hook
  (emacs-lisp-mode . flycheck-package-setup))

;;; ------------------------------ ATTRAP ------------------------------

(use-package attrap
  :after flycheck
  :config
  (setq saved-match-data nil))

(use-package emacs
  :elpaca nil
  :after flycheck attrap repeat
  :config
  (define-key flycheck-command-map "f" 'attrap-flycheck)
  (repeatize 'flycheck-command-map))



;;; ------------------------------ PROJECT ------------------------------

; this redefines something in project.el.  Why?
(cl-defmethod project-root ((project (head local)))
  "TODO."
  (cdr project))

(defun czm/project-try-local (dir)
  "Determine if DIR is a non-Git project.
DIR must include a .project file to be considered a project."
  (let ((root (locate-dominating-file dir ".project")))
    (and root (cons 'local root))))

(use-package project
  :elpaca nil

  :config
  (add-to-list 'project-find-functions 'czm/project-try-local))

;;; ------------------------------ MARK ------------------------------

;; copied from https://spwhitton.name/blog/entry/transient-mark-mode/
(defun spw/remap-mark-command (command &optional map)
  "Remap a mark-* command to temporarily activate Transient Mark mode."
  (let* ((cmd (symbol-name command))
         (fun (intern (concat "spw/" cmd)))
         (doc (concat "Call `"
                      cmd
                      "' and temporarily activate Transient Mark mode.")))
    (fset fun `(lambda ()
                 ,doc
                 (interactive)
                 (call-interactively #',command)
                 (activate-mark)))
    (if map
        (define-key map (vector 'remap command) fun)
      (global-set-key (vector 'remap command) fun))))

(dolist (command '(mark-word
                   mark-sexp
                   mark-paragraph
                   mark-defun
                   mark-page
                   mark-whole-buffer
                   rectangle-mark-mode))
  (spw/remap-mark-command command))



;;; ------------------------------ LATEX ------------------------------

(unless (eq window-system 'w32)
  (defun czm-tex-buffer-face ()
    (interactive)
    (setq buffer-face-mode-face
          '(:height 260
                    :width normal
                    :family
                    ;; "Monaco"
                    "Andale Mono"
                    ;; "Lucida Grande"
                    ;; "Papyrus"
                    ;; "Hoefler Text"
                    )
          ;; '(:height 260
          ;;           :width normal
          ;;           :family
          ;;           "Lucida Grande"
          ;;           )
          ;; '(:height 260 :width normal
          ;;                                 :family
          ;;                                 ;; "Didot"
          ;;                                 "Andale Mono"
          ;;                                 ;; "Lucida Grande"
          ;;                                 ;; "Papyrus"
          ;;                                 ;; "PT Mono"
          ;;                                 ;; "Baskerville"
          ;;                                 ;; "Hoefler Text"
          ;;                                 ;; "Monaco"
          ;;                                 ;; "Verdana"
          ;;                                 ;; "Menlo"
          ;;                                 ;; "DejaVu Mono-12"
          ;;                                 ;; "Lucida Typewriter"
          ;;                                 ;; "Courier New"
          ;;                                 )
          )
    ;; (unless buffer-face-mode
    ;;   (buffer-face-mode 0))
    (buffer-face-mode))

  (defun czm-tex-setup-environments-and-outline-regexp ()
    (LaTeX-add-environments
     '("lemma" LaTeX-env-label)
     '("exercise" LaTeX-env-label)
     '("example" LaTeX-env-label)
     '("proposition" LaTeX-env-label)
     '("corollary" LaTeX-env-label)
     '("remark" LaTeX-env-label)
     '("definition" LaTeX-env-label)
     '("theorem" LaTeX-env-label))
    (setq-local outline-regexp
	               (concat "\\\\"
		                      (regexp-opt (append latex-metasection-list
					                                       (mapcar #'car latex-section-alist)
					                                       '("bibliography"))
				                                t))))

  (defun czm-widen-first (orig-fun &rest args)
    (save-restriction
      (widen)
      (apply orig-fun args)))

  ;; https://karthinks.com/software/latex-input-for-impatient-scholars/
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-language latex
                                          calc-prefer-frac t
                                          calc-angle-mode rad)))))
          (t (let ((l (thing-at-point 'line)))
               (end-of-line 1) (kill-line 0)
               (insert (calc-eval `(,l
                                    calc-language latex
                                    calc-prefer-frac t
                                    calc-angle-mode rad)))))))

  (defun czm-latex-calc-grab (beg end)
    (interactive "r")
    ;; (let ((old-lang calc-language))
    ;;   (unwind-protect
    ;;       (progn
    ;;         (save-excursion
    ;;           (calc-create-buffer))
    ;;         (calc-set-language 'latex)
    ;;         (calc-grab-region beg end '(4)))
    ;;     (when old-lang
    ;;       (calc-set-language old-lang))))
    ;; (calc-grab-region beg end '(4))
    (symtex-with-calc-language 'latex
                               (calc-grab-region beg end '(4))))

  (defun czm-TeX-next-error-wrapper (&optional arg)
    (interactive "P")
    (if
        (or (null (TeX-active-buffer))
            (eq 'compilation-mode (with-current-buffer TeX-command-buffer
                                    major-mode)))
        (TeX-next-error arg reparse)
      (next-error arg)))

  (defun czm-TeX-previous-error-wrapper (&optional arg)
    (interactive "P")
    (if
        (or (null (TeX-active-buffer))
            (eq 'compilation-mode (with-current-buffer TeX-command-buffer
                                    major-mode)))
        (TeX-previous-error arg reparse)
      (previous-error arg)))

  (defun czm-unwrap-mark-sexp ()
    (interactive)
    (let ((results (sp-unwrap-sexp)))
      ;; this returns something like (:beg 12501 :end 12618 :op "\\left[" :cl "\\right]" :prefix "" :suffix ".").  let's bind those fields to variables using plist-get:
      (let ((end (plist-get results :end))
            (op (plist-get results :op))
            (cl (plist-get results :cl)))
        (let ((new-end
               (- end
                  (+ (length op) (length cl)))))
          ;; highlight region between beg and end
          (push-mark end)
          (activate-mark)))))

  (use-package latex
    :elpaca (auctex
             :files
             ("*.el" "*.info" "dir" "doc" "etc" "images" "latex" "style")
             :pre-build
             (("./autogen.sh")
              ("./configure"
               "--with-texmf-dir=$(dirname $(kpsexpand '$TEXMFHOME'))"
               "--with-lispdir=.")
              ("make")
              ("make" "install")))

    :demand                               ; otherwise, madness ensues.

    :config
    (setq TeX-data-directory (expand-file-name "elpaca/builds/auctex" user-emacs-directory))
    (setq TeX-lisp-directory TeX-data-directory)

    :hook
    (LaTeX-mode . TeX-fold-mode)
    (LaTeX-mode . turn-on-reftex)
    (LaTeX-mode . czm-tex-setup-environments-and-outline-regexp)
    (LaTeX-mode . czm-tex-buffer-face)
    (LaTeX-mode . outline-minor-mode)
    (LaTeX-mode . abbrev-mode)
    ;; (LaTeX-mode . toggle-word-wrap)
    (LaTeX-mode . visual-line-mode)
    (LaTeX-mode . (lambda ()
                    (setq fill-column 999999)))
    (LaTeX-mode . smartparens-mode)

    :bind
    (:map LaTeX-mode-map
          ("s-a" . abbrev-mode)
          ("s-c" . preview-clearout-at-point)
          ("s-q" . LaTeX-fill-buffer)

          ("C-c C-l" . latex-math-from-calc)
          ("C-c C-g" . czm-latex-calc-grab)
          ("C-c C-n" . nil)
                                        ; TeX-normal-mode
          ("C-c #" . nil)
          ;; ("M-n" . czm-TeX-next-error-wrapper)
          ;; ("M-p" . czm-TeX-previous-error-wrapper)
          ([remap next-error])
          ([remap previous-error])
          ("M-n" . next-error)
          ("M-p" . previous-error)

          ("M-u" . sp-up-sexp)
          ("M-U" . sp-unwrap-sexp)
          ("M-S" . czm-unwrap-mark-sexp)
          )

    :config
    (put 'LaTeX-narrow-to-environment 'disabled nil)
    (TeX-source-correlate-mode)
    (advice-add 'TeX-view :around #'czm-widen-first) ; fixes bug in TeX-view

    :custom
    (TeX-auto-save t)
    (TeX-parse-self t)
    (preview-auto-cache-preamble t)
    (preview-default-option-list
     '("displaymath" "floats" "graphics" "textmath" "sections" "footnotes" "showlabels"))
                                        ;  (preview-gs-command "/usr/local/bin/gs")  ; compare with rungs?
                                        ;  (preview-image-type 'pnm) ; compare with png?



    (reftex-derive-label-parameters
     '(15 50 t 1 "-"
          ("the" "on" "in" "off" "a" "for" "by" "of" "and" "is" "to")
          t)))

  ;;  don't want foldout to include "bibliography"
  (defun czm-LaTeX-outline-level-advice (orig-fun &rest args)
    (if (looking-at "\\\\bibliography") 1 (apply orig-fun args)))

  (defun my-preview-tailor-factor-function ()
    "ez"
    (if (string-suffix-p ".lean" (buffer-file-name)) 0.6 1.0))

  (use-package preview-tailor
    :elpaca (:host github :repo "ultronozm/preview-tailor.el"
                   :depth nil)
    :after latex
    :config
    (preview-tailor-init)
    :custom
    (preview-tailor-additional-factor-function #'my-preview-tailor-factor-function)
    (preview-tailor-multipliers
     '((((geometry 0 -2520 1920 1080)
         (workarea 0 -2520 1920 1080)
         (mm-size 1434 806)
         (source . "NS"))
        . 4)
       (((geometry -832 -1440 2560 1440)
         (workarea -832 -1440 2560 1440)
         (mm-size 602 338)
         (source . "NS"))
        . 1.3)
       (((geometry 0 0 1728 1117)
         (workarea 0 32 1728 1085)
         (mm-size 344 222)
         (source . "NS"))
        . 1.2)
       (((geometry 0 -1440 2560 1440)
         (workarea 0 -1440 2560 1440)
         (mm-size 602 338)
         (source . "NS"))
        . 1.3)
       (nil . 1.2))))

  (use-package foldout
    :elpaca nil
    :config
    (advice-add 'LaTeX-outline-level :around #'czm-LaTeX-outline-level-advice))

  (use-package smartparens
    :bind
    (:map LaTeX-mode-map
          ("C-M-f" . sp-forward-sexp)
          ("C-M-b" . sp-backward-sexp))


    :config
    (spw/remap-mark-command 'sp-mark-sexp LaTeX-mode-map)

    (defun sp-latex-insert-spaces-inside-pair (_id action _context)
      "ID, ACTION, CONTEXT."
      (when (eq action 'insert)
        (insert "  ")
        (backward-char 1))
      (when (and (eq action 'wrap)
                 (save-excursion
                   (goto-char (sp-get sp-last-wrapped-region :beg-in))
                   (not (sp--looking-back-p "[[{(]"))))
        (save-excursion
          (goto-char (sp-get sp-last-wrapped-region :end-in))
          (insert " ")
          (goto-char (sp-get sp-last-wrapped-region :beg-in))
          (insert " "))))

    (defun sp-latex-skip-match-apostrophe (ms _mb me)
      "MS, MB, ME."
      (when (equal ms "'")
        (save-excursion
          (goto-char me)
          (looking-at-p "\\sw"))))

    (defun sp-latex-skip-double-quote (_id action _context)
      "ID, ACTION, CONTEXT."
      (when (eq action 'insert)
        (when (looking-at-p "''''")
          (delete-char -2)
          (delete-char 2)
          (forward-char 2))))

    (defun sp-latex-point-after-backslash (id action _context)
      "Return t if point follows a backslash, nil otherwise.
This predicate is only tested on \"insert\" action.
ID, ACTION, CONTEXT."
      (when (eq action 'insert)
        (let ((trigger (sp-get-pair id :trigger)))
          (looking-back (concat "\\\\" (regexp-quote (if trigger trigger id))) nil))))

    (add-to-list 'sp-navigate-skip-match
                 '((tex-mode plain-tex-mode latex-mode) . sp--backslash-skip-match))

    (sp-with-modes '(
                     tex-mode
                     plain-tex-mode
                     latex-mode
                     LaTeX-mode
                     )
      (sp-local-pair "`" "'"
                     :actions '(:rem autoskip)
                     :skip-match 'sp-latex-skip-match-apostrophe
                     :unless '(sp-latex-point-after-backslash sp-in-math-p))
      ;; math modes, yay.  The :actions are provided automatically if
      ;; these pairs do not have global definitions.
      (sp-local-pair "$" "$")
      (sp-local-pair "\\[" "\\]"
                     :unless '(sp-latex-point-after-backslash))

      ;; disable useless pairs.
      (sp-local-pair "\\\\(" nil :actions nil)
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "\\\"" nil :actions nil)

      ;; quote should insert ``'' instead of double quotes.  If we ever
      ;; need to insert ", C-q is our friend.
      (sp-local-pair "``" "''"
                     :trigger "\""
                     :unless '(sp-latex-point-after-backslash sp-in-math-p)
                     :post-handlers '(sp-latex-skip-double-quote))

      ;; add the prefix function sticking to {} pair
      (sp-local-pair "{" nil :prefix "\\\\\\(\\sw\\|\\s_\\)*")

      ;; do not add more space when slurping
      (sp-local-pair "{" "}")
      (sp-local-pair "(" ")")
      (sp-local-pair "[" "]")

      ;; pairs for big brackets.  Needs more research on what pairs are
      ;; useful to add here.  Post suggestions if you know some.
      (sp-local-pair "\\left(" "\\right)"
                     :trigger "\\l("
                     :when '(sp-in-math-p)
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\left[" "\\right]"
                     :trigger "\\l["
                     :when '(sp-in-math-p)
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\left\\{" "\\right\\}"
                     :trigger "\\l{"
                     :when '(sp-in-math-p)
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\left|" "\\right|"
                     :trigger "\\l|"
                     :when '(sp-in-math-p)
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\bigl(" "\\bigr)"
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\biggl(" "\\biggr)"
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\Bigl(" "\\Bigr)"
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\Biggl(" "\\Biggr)"
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\bigl[" "\\bigr]"
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\biggl[" "\\biggr]"
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\Bigl[" "\\Bigr]"
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\Biggl[" "\\Biggr]"
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\bigl\\{" "\\bigr\\}"
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\biggl\\{" "\\biggr\\}"
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\Bigl\\{" "\\Bigr\\}"
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\Biggl\\{" "\\Biggr\\}"
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\lfloor" "\\rfloor"
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\lceil" "\\rceil"
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair "\\langle" "\\rangle"
                     :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair  "\\lVert" "\\rVert"
                      :when '(sp-in-math-p)
                      :trigger "\\lVert"
                      :post-handlers '(sp-latex-insert-spaces-inside-pair))
      (sp-local-pair  "\\lvert" "\\rvert"
                      :when '(sp-in-math-p)
                      :trigger "\\lvert"
                      :post-handlers '(sp-latex-insert-spaces-inside-pair))
      ;; (sp-local-pair  "\\left\\lvert" "\\right\\rvert"
      ;;                 :when '(sp-in-math-p)
      ;;                 :trigger "\\left\\lvert"
      ;;                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
      ;; (sp-local-pair  "\\left\\lVert" "\\right\\rVert"
      ;;                 :when '(sp-in-math-p)
      ;;                 :trigger "\\left\\lVert"
      ;;                 :post-handlers '(sp-latex-insert-spaces-inside-pair))

      ;; some common wrappings
      (sp-local-tag "\"" "``" "''" :actions '(wrap))
      (sp-local-tag "\\b" "\\begin{_}" "\\end{_}")
      (sp-local-tag "bi" "\\begin{itemize}" "\\end{itemize}")
      (sp-local-tag "be" "\\begin{enumerate}" "\\end{enumerate}")))

  (use-package spout
    :elpaca (:host github :repo "ultronozm/spout.el"
                   :depth nil)
    :after latex
    :hook
    (LaTeX-mode . spout-mode)
    :custom
    (spout-keys
     '(("n" outline-next-visible-heading)
       ("p" outline-previous-visible-heading)
       ("u" outline-up-heading)
       ("f" outline-forward-same-level)
       ("b" outline-backward-same-level)
       ("M-<left>" outline-promote left-word)
       ("M-<right>" outline-demote right-word)
       ("<" beginning-of-buffer)
       (">" end-of-buffer)
       ("<up>" outline-move-subtree-up windmove-up)
       ("<down>" outline-move-subtree-down windmove-down)
       ("s" outline-show-subtree)
       ("d" outline-hide-subtree)
       ("a" outline-show-all)
       ("q" outline-hide-sublevels)
       ("t" outline-hide-body)
       ("k" outline-show-branches)
       ("l" outline-hide-leaves)
       ("i" outline-insert-heading)
       ("o" outline-hide-other)
       ("@" outline-mark-subtree)
       ("z" foldout-zoom-subtree)
       ("x" foldout-exit-fold)))
    :config
    (require 'texmathp)
    (defun LaTeX-skip-verbatim (orig-fun &rest args)
      (if (eq major-mode 'latex-mode)
          (let ((n 100))
            (apply orig-fun args)
            (while (and (LaTeX-verbatim-p) (> n 0))
              (setq n (- n 1))
              (apply orig-fun args)))
        (apply orig-fun args)))
    (dolist (f '(outline-next-heading
                 outline-previous-heading
                 outline-up-heading
                 outline-forward-same-level
                 outline-backward-same-level))
      (advice-add f :around #'LaTeX-skip-verbatim)))

  (defun latex/kill-environment (arg)
    "Kill forward to end of environment.
With ARG N, kill forward to Nth end of environment;
negative ARG -N means kill backward to Nth start of environment."
    (interactive "p")
    (kill-region (point) (progn (latex/forward-environment arg) (point))))

  (defun latex/backward-kill-environment (arg)
    "Kill back to start of environment.
With ARG N, kill back to Nth start of environment;
negative ARG -N means kill forward to Nth end of environment."
    (interactive "p")
    (kill-region (point) (progn (latex/backward-environment arg) (point))))

  (defun latex/kill-sexp-or-environment (arg)
    (interactive "p")
    (if (looking-at "\\\\begin")
        (latex/kill-environment arg)
      (sp-kill-sexp arg)))

  (defun latex/backward-kill-sexp-or-environment (arg)
    (interactive "p")
    (sp-backward-kill-sexp arg))



  (defun latex/backward-up-list-or-beginning-of-environment (arg)
    (interactive "p")
    (condition-case nil
        (sp-backward-up-sexp arg)
      (scan-error (latex/beginning-of-environment arg))))

  (defun latex/down-list-or-enter-environment (arg)
    (interactive "p")
    (if (looking-at "\\\\begin")
        (progn
          (forward-line)
          (back-to-indentation)
          )
      (sp-down-sexp arg)))

  (global-set-key (kbd "M-u") 'up-list)



  (defun latex/mark-sexp-or-environment (arg)
    (interactive "p")
    (if (looking-at "\\\\begin")
        (progn (push-mark
                (save-excursion
                  (latex/forward-environment arg)
                  (point)))
               (activate-mark))
      (spw/sp-mark-sexp)))

  ;; use smartparens w/ latex

  (use-package latex-extra
    :after latex
    :bind
    (:map latex-extra-mode-map
          ("TAB" . nil)
          ("C-M-SPC" . latex/mark-sexp-or-environment)
          ("C-M-u" . latex/backward-up-list-or-beginning-of-environment)
          ("C-M-g" . latex/down-list-or-enter-environment)
          ("C-M-e" . latex/forward-environment)
          ("C-M-a" . latex/backward-environment)
          ("C-M-f" . sp-forward-sexp)
          ("C-M-b" . sp-backward-sexp)
          ("C-M-k" . latex/kill-sexp-or-environment)
          ("C-M-<backspace>" . latex/backward-kill-sexp-or-environment)
          ("C-s-n" . latex/forward-environment)
          ("C-s-p" . latex/backward-environment)
          ("C-s-e" . latex/forward-environment)
          ("C-s-a" . latex/backward-environment)
          ("C-s-k" . latex/kill-environment)
          ("C-s-<backspace>" . latex/backward-kill-environment)
          )
    :custom
    (latex/override-preview-map nil)
    (latex/override-font-map nil)
    (latex/override-fill-map nil)
    :hook
    (LaTeX-mode . latex-extra-mode))

  (use-package czm-tex-util
    :elpaca (:host github :repo "ultronozm/czm-tex-util.el"
                   :depth nil)
    :after latex)

  (use-package czm-tex-fold
    :elpaca (:host github :repo "ultronozm/czm-tex-fold.el"
                   :depth nil)
    :demand ; otherwise, this doesn't work until the second time you
                                        ; open a .tex file.  but it needs to be loaded after auctex.
    :bind
    (:map TeX-fold-mode-map
          ("C-c C-o C-s" . czm-tex-fold-fold-section)
          ("C-c C-o s" . czm-tex-fold-clearout-section))
    :config
    (czm-tex-fold-set-defaults)
    (czm-tex-fold-install)
    :custom
    (czm-tex-fold-bib-file "~/doit/refs.bib")
    :hook
    (LaTeX-mode . tex-fold-mode))

  ;; the following should perhaps be part of czm-tex-fold:

  (defun czm-tex-fold-macro-previous-word ()
    (interactive)
    (if TeX-fold-mode
        (save-excursion
	         (backward-word)
	         (TeX-fold-item 'macro))))

  (advice-add 'LaTeX-insert-item :after #'czm-tex-fold-macro-previous-word)


  (defun my-yank-after-advice (&rest _)
    "Fold any yanked ref or eqref."
    (when (and (eq major-mode 'latex-mode)
               TeX-fold-mode
               (string-match "\\\\\\(ref\\|eqref\\){\\([^}]+\\)}"
                             (current-kill 0)))
      (czm-tex-fold-macro-previous-word)))

  (advice-add 'yank :after #'my-yank-after-advice)

  ;;

  (use-package czm-tex-jump
    :elpaca (:host github :repo "https://github.com/ultronozm/czm-tex-jump.el.git"
                   :depth nil)
    ;; :after avy
    :bind
    (:map LaTeX-mode-map
          ("s-r" . czm-tex-jump)))

  (use-package czm-tex-ref
    :elpaca (:host github :repo "ultronozm/czm-tex-ref.el"
                   :depth nil)
    :custom
    (czm-tex-ref-master-bib-file "~/doit/refs.bib")
    (czm-tex-ref-rearrange-bib-entries t)
    :bind
    (:map global-map
	         ("C-c 0" . czm-tex-ref-bib))
    (:map LaTeX-mode-map
	         ("C-c 9" . czm-tex-ref-label)
	         ("C-c 0" . czm-tex-ref-bib)))

  ;; (defun czm-attrap-LaTeX-fixer (msg pos end)
  ;;   (cond
  ;;    ((s-matches? (rx "Use either `` or '' as an alternative to `\"'.")msg)
  ;;     (list (attrap-option 'fix-open-dquote
  ;;             (delete-region pos (1+ pos))
  ;;             (insert "``"))
  ;;           (attrap-option 'fix-close-dquote
  ;;             (delete-region pos (1+ pos))
  ;;             (insert "''"))))
  ;;    ((s-matches? (rx "Non-breaking space (`~') should have been used.") msg)
  ;;     (attrap-one-option 'non-breaking-space
  ;;       (if (looking-at (rx space))
  ;;           (delete-region pos (1+ pos))
  ;;         (delete-region (save-excursion (skip-chars-backward "\n\t ") (point)) (point)))
  ;;       (insert "~")))
  ;;    ((s-matches? (rx "Interword spacing (`\\ ') should perhaps be used.") msg)
  ;;     (attrap-one-option 'use-interword-spacing
  ;;       (delete-region (1-  (point))
  ;;                      (point))
  ;;       (insert "\\ ")))
  ;;    ((s-matches? (rx "Delete this space to maintain correct pagereferences.") msg)
  ;;     (attrap-one-option 'fix-space-pageref
  ;;       (if (looking-back (rx bol (* space)))
  ;;           (progn (skip-chars-backward "\n\t ")
  ;;                  (insert "%"))
  ;;         (delete-region (point) (save-excursion (skip-chars-forward " \t") (point)))
  ;; 	)))
  ;;    ((s-matches? (rx "You should enclose the previous parenthesis with `{}'.") msg)
  ;;     (attrap-one-option 'enclose-with-braces
  ;;       (insert "}")
  ;;       (save-excursion
  ;; 	(backward-char)
  ;; 	(backward-sexp)
  ;; 	(re-search-backward "[^[:alnum:]\\_\\/]")
  ;; 	(forward-char)
  ;; 	(insert "{")
  ;; 	)))
  ;;    ((s-matches? (rx "You should not use punctuation in front of quotes.") msg)
  ;;     (attrap-one-option 'swap-punctuation-with-quotes
  ;;       (progn
  ;; 	(delete-char 2)
  ;; 	(backward-char)
  ;; 	(insert "''"))
  ;;       ))))

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
	         (insert "''"))
        ))))

  (use-package emacs
    :elpaca nil
    :after flycheck attrap
    :config
    (add-to-list 'attrap-flycheck-checkers-alist '(tex-chktex . czm-attrap-LaTeX-fixer)))

  (use-package latex-flymake
    :elpaca nil
    :after latex)

  (with-eval-after-load 'attrap
    (setcdr (assoc 'LaTeX-flymake attrap-flymake-backends-alist)
            #'czm-attrap-LaTeX-fixer-flymake))

  (defun czm/latex-tmp-new ()
    "Create new temporary LaTeX buffer."
    (interactive)
    (let ((dir "~/doit/")
	         (filename (format-time-string "tmp-%Y%m%dT%H%M%S.tex")))
      (unless (file-directory-p dir)
        (make-directory dir t))
      (let ((filepath (expand-file-name filename dir)))
        (find-file filepath)
        (save-buffer)
        ;; (czm-preview-timer-toggle)
        )))

  (use-package dynexp
    :elpaca (:host github :repo "ultronozm/dynexp.el"
                   :depth nil)
    :demand ; but after auctex
    :bind
    (:map LaTeX-mode-map
          ("SPC" . dynexp-space)
          ("TAB" . dynexp-next))
    :config
    (with-eval-after-load 'latex
      (quietly-read-abbrev-file "~/.emacs.d/elpaca/repos/dynexp/lisp/dynexp-abbrev.el")))

  (use-package czm-tex-edit
    :elpaca (:host github :repo "ultronozm/czm-tex-edit.el"
                   :depth nil)
    :after latex dynexp
    :demand ; should come after latex and dynexp
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
          ("$" . czm-tex-edit-insert-dollar-or-wrap-region))
    :config
    (czm-tex-edit-define-color-functions-and-bindings
     "C-c t c"
     (("red" . "r") ("green" . "g") ("blue" . "b") ("yellow" . "y") ("orange" . "o") ("purple" . "p") ("black" . "k") ("white" . "w") ("cyan" . "c") ("magenta" . "m") ("lime" . "l") ("teal" . "t") ("violet" . "v") ("pink" . "i") ("brown" . "n") ("gray" . "a") ("darkgreen" . "d") ("lightblue" . "h") ("lavender" . "e") ("maroon" . "u") ("beige" . "j") ("indigo" . "x") ("turquoise" . "q") ("gold" . "f") ("silver" . "s") ("bronze" . "z"))))

  (use-package czm-tex-compile
    :elpaca (:host github :repo "ultronozm/czm-tex-compile.el"
                   :depth nil)
    :bind
    ("C-c k" . czm-tex-compile-toggle))

  (use-package czm-preview
    :elpaca (:host github :repo "ultronozm/czm-preview.el"
                   :depth nil)
    :after latex
    :mode ("\\.tex\\'" . latex-mode)
    :bind
    (:map LaTeX-mode-map
	         ("s-u" . czm-preview-mode)
	         ("C-c p m" . czm-preview-toggle-master))
    :custom
    (czm-preview-TeX-master "~/doit/preview-master.tex")
    (czm-preview-regions-not-to-preview '("<++>" "<+++>"))
    (czm-preview-allowed-files
     '("\\.tex\\(<\\([^>]+\\)>\\)*$"
       "\\[ latex \\]\\*\\(<\\([^>]+\\)>\\)*$"
       "\\.lean$"
       "\\.org$"
       ))
    (czm-preview-predicate #'my-czm-preview-predicate)
    :hook
    (LaTeX-mode . czm-preview-mode-conditionally-enable)

    :config
    (setq-default TeX-PDF-mode nil)
    ;; because texlive 2023 seems super slow
    (with-eval-after-load 'preview
      (let ((tex-dir (when (equal (system-name)
                                  "Pauls-MBP-3")
                       "/usr/local/texlive/2020/bin/x86_64-darwin/")))
        (setq preview-LaTeX-command
	             `(
	               ,(concat
	                 "%`"
	                 tex-dir
	                 "%l \"\\nonstopmode\\nofiles\\PassOptionsToPackage{")
	               ("," . preview-required-option-list)
	               "}{preview}\\AtBeginDocument{\\ifx\\ifPreview\\undefined" preview-default-preamble "\\fi}\"%' \"\\detokenize{\" %(t-filename-only) \"}\""))))

    ;; (setq czm-preview-latex-prefix-directory "/usr/local/texlive/2023/bin/universal-darwin/")
    ;; /usr/local/texlive/2023/bin/universal-darwin/

    )

  (defun current-mmm-mode ()
    "Return current mmm-mode at point."
    (let ((overlays (overlays-at (point)))
          result)
      (while (and overlays (not result))
        (let* ((overlay (car overlays))
               (properties (overlay-properties overlay))
               (mmm-mode (plist-get properties 'mmm-mode)))
          (setq result mmm-mode)
          (setq overlays (cdr overlays))))
      result))

  (defun my-czm-preview-predicate ()
    "Predicate for determining whether to preview.

If major-mode is latex-mode, then return t.

If major-mode is lean4-mode, and if mmm-mode is activated, then
return t precisely when the current mmm-mode is latex-mode.

Otherwise, return nil."
    (cond
     ;; check whether mmm-mode is bound as a symbol first
     (
      (and (boundp 'mmm-mode) mmm-mode)
      (or (eq mmm-primary-mode 'latex-mode)
          (eq (current-mmm-mode) 'latex-mode)))
     ((eq major-mode 'latex-mode) t)
     ;;((eq-major-mode 'org-mode))
     ))

  (use-package library
    :after latex czm-tex-util
    :elpaca (:host github :repo "ultronozm/library.el"
                   :depth nil)
    :custom
    (library-pdf-directory "~/Dropbox/math documents/unsorted/")
    (library-bibtex-file "~/doit/refs.bib")
    (library-download-directory "~/Downloads/")
    (library-org-capture-template-key "j")
    :bind
    ;; ("C-c n" . library-clipboard-to-refs)
    )


  ;; ;; testing this out for a bit, to make sure it works as you hoped
  ;; (defun LaTeX-env-beginning-pos-col ()
  ;;   "Return a cons: (POINT . COLUMN) for current environment's beginning."
  ;;   (save-excursion
  ;;     (LaTeX-find-matching-begin)
  ;;     (LaTeX-back-to-indentation)
  ;;     (cons (point) (current-column))))



  (defun preview--skip-preamble-region (region-text region-offset)
    "Skip preamble for the sake of predumped formats.
Helper function of `TeX-region-create'.

If REGION-TEXT doesn't contain preamble, it returns nil.
Otherwise, it returns cons (ALTERED-TEXT . ALTERED-OFFSET) where
ALTERED-TEXT is REGION-TEXT without the preamble part and
ALTERED-OFFSET is REGION-OFFSET increased by the number of lines
of the preamble part of REGION-TEXT."
    (if (and TeX-header-end (string-match TeX-header-end region-text))
        (cons (substring region-text (match-end 0))
              (with-temp-buffer
                (insert (substring region-text 0 (match-end 0)))
                (+ region-offset (TeX-current-offset))))))


  (defun czm-copy-standard-tex-files ()
    "Copy standard TeX files to the current directory."
    (interactive)
    ;; ask the user if he really wants to copy files into the current directory
    (if (y-or-n-p (format "Copy standard TeX files to %s? " default-directory))
        (let ((files '("~/doit/common.tex" "~/doit/refs.bib")))
          (dolist (file files)
            (let ((source (expand-file-name file))
                  (dest (expand-file-name (file-name-nondirectory file) default-directory)))
              (copy-file source dest t))))
      (message "Aborted.")))


  (when nil
    (defun modify-syntax-table-4-latex ()
      (modify-syntax-entry ?\{ "(}")
      (modify-syntax-entry ?\} "){"))



    (autoload #'latex-forward-sexp "tex-mode" nil t)
    (modify-syntax-entry ?\\ "/" LaTeX-mode-syntax-table)
    (defun fix-LaTeX-sexp ()
      (setq-local forward-sexp-function #'latex-forward-sexp))
    (add-hook 'LaTeX-mode-hook #'fix-LaTeX-sexp)
    ))


;;; ------------------------------ ABBREV and SPELLING ------------------------------

;; this could be its own package, accomodating git-friendly abbrev storage?
;; need a good way to update the source.
(defun modify-abbrev-table (table abbrevs)
  "Define abbreviations in TABLE given by ABBREVS."
  (unless table
                                        ; This probably means that you called this function before the
                                        ; appropriate major mode was loaded.  Hence the ":after" entries
                                        ; in the use-package declaration below
    (error "Abbrev table does not exist" table))
  (dolist (abbrev abbrevs)
    (define-abbrev table (car abbrev) (cadr abbrev) (caddr abbrev))))

(unless (eq window-system 'w32)
  (use-package emacs
    :elpaca nil

    :after cc-mode

    :custom
    (abbrev-file-name (concat user-emacs-directory "abbrev_defs.el"))
    (save-abbrevs 'silently)

    :hook
    (text-mode . abbrev-mode)

    :config
    (let ((abbrev-file (concat user-emacs-directory "abbrev_defs.el")))
      (when (file-exists-p abbrev-file)
        (quietly-read-abbrev-file abbrev-file)))
    (quietly-read-abbrev-file (concat user-emacs-directory "abbrev.el"))))

;; (unless (eq window-system 'w32)
;;   (use-package emacs
;;     :elpaca nil

;;     :after latex cc-mode

;;     :custom
;;     (abbrev-file-name (concat user-emacs-directory "abbrev_defs.el"))
;;     (save-abbrevs 'silently)

;;     :hook
;;     (text-mode . abbrev-mode)

;;     :config
;;     (let ((abbrev-file (concat user-emacs-directory "abbrev_defs.el")))
;;       (when (file-exists-p abbrev-file)
;;         (quietly-read-abbrev-file abbrev-file)))
;;     (quietly-read-abbrev-file (concat user-emacs-directory "abbrev.el"))))


(use-package czm-spell
  :elpaca (:host github :repo "ultronozm/czm-spell.el"
                 :depth nil)
  :bind
  ("s-;" . czm-spell-then-abbrev))

;; Forcing this to load so that c++-mode-abbrev-table is defined.
(use-package cc-mode
  :elpaca nil
  :demand)

;;; --------------------------------- PDF ---------------------------------

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (global-auto-revert-ignore-modes '(pdf-view-mode))
  (pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
  :config
  (pdf-tools-install :no-query)
  (require 'pdf-occur)
  :bind (:map pdf-view-mode-map
              ("<down>" . nil)
              ("<up>" . nil)))


;;; ------------------------------ ORG ------------------------------

(use-package emacs
  :elpaca nil
  :hook
  (org-mode . visual-line-mode)
  :custom
  (org-default-notes-file (concat org-directory "doit/todo.org"))
  (org-directory "~/")
  (org-agenda-files (append '("~/doit/todo.org")
                            (when (file-directory-p "~/ua")
                              '("~/ua/ua-log.org"))))
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
     ("~/doit/todo.org" :regexp . "Inbox")
     ("~/doit/todo.org" :regexp . "Reference")
     ("~/doit/todo.org" :regexp . "Someday")
     ("~/doit/todo.org" :regexp . "Scheduler")
     ("~/doit/todo.org" :regexp . "Tasks")
     ("~/doit/proj-var2.org" :regexp . "Inbox")
     ("~/doit/proj-var2.org" :regexp . "Backlog")
     ("~/doit/todo.org" :regexp . "Backlog")
     ("~/ua/ua-log.org" :regexp . "ua Inbox")))
  (org-refile-use-outline-path t)
; should add to list:  (org-speed-commands '(("B" . org-tree-to-indirect-buffer)))
  (org-src-preserve-indentation t)
  (org-tags-column -70)
  (org-use-speed-commands t)
  (org-capture-templates
   '(
     ("i" "Inbox" entry (file+headline "~/doit/todo.org" "Inbox")
      "* %?\n  %i")
     ("t" "Tasks" entry (file+headline "~/doit/todo.org" "Tasks")
      "* %?\n  %i")
     ("o" "Inbox + link" entry (file+headline "~/doit/todo.org" "Inbox")
      "* %?\n  %i\n  %a\n")
     ("j" "Journal" entry (file+datetree "~/doit/log.org")
      "* %?\nEntered on %U\n")
     ("d" "Daily Review" entry (file+datetree "~/doit/log.org")
      (file "~/doit/daily.org"))
     ("w" "Weekly Review" entry (file+datetree "~/doit/log.org")
      (file "~/doit/weekly.org"))
     ("e" "Emacs quick reference" item (file+headline "~/doit/todo.org" "Emacs quick reference")
      "- %?\n %x\n")
     ("k" "Interruptions" entry (file+headline "~/doit/todo.org" "Interruptions")
      "* %?\n%U\n" :clock-in t :clock-resume t))))

(defun czm-org-edit-latex-with-preview ()
  (interactive)
  (let
      ((src-buffer
        (save-window-excursion
          (org-edit-src-code)
          (setq fill-column 999999) ; should this be in a latex mode hook?
          (current-buffer))))
    (switch-to-buffer src-buffer)))

(defun czm-org-collect-entry-positions (match scope &rest skip)
  "Collect buffer point positions for org entries.
The list is ordered from bottom to top."
  (let ((positions ()))
    (org-map-entries
     (lambda () (setq positions (cons (point) positions))) match scope skip)
    positions))

(defun czm-org-archive-done-entries ()
  (interactive)
  (let ((positions (czm-org-collect-entry-positions "TODO={DONE\\|CANCELLED}" 'region-start-level)))
    (dolist (pos positions)
      (goto-char pos)
      (save-mark-and-excursion
        (deactivate-mark)
        (org-archive-subtree)
        ))))

(use-package org
  :elpaca nil
  :hook
  (org-mode . (lambda () (setq fill-column 999999)))
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
        ("C-c p" . czm-org-edit-latex-with-preview)))

(defun czm-new-tmp-org ()
  "Create new temporary org buffer."
  (interactive)
  (let ((dir "~/doit/")
        (filename (format-time-string "tmp-%Y%m%dT%H%M%S.org")))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (let ((filepath (expand-file-name filename dir)))
      (find-file filepath)
      (save-buffer))))

;;; ------------------------------ PUBLISH ------------------------------

(defun czm-file-is-tex-or-bib (file)
  "Return t if FILE is a .tex or .bib file."
  (or (string-suffix-p ".tex" file)
      (string-suffix-p ".bib" file)))

(use-package publish
  :elpaca (:host github :repo "ultronozm/publish.el"
                 :depth nil)
  :defer t
  :custom
  (publish-repo-root "~/math")
  (publish-disallowed-unstaged-file-predicate #'czm-file-is-tex-or-bib))

;;; ------------------------------ ERC ------------------------------

(use-package erc
  :elpaca nil
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
  (erc-nick '("czM" "czM_"))
  (erc-user-full-name "Paul Nelson")
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
  :elpaca nil
  :after erc
  :config
  (erc-log-mode)
  :custom
  (erc-log-channels-directory "~/.erc/logs/")
  (erc-log-insert-log-on-open t)
  (erc-log-write-after-send t)
  (erc-log-write-after-insert t)
  )

;; (use-package erc-ring
;;   :after erc
;;   :config
;;   (erc-ring-mode))

;; (use-package erc-netsplit
;;   :after erc
;;   :config
;;   (erc-netsplit-mode))

(use-package erc-desktop-notifications
  :elpaca nil
  :after erc
  ;; https://emacs.stackexchange.com/questions/28896/how-to-get-notifications-from-erc-in-macos
  )

; TODO:
;; (erc-modules
;;    '(autojoin button completion desktop-notifications fill imenu irccontrols list log match menu move-to-prompt netsplit networks noncommands notifications readonly ring stamp track))

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

;; use erc-select multiple times to connect to multiple IRC servers?

;; TODO: robust form of check-abbrev?

;;; ------------------------------ SAGE ------------------------------

(unless (eq window-system 'w32)
  (use-package sage-shell-mode
    :defer t
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
    :elpaca (:host github :repo "ultronozm/czm-tex-mint.el"
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
    :elpaca (:host github
                   :repo "ultronozm/symtex.el"
                   :files ("*.el" "*.py")
                   :depth nil)
    :after latex
    :bind
    (:map global-map
          ("C-c V" . symtex-process))
    (:map LaTeX-mode-map
	         ("C-c v" . symtex-dwim))))

;;; ------------------------------ CPP ------------------------------



(c-add-style "llvm4"
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

;; defvar-keymap
;; define-keymap

(defvar flymake-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'flymake-goto-next-error)
    (define-key map (kbd "p") 'flymake-goto-prev-error)
    (define-key map (kbd "f") 'attrap-flymake)
    (define-key map (kbd "M-n") 'flymake-goto-next-error)
    (define-key map (kbd "M-p") 'flymake-goto-prev-error)
    map))

(use-package flymake
  :elpaca nil
  :custom
  (flymake-show-diagnostics-at-end-of-line t)
  :config
  (repeatize 'flymake-repeat-map)
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)))

(use-package emacs
  :elpaca nil
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

;; maybe some of the following should be part of cmake-build.el?

(defun czm-eshell-run (command buffer-name)
  (eshell "new")
  (rename-buffer buffer-name)
  (insert command)
  (eshell-send-input))

(defun czm-cmake-build--invoke-eshell-run (config)
  (let* ((cmake-build-run-config config)
         (config (cmake-build--get-run-config))
         (command (cmake-build--get-run-command config))
         (default-directory (cmake-build--get-run-directory config))
         (process-environment (append
                               (list (concat "PROJECT_ROOT="
                                             (cmake-build--maybe-remote-project-root)))
                               (cmake-build--get-run-config-env)
                               process-environment))
         (buffer-name (string-replace "Run" "eshell-Run" (cmake-build--run-buffer-name))))
    ;; with a bit more sophistication, you should be able to set up a
    ;; proper dedicated window.  might be fun to look into.
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (czm-eshell-run command buffer-name))))

(defun czm-cmake-build-eshell-run ()
  (interactive)
  (when (cmake-build--validate "run")
    (let* ((this-root (cmake-build--project-root))
           (this-run-config cmake-build-run-config)
           (cmake-build-project-root this-root))
      (if  (and nil cmake-build-before-run)
          (cmake-build--invoke-build-current
           (lambda (process event)
             (let* ((this-root this-root)
                    (cmake-build-project-root this-root))
               (when (cl-equalp "finished\n" event)
                 (czm-cmake-build--invoke-eshell-run this-run-config)))))
        (czm-cmake-build--invoke-eshell-run this-run-config)))))

(use-package cmake-build
  :elpaca (:host github :repo "ultronozm/cmake-build.el"
                 :depth nil)
  :bind (("s-m m" . cmake-build-menu)
         ("s-m 1" . cmake-build-set-cmake-profile)
         ("s-m 2" . cmake-build-clear-cache-and-configure)
         ("s-m 3" . cmake-build-set-config)
         ("s-m b" . cmake-build-current)
         ("s-m o" . ff-find-other-file)
         ("s-m r" . cmake-build-run)
         ("s-m e" . czm-cmake-build-eshell-run)
         ("s-m c" . cmake-build-clean))
  :custom
  (cmake-build-override-compile-keymap nil)
  ;; (cmake-build-run-function 'czm-eshell-run)
  (cmake-build-export-compile-commands t)
  (cmake-build-options "-j 1")
  (cmake-build-options "-j 2")
  (cmake-build-options "-j 16")
  (cmake-build-options "-j 8 --verbose"))

(use-package czm-cpp
  :elpaca (:host github :repo "ultronozm/czm-cpp.el"
                 :depth nil)
  :after cmake-build)

;;; ------------------------------ CALC ------------------------------

(defun calcFunc-sage-factor ()
  "Use SAGE to factor the top element of the stack in Emacs Calc."
  (interactive)
  (if (equal (length calc-stack)
             0)
      (error "Stack is empty"))
  (let* ((top-of-stack (calc-top))
         (top-of-stack-string (math-format-value top-of-stack))
                (sage-code
                 (format "SR(\"%s\").factor()" top-of-stack-string))
         (modified-string (symtex-evaluate sage-code))
         (modified-value (math-read-exprs modified-string)))
    (if (eq (car-safe modified-value)
            'error)
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


(use-package calc
  :elpaca nil
  :custom
  (calc-kill-line-numbering t))

(defun czm-calc-grab-TeX-region (beg end arg)
  (interactive "r\nP")
  (with-calc-language 'latex
    (calc-grab-region beg end arg))
  (calc-refresh))

(use-package xr)

(defmacro with-calc-language (lang &rest body)
  "Execute the forms in BODY with `calc-language` set to LANG.
The value of `calc-language` is restored after BODY has been processed."
  `(let ((old-lang calc-language))
     (unwind-protect
         (progn
           (calc-set-language ,lang)
           ,@body)
       (calc-set-language old-lang))))


;;; ------------------------------ LEAN ------------------------------

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

(use-package lean4-mode
  :elpaca (:host github :repo "ultronozm/lean4-mode"
                 :files ("*.el" "data"))
  :hook (lean4-mode . spout-mode)
  :hook (lean4-mode . company-mode)
  :hook (lean4-mode . czm-lean4-set-imenu-generic-expression)
  :commands (lean4-mode)
  :custom
  (lean4-keybinding-lean4-toggle-info (kbd "C-c C-y"))
  (lean4-info-plain nil)
  :bind (:map lean4-mode-map
              ("RET" . newline)
              ("C-j" . default-indent-new-line)
              ("C-c C-q" . eglot-code-action-quickfix)
              ("C-M-i" . consult-company))
  :config
  :defer t)

(use-package czm-lean4
  :elpaca (:host github :repo "ultronozm/czm-lean4.el"
                 :depth nil)
  :after lean4-mode
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
  :custom
  (czm-lean4-info-window-height-fraction 0.4)
  (czm-lean4-info-window-width-fraction 0.47)
  :config
  (advice-add 'lean4-info-buffer-redisplay :around #'czm-lean4-info-buffer-redisplay))

(use-package eldoc-box
  :commands (eldoc-box-help-at-point)
  :bind
  (:map global-map ("C-c e" . eldoc-box-help-at-point)))

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
  :elpaca (:host github :repo "ultronozm/eldoc-icebox.el"
                 :depth nil)
  :bind (("C-c C-h" . eldoc-icebox-store)
         ("C-c C-n" . eldoc-icebox-toggle-display))
  :hook
  (eldoc-icebox-post-display . shrink-window-if-larger-than-buffer)
  (eldoc-icebox-post-display . czm-colorize-lean4-signature)
  (eldoc-icebox-post-display . czm-add-lean4-eldoc))

;;; ------------------------------ MISC ------------------------------

(use-package repo-scan
  :elpaca (:host github :repo "ultronozm/repo-scan.el"
                 :depth nil)
  :defer t)

(use-package pulsar
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

(defvar czm-repos '("ai-org-chat"
                    "czm-cpp"
                    "czm-preview"
                    "czm-spell"
                    "czm-tex-compile"
                    "czm-tex-edit"
                    "czm-tex-fold"
                    "czm-tex-util"
                    "dynexp"
                    "library"
                    "publish"
                    "czm-tex-mint"
                    "spout"
                    "czm-tex-ref"
                    "symtex"
                    "czm-tex-jump"))

(defun czm-repos-uncompiled ()
  (interactive)
  (dolist (name czm-repos)
    (let ((elc-file
           (concat "~/.emacs.d/elpaca/builds/" name "/" name ".elc")))
      (unless (file-exists-p elc-file)
        (message "%s.elc not found" name)))))

(defun czm-pull-my-stuff ()
  (interactive)
  (let* ((repos (append
                 ;; '("~/.emacs.d" "~/.emacs.d/emacsd" "~/doit")
                 (mapcar
                  (lambda (name)
                    (concat user-emacs-directory "elpaca/repos/" name))
                  czm-repos))))
    (repo-scan-pull repos)))

(defun czm-rebuild-my-stuff ()
  (interactive)
  (dolist (repo czm-repos)
    (let ((repo-symbol (intern repo)))
      (elpaca-rebuild repo-symbol))))

(setq debug-on-message nil)
(setq czm-preview--debug t)
(setq czm-preview--debug nil)

(use-package info-colors
  :elpaca (:host github :repo "ubolonton/info-colors")
  :hook (Info-selection . info-colors-fontify-node))

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

(let ((parameters
       '(window-parameters . ((no-other-window . t)
                              (no-delete-other-windows . t)))))
  (setq
   display-buffer-alist
   `(("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
      nil
      (window-parameters (mode-line-format . none)))
     ("\\*\\(?:help\\|grep\\|Completions\\|Occur\\)\\*"
      display-buffer-in-side-window
      (side . bottom) (slot . -1) (preserve-size . (nil . t))
      ,parameters))))

(let ((parameters
       '(window-parameters . ((no-other-window . t)
                              (no-delete-other-windows . t)))))
  (setq
   display-buffer-alist
   `(("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
      nil
      (window-parameters (mode-line-format . none))))))

(with-eval-after-load 'org
  (spw/remap-mark-command 'org-mark-element org-mode-map)
  (spw/remap-mark-command 'org-mark-subtree org-mode-map))

;; sometimes a key to just activate the mark is wanted
(global-set-key "\M-i" (lambda () (interactive) (activate-mark)))
;; resettle the previous occupant
(global-set-key "\M-I" #'tab-to-tab-stop)

(defun czm-set-margins (width)
  "Set the margins of the current window to WIDTH.
Interactively, prompt for WIDTH."
  (interactive "nWidth: ")
  (setq left-margin-width width right-margin-width width)
  (set-window-margins (selected-window) width width))

;; this should be a per-monitor setting, like with preview-tailor.
;; you should generalize that package to support other settings, I
;; guess?  there should be a "tailor" package with the basic get/set
;; functions.  that way, you can easily set margin widths for each
;; monitor.

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

(use-package pos-tip)

;; (setq lsp-log-io t)
(with-eval-after-load 'lsp-mode
  (setq lsp-log-io t))

(use-package consult-company)


(use-package outline
  :elpaca nil
  :bind (:map outline-navigation-repeat-map
              ("C-x" . foldout-exit-fold)
              ("x" . foldout-exit-fold)
              ("C-z" . foldout-zoom-subtree)
              ("z" . foldout-zoom-subtree)
              ("C-a" . outline-show-all)
              ("a" . outline-show-all)
              ("C-c" . outline-hide-entry)
              ("c" . outline-hide-entry)
              ("C-d" . outline-hide-subtree)
              ("d" . outline-hide-subtree)
              ("C-e" . outline-show-entry)
              ("e" . outline-show-entry)
              ("TAB" . outline-show-children)
              ("C-k" . outline-show-branches)
              ("k" . outline-show-branches)
              ("C-l" . outline-hide-leaves)
              ("l" . outline-hide-leaves)
              ("RET" . outline-insert-heading)
              ("C-o" . outline-hide-other)
              ("o" . outline-hide-other)
              ("C-q" . outline-hide-sublevels)
              ("q" . outline-hide-sublevels)
              ("C-s" . outline-show-subtree)
              ("s" . outline-show-subtree)
              ("C-t" . outline-hide-body)
              ("t" . outline-hide-body)
              ("@" . outline-mark-subtree))
  :config
  (repeatize 'outline-navigation-repeat-map))


(set-face-attribute 'default nil :height 150)


; (load (concat user-emacs-directory "uniteai.el"))

(defvar edebug-previous-result-raw nil) ;; Last result returned, raw.
(defun edebug-compute-previous-result (previous-value)
  (if edebug-unwrap-results
      (setq previous-value
                   (edebug-unwrap* previous-value)))
  (setq edebug-previous-result-raw previous-value)
  (setq edebug-previous-result
               (concat "Result: "
                              (edebug-safe-prin1-to-string previous-value)
                              (eval-expression-print-format previous-value))))

;; (defun my-avy-action-copy-and-yank (pt)
;;   "Copy and yank sexp starting on PT."
;;   (avy-action-copy pt)
;;   (yank))

;; (setq avy-dispatch-alist '((?c . avy-action-copy)
;;                            (?k . avy-action-kill-move)
;;                            (?K . avy-action-kill-stay)
;;                            (?m . avy-action-mark)
;;                            (?p . my-avy-action-copy-and-yank)))

(defun czm-search-log ()
  "Search your log files with `rg'."
  (interactive)
  (let ((log-files '("~/doit/log.org" "~/doit/log-old.org" "~/doit/todo.org")))
    (consult--grep "Ripgrep" #'consult--ripgrep-make-builder log-files nil)))

;; lsp-mode calculates line numbers without first calling widen.
;; let's fix that, so that line numbers work in narrowed buffers, too.
(defun my-lsp--cur-line (&optional point)
  (save-restriction
    (widen)
    (1- (line-number-at-pos point))))
(advice-add 'lsp--cur-line :override #'my-lsp--cur-line)

;; doesn't work out of the box with lean4-mode because the "contact"
;; argument to eglot ends up with a non-string argument, which it
;; shouldn't?  you're not exactly sure what's going on there.

;; (use-package eglot-booster
;;       :elpaca (:host github :repo "jdtsmith/eglot-booster"
;;                  :depth nil)
;;   :after eglot
;;       :config        (eglot-booster-mode))

(use-package symbol-overlay
  :bind (("M-s ," . symbol-overlay-put)
         ("M-s n" . symbol-overlay-switch-forward)
         ("M-s p" . symbol-overlay-switch-backward)
         ("M-s m" . symbol-overlay-mode)
         ("M-s n" . symbol-overlay-remove-all)))


(define-minor-mode error-navigation-mode
  "A mode for navigating between errors with M-n and M-p."
  :lighter " ErrNav"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-n") 'next-error)
            (define-key map (kbd "M-p") 'previous-error)
            map))

(define-minor-mode error-navigation-mode
  "A mode for navigating between errors with M-n and M-p."
  :lighter " ErrNav"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap next-error] 'next-error)
            (define-key map [remap previous-error] 'previous-error)
            map))

(use-package go-translate
  :custom
  gts-translate-list '(("fr" "en"))
  :config
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-google-engine) ;; (gts-bing-engine)
                        )
         :render (gts-buffer-render))))

;; next four taken from oantolin's cfg
(defun mark-inside-sexp ()
  "Mark inside a sexp."
  (interactive)
  (let (beg end)
    (backward-up-list 1 t t)
    (setq beg (1+ (point)))
    (forward-sexp)
    (setq end (1- (point)))
    (goto-char beg)
    (push-mark)
    (goto-char end))
  (activate-mark))

(defun kill-inside-sexp ()
  "Kill inside a sexp."
  (interactive)
  (mark-inside-sexp)
  (kill-region (mark) (point)))

;; M-U?
(defun unwrap-sexp ()
  "Unwrap a sexp."
  (interactive)
  (let (end)
    (mark-inside-sexp)
    (delete-char 1)
    (setq end (1- (point)))
    (goto-char (mark))
    (delete-char -1)
    (set-mark end)))

;; M-S?
(defun unwrap-mark-sexp ()
  "Unwrap a sexp and mark the contents."
  (interactive)
  (unwrap-sexp)
  (setq deactivate-mark nil))
