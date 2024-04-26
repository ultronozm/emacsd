;;; -*- lexical-binding: t; -*-

;;; ------------------------------ ELPACA ------------------------------

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
  (setq use-package-always-ensure t)
)

(elpaca-wait)

(use-package exec-path-from-shell
  :demand
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(elpaca-wait)

;; (use-package org
;;   :ensure `(org
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
  (:map isearch-mode-map
        ("M-j" . avy-isearch)))

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
         ("C-x j" . czm-misc-dired-popup)
         ("C-;" . czm-misc-avy-goto-or-copy-line))
  (:map minibuffer-local-map
        ("C-c d" . czm-misc-insert-date)))

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode)
  (LaTeX-mode . aggressive-indent-mode))

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

;;; ------------------------------ REPEAT ------------------------------


(defun czm-fill-previous-paragraph ()
  "Fill the previous paragraph."
  (interactive)
  (save-excursion
    (previous-line)
    (fill-paragraph)))

(use-package define-repeat-map
  :ensure (:host nil :repo "https://tildegit.org/acdw/define-repeat-map.el")
  :demand t

  :config
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
     "q" czm-fill-previous-paragraph
     "C-l" recenter-top-bottom))
  (repeat-mode 1))


;;; ------------------------------ LISP ------------------------------

(defun czm-deactivate-mark-interactively ()
  "Deactivate the mark interactively."
  (interactive)
  (deactivate-mark))

(defun czm-lispy-comment-maybe ()
  "Comment the list at point, or self-insert."
  (interactive)
  (if (looking-at "(")
      (lispy-comment)
    (call-interactively #'self-insert-command)))

(use-package lispy
  :after define-repeat-map
  :demand t
  :config
  (define-repeat-map structural-edit
    ("n" forward-list
     "p" backward-list
     "u" backward-up-list
     "M-u" up-list
     "g" down-list)
    (:continue
     "M-g" czm-backward-down-list
     "f" forward-sexp
     "b" backward-sexp
     "a" beginning-of-defun
     "e" end-of-defun
     "d" czm-deactivate-mark-interactively
     "k" kill-sexp
     "x" eval-last-sexp
     "o" lispy-oneline
     "m" lispy-multiline
     "j" lispy-split
     "+" lispy-join
     ">" lispy-slurp-or-barf-right
     "<" lispy-slurp-or-barf-left
     "C-/" undo
     "/" lispy-splice
     ";" lispy-comment
     "r" lispy-raise
     ;;  "r" raise-sexp
     ;; "/" delete-pair
     "t" transpose-sexps
     "w" kill-region
     "M-w" kill-ring-save
     "y" yank
     "c" lispy-clone
     "C-M-SPC" mark-sexp
     "RET" newline-and-indent
     "i" lispy-tab
     "<up>" outline-move-subtree-up
     "<down>" outline-move-subtree-down))
  (repeat-mode 1)

  :commands (lispy-comment)
  
  :bind
  (:map emacs-lisp-mode-map
        (";" . czm-lispy-comment-maybe)))

;;     ;; (lispy-define-key map "w" 'lispy-move-up)
;;     ;; (lispy-define-key map "s" 'lispy-move-down)
;;     ;; (lispy-define-key map "A" 'lispy-beginning-of-defun)
;;     ;; (lispy-define-key map "C" 'lispy-convolute)
;;     ;; (lispy-define-key map "X" 'lispy-convolute-left)
;;     ;; (lispy-define-key map "q" 'lispy-ace-paren)
;;     ;; (lispy-define-key map "-" 'lispy-ace-subword)
;;     ;; (lispy-define-key map "e" 'lispy-eval)
;;   ;; ;; (("C-M-K" . lispy-kill))
;;   ;; (("C-M-k" . kill-sexp))

(defun czm-edebug-eval-hook ()
  (lispy-mode 0)
  (copilot-mode 0)
  (aggressive-indent-mode 0))
(add-hook 'edebug-eval-mode-hook #'czm-edebug-eval-hook)

(use-package info-colors
  :ensure (:host github :repo "ubolonton/info-colors")
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


;; sometimes a key to just activate the mark is wanted
;; (global-set-key "\M-i" (lambda () (interactive) (activate-mark)))
;; resettle the previous occupant
;; (global-set-key "\M-I" #'tab-to-tab-stop)

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

(use-package pos-tip)

;; (setq lsp-log-io t)
(with-eval-after-load 'lsp-mode
  (setq lsp-log-io t))

(use-package consult-company)

(use-package outline
  :ensure nil
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
     "x" foldout-exit-fold
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



                                        ; (load (concat user-emacs-directory "uniteai.el"))

;; (defun my-avy-action-copy-and-yank (pt)
;;   "Copy and yank sexp starting on PT."
;;   (avy-action-copy pt)
;;   (yank))

;; (setq avy-dispatch-alist '((?c . avy-action-copy)
;;                            (?k . avy-action-kill-move)
;;                            (?K . avy-action-kill-stay)
;;                            (?m . avy-action-mark)
;;                            (?p . my-avy-action-copy-and-yank)))


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
;;       :ensure (:host github :repo "jdtsmith/eglot-booster"
;;                  :depth nil)
;;   :after eglot
;;       :config        (eglot-booster-mode))

(use-package symbol-overlay
  :bind (("M-s ," . symbol-overlay-put)
         ("M-s n" . symbol-overlay-switch-forward)
         ("M-s p" . symbol-overlay-switch-backward)
         ("M-s m" . symbol-overlay-mode)
         ("M-s n" . symbol-overlay-remove-all)))

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

(use-package rustic
  :defer t
  :custom
  (rustic-lsp-client 'eglot))

;;; ------------------------------ ESSENTIAL PACKAGES ------------------------------

;; (use-package eldoc
;;   :ensure nil
;;   :custom
;;   ;  (eldoc-echo-area-use-multiline-p truncate-sym-name-if-fiteldoc-echo-area-use-multiline-p)
;;   (eldoc-echo-area-use-multiline-p t)
;;   (eldoc-idle-delay 0.25))

(use-package ef-themes
  :demand
  :config
  ;; (load-theme 'modus-vivendi t)
  ;; (load-theme 'modus-operandi t)
  (load-theme 'ef-frost t)
  ;; (load-theme 'ef-elea-dark t)

  ;; (let ((hour (string-to-number (substring (current-time-string) 11 13))))
  ;;   (cond
  ;;    ((<= hour 7)
  ;;     (load-theme 'modus-vivendi t))
  ;;    ((<= hour 19)
  ;;     (load-theme 'ef-frost t))
  ;;    (t
  ;;     (load-theme 'ef-autumn t))))
  )

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

(defun czm-consult-imenu-emacsd ()
  "Call =consult-imenu-multi= for project =~/.emacs.d/emacsd=."
  (interactive)
  (let ((consult-project-function
         (lambda (may-prompt) "~/.emacs.d/emacsd/")))
    (with-current-buffer "init-main.el"
      (consult-imenu-multi))))

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
         ("s-I" . czm-consult-imenu-emacsd)
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
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  ;; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(setq-default completion-in-region-function 'consult-completion-in-region)

(use-package corfu
  :disabled

  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto nil)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  :bind (:map corfu-map
              ("C-M-i" . nil)
              ("C-a" . nil)
              ("C-g" . nil)
              ("<down>" . nil)
              ("<tab>" . nil)
              ("<up>" . nil)
              ([remap beginning-of-buffer] . nil)
              ([remap completion-at-point] . nil)
              ([remap end-of-buffer] . nil)
              ([remap move-beginning-of-line] . nil)
              ([remap move-end-of-line] . nil)
              ([remap next-line] . nil)
              ([remap previous-line] . nil)
              ([remap scroll-down-command] . nil)
              ([remap scroll-up-command] . nil))

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package ace-window
  :bind
  ("C-x o" . ace-window))

(use-package which-key
  :config
  (which-key-mode))

(use-package ace-link ; activate using 'o' in info/help/(...)
  :config
  (ace-link-setup-default))

(use-package xr)


(use-package eldoc-box
  :commands (eldoc-box-help-at-point)
  :bind
  (:map global-map ("C-c e" . eldoc-box-help-at-point)))

;;; ------------------------------ AI ------------------------------

(use-package copilot
  :ensure (:host github
                 :repo "zerolfx/copilot.el"
                 ;; :repo "ultronozm/copilot.el"
                 :files ("*.el" "dist")
                 :depth nil)
  :hook
  ((prog-mode LaTeX-mode git-commit-mode) . copilot-mode)
  (emacs-lisp-mode . (lambda () (setq tab-width 1)))
  (lean4-mode . (lambda () (setq tab-width 2)))

  :config
  (add-to-list 'warning-suppress-types '(copilot copilot-exceeds-max-char))

  :custom
  (copilot-indent-offset-warning-disable t)
  :bind
  (:map global-map
        ("H-x" . copilot-mode))
  (:map copilot-completion-map
        ("§" . copilot-accept-completion)
        ("M-§" . copilot-accept-completion-by-word)
        ("C-§" . copilot-accept-completion-by-line)
        ("C-M-§" . copilot-accept-completion-by-paragraph)
        ("`" . nil)
        ("M-`" . copilot-accept-completion-by-word)
        ("C-`" . copilot-accept-completion-by-line)
        ("C-M-`" . copilot-accept-completion-by-paragraph)
        ("C-M-<down>" . copilot-next-completion)
        ("C-M-<up>" . copilot-previous-completion)
        ;; ("`" . copilot-accept-completion)
        ;; ("M-`" . copilot-accept-completion-by-word)
        ;; ("C-`" . copilot-accept-completion-by-line)
        ;; ("C-M-`" . copilot-accept-completion-by-paragraph)
        ;; ("C-M-<down>" . copilot-next-completion)
        ;; ("C-M-<up>" . copilot-previous-completion)
        ))

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
   gptel-backend gptel--openai))

(use-package ai-org-chat
  :ensure (:host github :repo "ultronozm/ai-org-chat.el"
                 :depth nil)
  :bind
  (:map global-map
        ("s-/" . ai-org-chat-new))
  (:map ai-org-chat-minor-mode
        ("s-<return>" . ai-org-chat-respond)
        ("C-c n" . ai-org-chat-branch))
  :commands (ai-org-chat-minor-mode) ; for manual activation
  :custom
  (ai-org-chat-user-name my-first-name)
  (ai-org-chat-dir my-tmp-gpt-dir)
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


;;; ------------------------------ FLYCHECK / FLYMAKE ------------------------------



;; C-c ! C-c	flycheck-compile
;; C-c ! C-w	flycheck-copy-errors-as-kill
;; C-c ! ?		flycheck-describe-checker
;; C-c ! C		flycheck-clear
;; C-c ! H		display-local-help
;; C-c ! V		flycheck-version
;; C-c ! c		flycheck-buffer
;; C-c ! e		flycheck-explain-error-at-point
;; C-c ! f		attrap-flycheck
;; C-c ! h		flycheck-display-error-at-point
;; C-c ! i		flycheck-manual
;; C-c ! l		flycheck-list-errors
;; C-c ! n		flycheck-next-error
;; C-c ! p		flycheck-previous-error
;; C-c ! s		flycheck-select-checker
;; C-c ! v		flycheck-verify-setup
;; C-c ! x		flycheck-disable-checker

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
  (flymake-show-diagnostics-at-end-of-line t)
  :after define-repeat-map
  :config
  (define-repeat-map flymake-repeat-map
    ("n" flymake-goto-next-error
     "p" flymake-goto-prev-error
     "f" attrap-flymake
     "M-n" flymake-goto-next-error
     "M-p" flymake-goto-prev-error
     "l" flymake-show-diagnostics-buffer
     ))
  (repeat-mode 1)
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)))

;;; ------------------------------ ATTRAP ------------------------------

(use-package attrap
  :after flycheck
  :config
  (setq saved-match-data nil))

(use-package emacs
  :ensure nil
  :after flycheck attrap repeat
  :config
  (define-key flycheck-command-map "f" 'attrap-flycheck)
  (put 'attrap-flycheck 'repeat-map 'flymake-repeat-map))

;;; ------------------------------ MARK ------------------------------

(when nil
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
    (spw/remap-mark-command command)))

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

(use-package czm-spell
  :ensure (:host github :repo "ultronozm/czm-spell.el"
                 :depth nil)
  :after latex
  :bind
  ("s-;" . czm-spell-then-abbrev))

;; Forcing this to load so that c++-mode-abbrev-table is defined.
(use-package cc-mode
  :ensure nil
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
              ("j" . pdf-view-jump-to-register)
              ("<down>" . nil)
              ("<up>" . nil)))

;;; ------------------------------ ORG ------------------------------

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
     ("k" "Interruptions" entry (file+headline my-todo-file "Interruptions")
      "* %?\n%U\n" :clock-in t :clock-resume t))))

(defun czm-org-edit-src ()
  (interactive)
  (let
      ((src-buffer
        (save-window-excursion
          (org-edit-src-code)
          (setq fill-column 999999) ; should this be in a latex mode hook?
          (setq TeX-master my-preview-master)
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
        (org-archive-subtree)))))

(use-package org
  :ensure nil
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
     ;; "M-h" spw/mark-paragraph
     ;; "h" spw/mark-paragraph
     "k" kill-paragraph
     "w" kill-region
     "M-w" kill-ring-save
     "y" yank
     "C-/" undo
     "t" transpose-paragraphs
     "q" czm-fill-previous-paragraph))
  (repeat-mode 1))

(defun czm-new-tmp-org ()
  "Create new temporary org buffer."
  (interactive)
  (let ((dir (file-name-as-directory my-tmp-org-dir))
        (filename (format-time-string "tmp-%Y%m%dT%H%M%S.org")))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (let ((filepath (expand-file-name filename dir)))
      (find-file filepath)
      (save-buffer))))

(defun czm-search-log ()
  "Search your log files with `rg'."
  (interactive)
  (let ((log-files `(,my-log-file ,my-old-log-file ,my-todo-file)))
    (consult--grep "Ripgrep" #'consult--ripgrep-make-builder log-files nil)))

;;; ------------------------------ ERC ------------------------------

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
  :after erc
  ;; https://emacs.stackexchange.com/questions/28896/how-to-get-notifications-from-erc-in-macos
  )

;; (erc-modules
;;    '(autojoin button completion desktop-notifications fill imenu irccontrols list log match menu move-to-prompt netsplit networks noncommands notifications readonly ring stamp track))

;; use erc-select multiple times to connect to multiple IRC servers?

;; TODO: robust form of check-abbrev?


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
  :ensure (:host github :repo "ultronozm/cmake-build.el"
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
  :ensure (:host github :repo "ultronozm/czm-cpp.el"
                 :files ("*.el" "template")
                 :depth nil)
  :custom
  (czm-cpp-scratch-directory my-tmp-cpp-dir))

(add-to-list 'auto-mode-alist '("\\.ixx\\'" . c++-mode))

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

(defun flymake--update-eol-overlays ()
  "Update the `before-string' property of end-of-line overlays."
  (save-restriction
    (widen)
    (dolist (o (overlays-in (point-min) (point-max)))
      (when (overlay-get o 'flymake--eol-overlay)
        (if-let ((src-ovs (overlay-get o 'flymake-eol-source-overlays)))
            (overlay-put o 'before-string (flymake--eol-overlay-summary src-ovs))
          (delete-overlay o))))))

(use-package perfect-margin)

(use-package diminish
  :demand t
  :after copilot
  :config
  (diminish 'copilot-mode "Co")
  (diminish 'abbrev-mode "Ab")
  (diminish 'lean4-mode)
  (diminish 'visual-line-mode)
  (diminish 'outline-minor-mode)
  (diminish 'which-key-mode)
  (diminish 'buffer-face-mode)
  (diminish 'eldoc-mode)
  (diminish 'reftex-mode)
  (diminish 'copilot-mode)
  (diminish 'aggressive-indent-mode)
  (diminish 'perfect-margin-mode)
  (diminish 'whitespace-mode))

(defvar git-fill-column-alist '(("emacs" . 64) ("auctex" . 64)))

(defun set-git-commit-fill-column ()
  (when-let ((project (project-current))
             (root (project-root project))
             (name (file-name-nondirectory (directory-file-name root)))
             (assn (assoc name git-fill-column-alist)))
    (setq fill-column (cdr assn))))

(add-hook 'git-commit-mode-hook 'set-git-commit-fill-column)

;; maybe this is all that's needed to get indirect buffers to work?

(defun set-TeX-master-from-cloned ()
  (when (eq major-mode 'LaTeX-mode)
    (setq TeX-master (with-current-buffer (buffer-base-buffer)
                       (TeX-master-file)))))

(add-hook 'clone-indirect-buffer-hook 'set-TeX-master-from-cloned)
