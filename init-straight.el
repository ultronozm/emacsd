;; (setq straight-use-package-by-default t)
;; (setq straight-vc-git-default-protocol 'ssh)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; ------------------------------ GENERAL ------------------------------

(setq custom-file (concat user-emacs-directory "init-custom.el"))
;; (load custom-file)
(load (concat user-emacs-directory "init-personal.el"))

(use-package emacs
  :custom
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
  (tool-bar-mode nil)
  (vc-follow-symlinks t)

  :config
  (electric-pair-mode)
  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-hl-line-mode)
  (minibuffer-depth-indicate-mode)
  (global-auto-revert-mode)
  (setq-default indent-tabs-mode nil)
; (warning-suppress-log-types '((comp))) ; don't care about this anymore?
  (save-place-mode)
  (scroll-bar-mode 0)
  (line-number-mode)
  (column-number-mode)
  (winner-mode))

(use-package recentf
  :custom
  (recentf-max-saved-items 500)
  :config
  (recentf-mode))

(defun czm/clone-indirect-buffer-same-window ()
    "Clone the current buffer in-place."
    (interactive)
    (let (buf)
      (save-window-excursion
        (setq buf
              (call-interactively #'clone-indirect-buffer)))
      (switch-to-buffer buf)))

(define-key global-map (kbd "C-x c") 'czm/clone-indirect-buffer-same-window)

(defun czm/open-downloads-dired ()
  "Open the '~/Downloads' directory in Dired mode."
  (interactive)
  (dired "~/Downloads"))

(define-key global-map (kbd "C-c d") 'czm/open-downloads-dired)

(defun czm/insert-date ()
    "Insert the current date and time."
    (interactive)
    (insert (format-time-string "%Y%m%dT%H%M%S")))


(define-key minibuffer-local-map (kbd "C-c d") 'czm/insert-date)


(defun czm/find-newest-pdf-in-Downloads ()
  "Find the newest PDF file in ~/Downloads."
  (interactive)
  (let* ((downloads-dir (expand-file-name "~/Downloads"))
         (pdf-files (directory-files downloads-dir t "\\.pdf$"))
         (pdf-files-sorted (sort pdf-files
                                 (lambda (a b)
                                  (time-less-p (nth 5 (file-attributes b))
                                               (nth 5 (file-attributes a)))))))
    (if pdf-files
        (find-file (car pdf-files-sorted))
      (message "No PDF files found in ~/Downloads."))))

(defun czm/resize-frame-to-bottom-third ()
  "Reshape current frame to occupy bottom third of the screen."
  (interactive)
  (if (frame-parameter (selected-frame) 'fullscreen)
      (toggle-frame-fullscreen))
  (redisplay)
  (let* ((window-system-frame-alist
          (cdr (assq initial-window-system
                     window-system-default-frame-alist)))
         (screen-width (display-pixel-width))
         (screen-height (display-pixel-height))
         (frame-padding-chars (frame-parameter nil 'internal-border-width))
         (frame-padding (* (frame-char-height) frame-padding-chars))
         (frame-top (- screen-height (/ screen-height 3)))
         (frame-height (- (/ screen-height 3) (* 2 frame-padding)))
         (frame-width (- screen-width frame-padding))
         (frame-left 0))
    (set-frame-position (selected-frame) frame-left frame-top)
    (set-frame-size (selected-frame) frame-width frame-height t)))

(use-package prog-mode
  :hook
  (prog-mode . outline-minor-mode)
  (prog-mode . hs-minor-mode))

;;; ------------------------------ GIT ------------------------------

(use-package magit
  :straight magit)

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

(use-package eldoc
  :custom
  ;  (eldoc-echo-area-use-multiline-p truncate-sym-name-if-fiteldoc-echo-area-use-multiline-p)
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-idle-delay 0.25))

(use-package copilot
  :straight (copilot :host github :repo "zerolfx/copilot.el"
                     :files ("*.el" "dist"))
  :hook ((prog-mode LaTeX-mode) . copilot-mode)
  :bind (:map copilot-completion-map
              ("§" . copilot-accept-completion)))

(straight-use-package '(auctex :source el-get
                        :files ("*.el" "*.info" "dir"
                                "doc" "etc" "images" "latex" "style")))

(use-package ef-themes
  :straight ef-themes
  :demand t
  :config
  (load-theme 'ef-elea-dark t))

(use-package vertico
  :straight vertico
  :config
  (vertico-mode))

(use-package marginalia
  :straight marginalia
  :demand
  :after vertico
  :config
  (marginalia-mode)
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle)))

(use-package orderless
  :straight orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic)))

(use-package consult
  :straight consult
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
  ::straight embark
  
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
  :straight embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; (use-package savehist
;;   :ensure t
;;   (completion-category-overrides '((file (styles basic partial-completion))))
;;   :init
;;   (savehist-mode))


(use-package company
  :straight company
  ;; :config (global-company-mode 1) ;; doesn't work in magit, for instance
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup.  Consider changing to 0.0?
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
        ("C-n". company-select-next)
        ("C-p". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete)))

; TODO: activate company mode?  what does this do, anyway?





;; unless (string-equal system-type "windows-nt")

(use-package exec-path-from-shell
  :straight exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package gptel
  :straight gptel
  :after exec-path-from-shell
  :custom
  (gptel-model "gpt-4")
  :config
  (setq gptel-api-key (exec-path-from-shell-getenv "OPENAI_API_KEY")))

(use-package ai-threaded-chat
  :straight (ai-threaded-chat :host github :repo "ultronozm/ai-threaded-chat.el")
  :after gptel
    :bind
  (:map global-map
        ("s-/" . ai-threaded-chat-new)
        ("C-M-; /" . czm/avy-ai-chat-new-region))
  (:map ai-threaded-chat-minor-mode
        ("s-<return>" . ai-threaded-chat-respond)
        ("C-c n" . ai-threaded-chat-append-top-level-heading))
  :commands (ai-threaded-chat-minor-mode) ; for manual activation
  :custom
  (ai-threaded-chat-user-name "Paul")
  (ai-threaded-chat-dir "~/gpt")
  (ai-threaded-chat-prompt-preamble
   "You are a brilliant and helpful assistant.

You know everything about programming: languages, syntax, debugging techniques, software design, code optimization, documentation.

Respond with Emacs org-mode syntax.  For example, when providing code examples, do NOT use triple backticks and markdown, but rather source blocks, e.g.:
#+begin_src elisp
  (number-sequence 0 9)
#+end_src

Avoid attempting to give the answer right away.  Instead, begin by breaking any problem down into steps.

Don't attempt nontrivial calculations directly.  In particular, you are unable to directly answer any question that requires analyzing text as a sequence of characters (e.g., counting length, reversing strings), counting of more than several items (e.g., words in a sequence or items in a list), or arithmetic that a human could not perform easily in their head.  In such cases, return a source block containing relevant elisp or Python code.  For example, if the question is \\"What is 7 + 19^3\\", you could return either of the following:

#+begin_src elisp
  (+ 7 (expt 19 3))
#+end_src

#+begin_src python
  return 7 + 19 ** 3
#+end_src

When faced with a complicated word problem, reduce it first to a problem in algebra, then solve it using elisp or Python.  For example, if the problem reduces to computing the binomial coefficient \\"20 choose 13\\", then you could return:

#+begin_src python
  from math import factorial
  return factorial(20) / ( factorial(13) * factorial(20-13) )
#+end_src

Or if you need to solve an equation, use something like sympy:

#+begin_src python
  from sympy import symbols, solve
  x = symbols('x')
  solution = solve([2*x + x + x + 2 - 26, x>0], x)
  return solution
#+end_src

Never describe the results of running code.  Instead, wait for me to run the code and then ask you to continue."))


(use-package avy
  :straight avy
  :bind
  (:map global-map
        ("C-;" . avy-goto-line)
        ("C-M-; y" . avy-copy-region)
        ("C-M-; n" . avy-kill-ring-save-region)
        ("C-M-; t" . avy-move-region)
        ("C-M-; x" . avy-kill-region))
  (:map isearch-mode-map
        ("M-j" . avy-isearch)))


(use-package ace-window
  :straight ace-window
  :bind
  ("C-x o" . ace-window))





(use-package which-key
  :straight which-key
  :config
  (which-key-mode))

(use-package ace-link ; activate using 'o' in info/help/(...)
  :straight ace-link
  :config
  (ace-link-setup-default))

(use-package zzz-to-char
  :straight zzz-to-char
  :bind
  ("s-t" . zzz-to-char-up-to-char)
  ("s-f" . zzz-to-char)
  :custom
  (zzz-to-char-reach 200))

;;; ------------------------------ REPEAT ------------------------------

(use-package repeat
  :config
  (setcdr other-window-repeat-map nil)
  (repeat-mode))

(defun repeatize (keymap)
  "Add `repeat-mode' support to a KEYMAP."
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map keymap)))
   (symbol-value keymap)))

;;; ------------------------------ FLYCHECK ------------------------------

(use-package flycheck
  :straight flycheck
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))


(use-package flycheck-package
  :straight flycheck-package
  :hook
  (emacs-lisp-mode . flycheck-package-setup))

;;; ------------------------------ ATTRAP ------------------------------

(use-package attrap
  :straight attrap
  :after flycheck
  :config
  (setq saved-match-data nil))

(use-package emacs
  :after flycheck attrap repeat
  :config
  (define-key flycheck-command-map "f" 'attrap-flycheck)
  (repeatize 'flycheck-command-map))


;;; ------------------------------ LISP ------------------------------

(use-package emacs
  :custom
  (delete-pair-blink-delay 0)
  :bind
  (:map emacs-lisp-mode-map
        ("M-_" . delete-pair)
        ("M-+" . kill-backward-up-list)))

(use-package lispy
  :straight lispy
  :config
  (setcdr lispy-mode-map nil)
  (let ((map lispy-mode-map))
    (lispy-define-key map ">" 'lispy-slurp-or-barf-right)
    (lispy-define-key map "<" 'lispy-slurp-or-barf-left)
    (lispy-define-key map "/" 'lispy-splice)
    (lispy-define-key map "+" 'lispy-join)
    (define-key map (kbd "C-M-j") 'lispy-split)
    (lispy-define-key map "c" 'lispy-clone)
    (lispy-define-key map ";" 'lispy-comment)
    (define-key map (kbd "\"") 'lispy-quotes)
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
  :hook
  (emacs-lisp-mode  . lispy-mode)
  (minibuffer-setup . czm-conditionally-enable-lispy))



;;; OLD: ------------------------------------------------------------





;; (require 'tex-site)
;; (require 'preview-latex)

(use-package latex
  ;; :ensure auctex ; makes latex easier to use
  :straight auctex
  :demand t
  :init
  (setq TeX-PDF-mode t) ; compile tex as PDF
  (setq TeX-command-force "LaTex") ; Don’t ask for options like View, Latex etc..
  (setq TeX-view-program-list '(("zathura" "zathura %o"))
            TeX-view-program-selection '((output-pdf "zathura")))
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (setq TeX-source-correlate-start-server t)
  (add-hook 'LaTeX-mode-hook
        (lambda ()
          (add-hook 'after-save-hook 'TeX-command-master nil t)))
  :config
  ;;(my/map-keys `(("C-c l l" ,#'Tex-command-master "pdf")) 'org-mode-map)
)
    

;; (straight-use-package 'use-package)

;; (when nil
;;   (use-package copilot-emacsd
;;     :straight (copilot-emacsd :type git :host github :repo
;; 			      "rksm/copilot-emacsd"
;; 			      ))
;; )

;; (when nil
;;   )
 
