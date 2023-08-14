;;; ------------------------------ PACKAGE ------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq custom-file "~/.emacs.d/init-custom.el")
;; (load custom-file)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;;; ------------------------------ MINIMAL CONFIG ------------------------------

(load "~/.emacs.d/init-bare.el")
(load "~/.emacs.d/init-personal.el")

;;; ------------------------------ GENERAL ------------------------------

(use-package emacs
  :bind
  (:map global-map
)
  :custom
  (use-dialog-box nil)
  (show-paren-delay 0)
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
  (column-number-mode))

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
  "Resize and reposition the current frame to occupy the bottom third of the screen"
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
;;; ------------------------------ PROJECT ------------------------------

(cl-defmethod project-root ((project (head local)))
  (cdr project))

(defun czm/project-try-local (dir)
  "Determine if DIR is a non-Git project.
DIR must include a .project file to be considered a project."
  (let ((root (locate-dominating-file dir ".project")))
    (and root (cons 'local root))))

(use-package project
  :config
  (add-to-list 'project-find-functions 'czm/project-try-local))

;;; ------------------------------ ABBREV and SPELLING ------------------------------

(use-package emacs
  :custom
  (abbrev-file-name "~/.emacs.d/abbrev_defs.el")
  (save-abbrevs 'silently)
  
  :hook
  (text-mode . abbrev-mode)
  
  :config
  (quietly-read-abbrev-file "~/.emacs.d/abbrev_defs.el")
  (defun modify-abbrev-table (table abbrevs)
    "Define abbreviations in TABLE given by ABBREVS."
    (dolist (abbrev abbrevs)
      (define-abbrev table (car abbrev) (cadr abbrev) (caddr abbrev))))
  (quietly-read-abbrev-file "~/.emacs.d/abbrev.el"))

(use-package czm-spell
  :vc (:url "https://github.com/ultronozm/czm-spell.el.git"
            :rev :newest)
  :bind ("s-;" . czm-spell-then-abbrev))

;;; --------------------------------- PROJECT ---------------------------------

;; Declare directories with ".project" as a project
(cl-defmethod project-root ((project (head local)))
  (cdr project))

(defun czm/project-try-local (dir)
  "Determine if DIR is a non-Git project.
DIR must include a .project file to be considered a project."
  (let ((root (locate-dominating-file dir ".project")))
    (and root (cons 'local root))))

(use-package emacs
  :config
  (add-to-list 'project-find-functions 'czm/project-try-local))

;;; ------------------------------ GIT ------------------------------

(use-package magit
  :ensure)

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
	  (magit-push-current-to-upstream nil)))))))


;;; ------------------------------ ESSENTIAL PACKAGES ------------------------------

(use-package eldoc
  ; built-in
  :custom
  ;  (eldoc-echo-area-use-multiline-p truncate-sym-name-if-fiteldoc-echo-area-use-multiline-p)
  (eldoc-echo-area-use-multiline-p t))

(use-package copilot
  :vc (:url "https://github.com/ultronozm/copilot.el.git"
            :rev :newest)
  :hook ((prog-mode LaTeX-mode) . copilot-mode)
  :bind (:map copilot-completion-map
              ("§" . copilot-accept-completion)))



;; (use-package ef-themes
;;   :ensure t
;;   :demand t
;;   :config
;;   (load-theme 'ef-elea-dark t))

(use-package vertico
  :ensure
  :config
  (vertico-mode))

(use-package marginalia
  :ensure
  :demand
  :after vertico
  :config
  (marginalia-mode)
  :bind (:map minibuffer-local-map                                          
         ("M-A" . marginalia-cycle)))

(use-package orderless
  :ensure t
  :after vertico
  :custom
  (completion-styles '(orderless basic)))

(use-package consult
  :ensure t
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
  :ensure t
  
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
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; (use-package savehist
;;   :ensure t
;;   (completion-category-overrides '((file (styles basic partial-completion))))
;;   :init
;;   (savehist-mode))


(use-package company
  :ensure t
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
  :ensure
  :init
  (exec-path-from-shell-initialize))

(use-package gptel
  :ensure
  :after exec-path-from-shell
  :custom
  (gptel-model "gpt-4")
  :config
  (setq gptel-api-key (exec-path-from-shell-getenv "OPENAI_API_KEY")))

(use-package ai-threaded-chat
  :vc (:url "https://github.com/ultronozm/ai-threaded-chat.el.git"
            :rev :newest)
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
  :ensure t
  :demand t
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
  :ensure t
  :bind
  ("C-x o" . ace-window))


(use-package flycheck
  :ensure t
  :config
  (defun repeatize (keymap)
    "Add `repeat-mode' support to a KEYMAP."
    (map-keymap
     (lambda (_key cmd)
       (when (symbolp cmd)
         (put cmd 'repeat-map keymap)))
     (symbol-value keymap)))
  (repeatize 'flycheck-command-map)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-package
  :ensure t
  :hook
  (emacs-lisp-mode . flycheck-package-setup))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package ace-link ; activate using 'o' in info/help/(...)
  :ensure t
  :config
  (ace-link-setup-default))

(use-package zzz-to-char
  :ensure t
  :bind
  ("s-t" . zzz-to-char-up-to-char)
  ("s-f" . zzz-to-char)
  :custom
  (zzz-to-char-reach 200))

;;; ------------------------------ LISP ------------------------------

(use-package emacs
  :custom
  (delete-pair-blink-delay 0)
  :bind
  (:map emacs-lisp-mode-map
        ("M-_" . delete-pair)
        ("M-+" . kill-backward-up-list)))

(use-package lispy
  :ensure
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

;;; ------------------------------ LATEX ------------------------------

(use-package latex
  :ensure auctex
  :config
  (put 'LaTeX-narrow-to-environment 'disabled nil)
  (TeX-source-correlate-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (preview-auto-cache-preamble t)
  (preview-default-option-list
   '("displaymath" "floats" "graphics" "textmath" "sections" "footnotes" "showlabels"))
;  (preview-gs-command "/usr/local/bin/gs")  ; compare with rungs?
;  (preview-image-type 'pnm) ; compare with png?
  ;; (preview-scale-function 1.5) ; maybe enable, or see what else you were doing?
  (reftex-derive-label-parameters
   '(15 50 t 1 "-"
	("the" "on" "in" "off" "a" "for" "by" "of" "and" "is" "to")
	t))
  )

(use-package foldout
  :ensure t)

(use-package spout
  :vc (:url "https://github.com/ultronozm/spout.el.git"
            :rev :newest)
  :after latex
  :hook
  (LaTeX-mode . spout-mode)
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

(use-package latex-extra
  :ensure
  :custom
  (latex/override-preview-map nil)
  :hook
  (LaTeX-mode . latex-extra-mode))

(use-package czm-tex-util
  :vc (:url "https://github.com/ultronozm/czm-tex-util.el.git"
            :rev :newest))

(use-package czm-tex-fold
  :vc (:url "https://github.com/ultronozm/czm-tex-fold.el.git"
            :rev :newest)  
    :after latex
    :bind
    (:map TeX-fold-mode-map
          ("C-c C-o C-s" . czm-tex-fold-fold-section)
          ("C-c C-o s" . czm-tex-fold-clearout-section))
    :config
    (czm-tex-fold-setup)
    :custom
    (czm-tex-fold-bib-file "~/doit/refs.bib"))

;; (use-package eros
;;   :ensure t
;;   :hook
;;   (emacs-lisp-mode . eros-mode))

;; (use-package highlight-defined
;;   :ensure t
;;   :custom
;;   (highlight-defined-face-use-itself t)
;;   :hook
;;   (help-mode . highlight-defined-mode)
;;   (emacs-lisp-mode . highlight-defined-mode))
;; (use-package highlight-sexp
;;   :vc (:url "https://github.com/daimrod/highlight-sexp.git")
;;   :hook
;;   (clojure-mode . highlight-sexp-mode)
;;   (emacs-lisp-mode . highlight-sexp-mode)
;;   (lisp-mode . highlight-sexp-mode))

(use-package tex-follow-avy
  :vc (:url "https://github.com/ultronozm/tex-follow-avy.el.git"
            :rev :newest)
  :after latex avy
  :bind
  (:map LaTeX-mode-map
        ("s-r" . tex-follow-avy)))

(use-package sultex
  :ensure
  :vc (:url "https://github.com/ultronozm/sultex.el.git"
            :rev :newest)
  :custom
  (sultex-master-bib-file "~/doit/refs.bib")
  (sultex-rearrange-bib-entries t)
  :bind
  (:map global-map
	("C-c 0" . sultex-bib))
  (:map LaTeX-mode-map
	("C-c 9" . sultex-label)
	("C-c 0" . sultex-bib)))

(use-package attrap
  :ensure
  :after flycheck
  :config
  (defun czm-attrap-LaTeX-fixer (msg pos end)
    (cond
     ((s-matches? (rx "Use either `` or '' as an alternative to `\"'.")msg) 
      (list (attrap-option 'fix-open-dquote
                           (delete-region pos (1+ pos))
                           (insert "``"))
            (attrap-option 'fix-close-dquote
                           (delete-region pos (1+ pos))
                           (insert "''"))))
     ((s-matches? (rx "Non-breaking space (`~') should have been used.") msg)
      (attrap-one-option 'non-breaking-space
                         (if (looking-at (rx space))
                             (delete-region pos (1+ pos))
                           (delete-region (save-excursion (skip-chars-backward "\n\t ") (point)) (point)))
                         (insert "~")))
     ((s-matches? (rx "Interword spacing (`\\ ') should perhaps be used.") msg)
      (attrap-one-option 'use-interword-spacing
                         (delete-region pos (1+ pos))
                         (insert "\\ ")))
     ((s-matches? (rx "Delete this space to maintain correct pagereferences.") msg)
      (attrap-one-option 'fix-space-pageref
                         (if (looking-back (rx bol (* space)))
                             (progn (skip-chars-backward "\n\t ")
                                    (insert "%"))
                           (delete-region (point) (save-excursion (skip-chars-forward " \t") (point)))
	                   )))
     ((s-matches? (rx "You should enclose the previous parenthesis with `{}'.") msg)
      (attrap-one-option 'enclose-with-braces
                         (insert "}")
                         (save-excursion
	                   (backward-char)
	                   (backward-sexp)
	                   (re-search-backward "[^[:alnum:]\\_\\/]")
	                   (forward-char)
	                   (insert "{")
	                   )))
     ((s-matches? (rx "You should not use punctuation in front of quotes.") msg)
      (attrap-one-option 'swap-punctuation-with-quotes
                         (progn
	                   (delete-char 2)
	                   (backward-char)
	                   (insert "''"))
                         )))
    )
  (add-to-list 'attrap-flycheck-checkers-alist '(tex-chktex . czm-attrap-LaTeX-fixer))
  (setq saved-match-data nil)
  :bind
  (:map flycheck-command-map
	("f" . attrap-flycheck)))


;;; --------------------------------- PDF ---------------------------------

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (global-auto-revert-ignore-modes '(pdf-view-mode))
  (pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
  :config
  (pdf-tools-install :no-query)
  (require 'pdf-occur))

;;; ------------------------------ ORG ------------------------------

(use-package emacs
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
  (org-babel-load-languages '((latex . t) (emacs-lisp . t)))
  (org-babel-python-command "python3")
  (org-confirm-babel-evaluate nil)
  (org-link-elisp-confirm-function nil)
  (org-enforce-todo-dependencies t)
  (org-file-apps '((auto-mode . emacs) ("\\.x?html?\\'" . default))) ; open pdfs in emacs
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

;;; ------------------------------ ERC ------------------------------

(use-package erc
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

(use-package sage-shell-mode
  :ensure
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
  :ensure
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
  :ensure
  :custom
  (mmm-global-mode 'maybe)
  :custom-face
  (mmm-default-submode-face ((t (:background "#ddffff")))))

(use-package sagemintex
  :vc (:url "https://github.com/ultronozm/sagemintex.el.git"
            :rev :newest)
  :ensure
  :after latex mmm-mode
  :demand t
  :custom
  (LaTeX-command "latex -shell-escape")
  :bind
  (:map sagemintex-mode-map
	("C-c C-c" . sagemintex-evaluate)
	("C-c C-l" . sagemintex-evaluate-latex))
  :hook
  (mmm-sage-shell:sage-mode-enter . sagemintex-enable)
  (mmm-sage-shell:sage-mode-exit . sagemintex-disable))



;;; ------------------------------ MISC ------------------------------


(find-file "~/.emacs.d/init-package.el")
(find-file-other-window "~/emacs-bak/init-bak.el")


; czm-dynexp
; sagemintex
; symtex
; arxiv/bib stuff
; publishing to ~/math

; cmake-build.  stuff in custom, too.
; (cmake-ide-build-dir . "build")

; tramp stuff, which has some stuff in custom
; gud?  what is that?

