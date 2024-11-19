;;; -*- lexical-binding: t; -*-

;;; basics
(setopt use-package-verbose t
        use-package-minimum-reported-time 0.1)
;; (setq use-package-compute-statistics t)

;; disable customization interface
(setq custom-file (locate-user-emacs-file "init-custom.el"))

(load (locate-user-emacs-file "init-bare.el"))
(load (locate-user-emacs-file "init-settings.el"))

(defun czm-dired-downloads ()
  "Open the downloads directory in Dired mode."
  (interactive)
  (dired my-downloads-folder))

(keymap-global-set "C-c d" #'czm-dired-downloads)

(defun czm-find-math-document ()
  "Find a file in the math documents folder."
  (interactive)
  (require 'project)
  (project-find-file-in nil (list my-math-folder) `(local . ,my-math-folder)))

(keymap-global-set "s-d" #'czm-find-math-document)

;; use-package keyword :repo-scan, for packages that I develop

(defalias 'use-package-normalize/:repo-scan 'use-package-normalize-predicate)

(defun use-package-handler/:repo-scan (name _keyword pred rest state)
  "Handle :repo-scan keyword in `use-package' forms.
If the predicate is true, add NAME to `repo-scan-repos'."
  (use-package-concat
   (when pred
     `((with-eval-after-load 'repo-scan
         (add-to-list 'repo-scan-repos ,(symbol-name name)))))
   (use-package-process-keywords name rest state)))

(unless (memq :repo-scan use-package-keywords)
  (setq use-package-keywords
        (use-package-list-insert :repo-scan use-package-keywords :init)))

;;; elpaca

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

(push 'notmuch elpaca-ignored-dependencies)

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))

(keymap-global-set "s-r" #'elpaca-rebuild)

(elpaca-wait)

;;; exec-path-from-shell

;; This needs to come early so that environment variables are set up
;; properly (for copilot, latex, ...)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init
  ;; With this next option, it's important that PATH is set up inside
  ;; .zshenv rather than .zshrc.
  (setq exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize))

(elpaca-wait)

;;; lots of packages

(defun fill-previous-paragraph ()
  "Fill the previous paragraph."
  (interactive)
  (save-excursion
    (previous-line)
    (fill-paragraph)))

(bind-keys
 :repeat-map paragraph-repeat-map
 ("]" . forward-paragraph)
 ("}" . forward-paragraph)
 ("[" . backward-paragraph)
 ("{" . backward-paragraph)
 :exit
 ("C-/" . undo)
 :continue-only
 ("M-h" . mark-paragraph)
 ("h" . mark-paragraph)
 ("k" . kill-paragraph)
 ("w" . kill-region)
 ("M-w" . kill-ring-save)
 ("y" . yank)
 ("t" . transpose-paragraphs)
 ("q" . fill-previous-paragraph)
 ("C-l" . recenter-top-bottom))

(use-package aggressive-indent
  :defer t
  :diminish
  :hook
  ((emacs-lisp-mode LaTeX-mode rust-mode c++-mode) . aggressive-indent-mode))

(use-package diminish
  :demand t
  :config
  (diminish 'abbrev-mode "Ab")
  (dolist (mode '(visual-line-mode outline-minor-mode buffer-face-mode
                                   eldoc-mode reftex-mode whitespace-mode))
    (diminish mode)))

;; Remove "%n" from mode-line-modes -- I know when I'm narrowing.
(setq mode-line-modes (delete "%n" mode-line-modes))

(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "E")))
(add-hook 'lisp-interaction-mode-hook (lambda () (setq mode-name "LI")))

(with-eval-after-load 'tex-mode
  (add-hook 'LaTeX-mode-hook
            (lambda () (setq TeX-base-mode-name "L"))))

;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2024-06/msg01858.html
(use-package org
  :ensure nil
  :demand)

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

(defun my/org-schedule-and-refile ()
  "Schedule the current heading and refile it to the Scheduled node."
  (interactive)
  (call-interactively #'org-schedule)
  (let* ((org-refile-target-verify-function
          (lambda () (string= (nth 4 (org-heading-components)) "Scheduled")))
         (rfloc (car (org-refile-get-targets))))
    (when rfloc
      (org-refile nil nil rfloc))))

(use-package org
  :ensure nil
  :hook
  (org-mode . visual-line-mode)
  (org-mode . (lambda () (setq fill-column 999999)))
  (org-mode . abbrev-mode)
  :custom
  (org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((agenda "")
       (todo "TODO")
       (tags "CLOSED>=\"<today>\""
             ((org-agenda-overriding-header "\nCompleted today\n")))))))
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
  (org-refile-targets nil)
  (org-refile-use-outline-path nil)
  ;; should add to list:  (org-speed-commands '(("B" . org-tree-to-indirect-buffer)))
  (org-src-preserve-indentation t)
  (org-agenda-skip-scheduled-if-done t)
  (org-tags-column -70)
  (org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d)" "CANCELED(c)")))
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
  (org-src-window-setup 'current-window)
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
  (:repeat-map
   org-paragraph-repeat-map
   ("]" . org-forward-paragraph)
   ("}" . org-forward-paragraph)
   ("[" . org-backward-paragraph)
   ("{" . org-backward-paragraph)
   :continue-only
   ("w" . kill-region)
   ("M-w" . kill-ring-save)
   ("y" . yank)
   ("C-/" . undo)
   ("t" . transpose-paragraphs)
   ("q" . fill-previous-paragraph))
  :config
  (require 'ob-shell)
  (dolist (item '(("m" . org-babel-mark-block)
                  ("\C-m" . org-babel-mark-block)))
    (add-to-list 'org-babel-key-bindings item))
  (pcase-dolist (`(,key . ,def) org-babel-key-bindings)
    (define-key org-babel-map key def))
  (add-to-list 'org-speed-commands '("S" . my/org-schedule-and-refile) t))

(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(defun avy-action-easy-kill (pt)
  (unless (require 'easy-kill nil t)
    (user-error "Easy Kill not found, please install."))
  (cl-letf* ((bounds (if (use-region-p)
                         (prog1 (cons (region-beginning) (region-end))
                           (deactivate-mark))
                       (bounds-of-thing-at-point 'sexp)))
             (transpose-map
              (define-keymap
                "M-t" (lambda () (interactive "*")
                        (pcase-let ((`(,beg . ,end) (easy-kill--bounds)))
                          (transpose-regions (car bounds) (cdr bounds) beg end
                                             'leave-markers)))))
             ((symbol-function 'easy-kill-activate-keymap)
              (lambda ()
                (let ((map (easy-kill-map)))
                  (set-transient-map
                   (make-composed-keymap transpose-map map)
                   (lambda ()
                     ;; Prevent any error from activating the keymap forever.
                     (condition-case err
                         (or (and (not (easy-kill-exit-p this-command))
                                  (or (eq this-command
                                          (lookup-key map (this-single-command-keys)))
                                      (let ((cmd (key-binding
                                                  (this-single-command-keys) nil t)))
                                        (command-remapping cmd nil (list map)))))
                             (ignore
                              (easy-kill-destroy-candidate)
                              (unless (or (easy-kill-get mark) (easy-kill-exit-p this-command))
                                (easy-kill-save-candidate))))
                       (error (message "%s:%s" this-command (error-message-string err))
                              nil)))
                   (lambda ()
                     (let ((dat (ring-ref avy-ring 0)))
                       (select-frame-set-input-focus
                        (window-frame (cdr dat)))
                       (select-window (cdr dat))
                       (goto-char (car dat)))))))))
    (goto-char pt)
    (easy-kill)))

(use-package avy
  :custom
  (avy-single-candidate-jump nil)
  :config
  (setq
   avy-dispatch-alist
   '((?x . avy-action-kill-move)
     (?X . avy-action-kill-stay)
     (?t . avy-action-teleport)
     (?T . avy-action-teleport-whole-line)
     (?m . avy-action-mark)
     (?n . avy-action-copy)
     (?y . avy-action-yank)
     (?Y . avy-action-yank-whole-line)
     (?z . avy-action-zap-to-char)
     (?  . avy-action-embark)
     (?w . avy-action-easy-kill)
     (?K . avy-action-kill-whole-line)))
  (with-eval-after-load 'org
    (keymap-set org-mode-map "C-'" nil))

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun avy-action-kill-whole-line (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (kill-whole-line)
          (avy-resume)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-exchange (pt)
    "Exchange sexp at PT with the one at point."
    (set-mark pt)
    (transpose-sexps 0))

  :bind
  (("C-'" . avy-goto-char-timer)
   ("C-;" . avy-goto-line)
   ("C-c g" . avy-goto-line)
   ("M-s M-p" . avy-goto-line-above)
   ("M-s M-n" . avy-goto-line-below)
   ("M-s C-w" . avy-kill-region)
   ("M-s M-w" . avy-kill-ring-save-region))
  (:map
   isearch-mode-map
   ("M-j" . avy-isearch)))

(use-package czm-misc
  :repo-scan
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
  :config (vertico-mode))

(use-package marginalia
  :demand
  :config (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

(use-package consult
  :bind (("C-c M-x" . consult-mode-command)
         ("C-c i" . consult-info)
         ([remap repeat-complex-command] . consult-complex-command)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ("s-t" . consult-register-load)
         ("s-T" . consult-register-store)
         ("C-s-t" . consult-register)
         ([remap yank-pop] . consult-yank-pop)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ([remap goto-line] . consult-goto-line)
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
         ([remap isearch-edit-string] . consult-isearch-history)
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

(defun czm-search-log ()
  "Search your log files with `rg'."
  (interactive)
  (let ((log-files `(,my-log-file ,my-old-log-file ,my-todo-file)))
    (consult--grep "Ripgrep" #'consult--ripgrep-make-builder log-files nil)))

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

;; https://www.jamescherti.com/emacs-customize-ellipsis-outline-minor-mode/
(defun my-outline-set-global-ellipsis (ellipsis)
  "Apply the ellipsis ELLIPSIS to outline mode globally."
  (let* ((face-offset (* (face-id 'shadow) (ash 1 22)))
         (value (vconcat (mapcar (lambda (c) (+ face-offset c)) ellipsis))))
    (set-display-table-slot standard-display-table 'selective-display value)))

(my-outline-set-global-ellipsis " ▼ ")

(use-package outline-skip
  :after latex
  :ensure (:host github :repo "ultronozm/outline-skip.el"
                 :depth nil)
  :hook (LaTeX-mode . outline-skip-mode))

(use-package smerge-mode
  :ensure nil
  :defer
  :config
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map 'smerge-basic-map)))
   smerge-basic-map))

(use-package perfect-margin
  :defer t
  :diminish
  :bind ("H-m" . perfect-margin-mode))

(use-package easy-kill
  :bind ([remap kill-ring-save] . easy-kill))

(use-package wgrep ;; use C-c C-p in embark export following ripgrep
  :defer t)

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(use-package ace-link ; activate using 'o' in info/help/(...)
  :defer 2
  :config
  (ace-link-setup-default))

(use-package eldoc-box
  :bind
  (:map global-map ("C-c e" . eldoc-box-help-at-point)))

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

(use-package pos-tip
  :defer t)

(use-package go-translate
  :defer t
  :config
  (setq gt-langs '(da en))
  (setq gt-default-translator (gt-translator :engines (gt-google-engine))))

(use-package rust-mode
  :defer t
  :hook
  (rust-mode . eglot-ensure))

(use-package xr
  :defer t)

(use-package copilot
  :ensure (:host github
                 :repo "zerolfx/copilot.el"
                 ;; :repo "ultronozm/copilot.el"
                 :files ("*.el" "dist")
                 :depth nil)
  :diminish " Co"
  :hook
  ((prog-mode LaTeX-mode git-commit-setup) . copilot-mode)
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
  :defer t
  :ensure (:host github :repo "ahyatt/llm"
                 :depth nil)
  ;; :init
  ;; (require 'llm-openai)
  ;; (require 'llm-gemini)
  ;; (require 'llm-ollama)
  :custom
  (llm-warn-on-nonfree nil)
  (llm-log t)
  :config
  (add-to-list 'warning-suppress-types '(llm)))

(use-package ai-org-chat
  :repo-scan
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
  :custom
  (ai-org-chat-user-name my-first-name)
  (ai-org-chat-dir my-scratch-gpt-dir)
  (ai-org-chat-context-style nil)
  :config
  (require 'exec-path-from-shell)
  (ai-org-chat-select-model "sonnet 3.5"))

(defun ai-org-chat-suggest-filename-function ()
  "Blah"
  (make-llm-function-call
   :function (lambda (suggested-name)
               (cons 'suggested-name suggested-name))
   :name "suggest_filename"
   :description "Suggest a better filename for the current file."
   :args (list (make-llm-function-arg
                :name "suggested_name"
                :description "The suggested new filename."
                :type 'string
                :required t))))

(defun ai-org-chat-suggest-better-filename ()
  "Ask LLM for a better filename and prompt user to rename the current file."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Current buffer is not visiting a file"))

  (let* ((current-name (file-name-nondirectory (buffer-file-name)))
         (file-content (buffer-substring-no-properties (point-min) (point-max)))
         (buffer (current-buffer))
         (prompt (format "Given the following file content and current filename '%s', suggest a better, more descriptive filename.  Make sure to keep the file extension the same.  Also, if the filename contains a timestamp at or near the beginning, then preserve that -- follow that timestamp with double dashes followed by a name separated by single dashes.  Use the suggest_filename function to provide your suggestion.\n\nFile content:\n%s"
                         current-name
                         (if (> (length file-content) 1000)
                             (concat (substring file-content 0 1000) "...")
                           file-content)))
         (final-cb
          (lambda (response)
            (with-current-buffer buffer
              (if (and (listp response)
                       (equal (caar response) "suggest_filename"))
                  (let* ((suggested-name (cddar response))
                         (current-dir (file-name-directory (buffer-file-name)))
                         (new-path (read-file-name "Rename file to: "
                                                   current-dir
                                                   nil
                                                   nil
                                                   suggested-name)))
                    (when (y-or-n-p (format "Rename '%s' to '%s'? "
                                            (buffer-file-name)
                                            new-path))
                      (require 'dired-aux) ; Ensure dired-rename-file is available
                      (dired-rename-file (buffer-file-name) new-path 1)
                      (message "File renamed to '%s'" (file-name-nondirectory new-path))))
                (user-error "Failed to get a valid filename suggestion from LLM")))))
         (error-cb
          (lambda (err msg)
            (message "Error: %s - %s" err msg))))

    (llm-chat-async ai-org-chat-provider
                    (llm-make-chat-prompt
                     prompt
                     :functions (list (ai-org-chat-suggest-filename-function)))
                    final-cb
                    error-cb)))

(use-package elpy
  :defer t)

(defun czm-lispy-comment-maybe ()
  "Comment the list at point, or self-insert."
  (interactive)
  (if (looking-at "(")
      (lispy-comment)
    (call-interactively #'self-insert-command)))

(use-package lispy
  :bind
  (:map
   emacs-lisp-mode-map
   (";" . czm-lispy-comment-maybe)
   ("M-1" . lispy-describe-inline)
   ("M-2" . lispy-arglist-inline))
  (:repeat-map
   structural-edit-map
   ("n" . forward-list)
   ("p" . backward-list)
   ("u" . backward-up-list)
   ("M-u" . up-list)
   ("g" . down-list)
   :continue-only
   ("M-g" . backward-down-list)
   ("f" . forward-sexp)
   ("b" . backward-sexp)
   ("a" . beginning-of-defun)
   ("e" . end-of-defun)
   ("k" . kill-sexp)
   ("x" . eval-last-sexp)
   ("m" . lispy-multiline)
   ("j" . lispy-split)
   ("+" . lispy-join)
   (">" . lispy-slurp-or-barf-right)
   ("<" . lispy-slurp-or-barf-left)
   ("C-/" . undo)
   ("/" . lispy-splice)
   (";" . lispy-comment)
   ("t" . transpose-sexps)
   ("w" . kill-region)
   ("M-w" . kill-ring-save)
   ("y" . yank)
   ("c" . lispy-clone)
   ("C-M-SPC" . mark-sexp)
   ("RET" . newline-and-indent)
   ("i" . lispy-tab)
   ("<up>" . lispy-move-up)
   ("<down>" . lispy-move-down)))

(defun czm-edebug-eval-hook ()
  (dolist (cmd '(lispy-mode copilot-mode aggressive-indent-mode))
    (when (fboundp cmd)
      (funcall cmd 0))))

(add-hook 'edebug-eval-mode-hook #'czm-edebug-eval-hook)

(defun isearch-forward-enclosing-defun ()
  "Start an incremental search for the name of the enclosing defun."
  (interactive)
  (end-of-defun)
  (beginning-of-defun)
  (down-list)
  (forward-sexp 2)
  (isearch-forward-symbol-at-point))

(keymap-global-set "M-s q" #'isearch-forward-enclosing-defun)

(use-package symbol-overlay
  :bind
  (("M-s ," . symbol-overlay-put)
   ("M-s n" . symbol-overlay-switch-forward)
   ("M-s p" . symbol-overlay-switch-backward)
   ;; ("M-s m" . symbol-overlay-mode)
   ;; ("M-s n" . symbol-overlay-remove-all)
   ))

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

(use-package flycheck
  :defer t
  :bind
  (:repeat-map
   flycheck-repeat-map
   ("C-c" . flycheck-compile)
   ("C-w" . flycheck-copy-errors-as-kill)
   ("?" . flycheck-describe-checker)
   ("C" . flycheck-clear)
   ("H" . display-local-help)
   ("V" . flycheck-version)
   ("c" . flycheck-buffer)
   ("e" . flycheck-explain-error-at-point)
   ("f" . attrap-flycheck)
   ("h" . flycheck-display-error-at-point)
   ("i" . flycheck-manual)
   ("l" . flycheck-list-errors)
   ("n" . flycheck-next-error)
   ("p" . flycheck-previous-error)
   ("s" . flycheck-select-checker)
   ("v" . flycheck-verify-setup)
   ("x" . flycheck-disable-checker))
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-package
  :defer t
  :hook
  (emacs-lisp-mode . flycheck-package-setup))

(use-package flymake
  :ensure nil
  :custom
  (flymake-mode-line-lighter "F")
  (flymake-show-diagnostics-at-end-of-line t)
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error))
  (:repeat-map flymake-repeat-map
               ("n" . flymake-goto-next-error)
               ("p" . flymake-goto-prev-error)
               ("f" . attrap-flymake)
               ("M-n" . flymake-goto-next-error)
               ("M-p" . flymake-goto-prev-error)
               ("l" . flymake-show-diagnostics-buffer))
  :config
  (with-eval-after-load 'preview
    (dolist (cmd '(flymake-goto-next-error flymake-goto-prev-error))
      (add-to-list 'preview-auto-reveal-commands cmd)))
  (with-eval-after-load 'tex-fold
    (dolist (cmd '(flymake-goto-next-error flymake-goto-prev-error))
      (add-to-list 'TeX-fold-auto-reveal-commands cmd))))

(use-package attrap
  :defer t
  :after flycheck
  :config
  (setq saved-match-data nil)
  (define-key flycheck-command-map "f" 'attrap-flycheck))

(use-package treesit-auto
  :defer t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package eglot
  :bind
  (:map eglot-mode-map
        ("C-c C-q" . eglot-code-action-quickfix)
        ("C-c C-a" . eglot-code-actions)))

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

(use-package consult-abbrev
  :repo-scan
  :defer t
  :ensure (:host github :repo "ultronozm/consult-abbrev.el" :depth nil))

(defun foldout-exit-fold-without-hiding ()
  (interactive)
  (foldout-exit-fold -1))

(use-package czm-spell
  :repo-scan
  :ensure (:host github :repo "ultronozm/czm-spell.el" :depth nil)
  ;; :after latex
  :bind ("s-;" . czm-spell-then-abbrev))

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

(defun my/pdf-annot-setup (_a)
  (LaTeX-mode)
  (setq TeX-master my-preview-master)
  (preview-auto-mode))

(setq pdf-annot-edit-contents-setup-function #'my/pdf-annot-setup)

(use-package doc-dual-view
  :repo-scan
  :defer t
  :ensure (:host github :repo "ultronozm/doc-dual-view.el" :depth nil))

;;; erc

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

;;; c/c++

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

(use-package cc-mode
  :ensure nil
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
  :ensure (:host github :repo "ultronozm/cmake-build.el" :depth nil)
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
  :repo-scan
  :ensure (:host github :repo "ultronozm/czm-cpp.el" :files ("*.el" "template") :depth nil)
  :defer t
  :custom
  (czm-cpp-scratch-directory my-scratch-cpp-dir))

(add-to-list 'auto-mode-alist '("\\.ixx\\'" . c++-mode))

(use-package c-ts-mode
  :ensure nil ;; emacs built-in
  :defer t
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
  :init
  ;; commenting out c-mode remap because it seems a bit premature
  ;; (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  ;; (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
  :config
  (require 'treesit-auto)
  (setq c-ts-mode-indent-offset 2)
  (setq c-ts-mode-indent-style #'my--c-ts-indent-style))

;;; calc

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

;;; git

(use-package transient
  :ensure t
  :demand t)

(use-package magit
  :defer t
  :hook
  (magit-status-mode . visual-line-mode)
  :bind
  (:repeat-map
   magit-smerge-repeat-map
   ("RET" . magit-smerge-keep-current)
   ("b" . magit-smerge-keep-base)
   ("l" . magit-smerge-keep-lower)
   ("u" . magit-smerge-keep-upper)))

(defun czm-file-is-tex-or-bib (file)
  "Return t if FILE is a .tex or .bib file."
  (or (string-suffix-p ".tex" file)
      (string-suffix-p ".bib" file)))

(use-package publish
  :repo-scan
  :ensure (:host github :repo "ultronozm/publish.el" :depth nil)
  :defer t
  :custom
  (publish-repo-root "~/math")
  (publish-disallowed-unstaged-file-predicate #'czm-file-is-tex-or-bib))

(use-package magit-fill-column
  :repo-scan
  :ensure (:host github :repo "ultronozm/magit-fill-column.el" :depth nil)
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

(use-package repo-scan
  :repo-scan
  :ensure (:host github :repo "ultronozm/repo-scan.el" :depth nil)
  :defer t)

;;; latex

(use-package tex-mode
  :ensure nil
  :defer t
  :config
  (mapc
   (lambda (sym) (add-to-list 'tex--prettify-symbols-alist sym))
   '(("``" . ?“)
     ("''" . ?”)
     ("\\begin{equation*}" . ?↴)
     ("\\begin{equation}" . ?↴)
     ("\\end{equation*}" . ?↲)
     ("\\end{equation}" . ?↲)
     ("\\begin{align*}" . ?⌈)
     ("\\begin{align}" . ?⌈)
     ("\\end{align*}" . ?⌋)
     ("\\end{align}" . ?⌋)
     ("\\begin{multline*}" . ?⎧)
     ("\\begin{multline}" . ?⎧)
     ("\\end{multline*}" . ?⎭)
     ("\\end{multline}" . ?⎭)
     ("\\S" . ?§)
     ("\\Bbb{A}" . ?𝔸)
     ("\\Bbb{B}" . ?𝔹)
     ("\\Bbb{C}" . ?ℂ)
     ("\\Bbb{D}" . ?𝔻)
     ("\\Bbb{E}" . ?𝔼)
     ("\\Bbb{F}" . ?𝔽)
     ("\\Bbb{G}" . ?𝔾)
     ("\\Bbb{H}" . ?ℍ)
     ("\\Bbb{I}" . ?𝕀)
     ("\\Bbb{J}" . ?𝕁)
     ("\\Bbb{K}" . ?𝕂)
     ("\\Bbb{L}" . ?𝕃)
     ("\\Bbb{M}" . ?𝕄)
     ("\\Bbb{N}" . ?ℕ)
     ("\\Bbb{O}" . ?𝕆)
     ("\\Bbb{P}" . ?ℙ)
     ("\\Bbb{Q}" . ?ℚ)
     ("\\Bbb{R}" . ?ℝ)
     ("\\Bbb{S}" . ?𝕊)
     ("\\Bbb{T}" . ?𝕋)
     ("\\Bbb{U}" . ?𝕌)
     ("\\Bbb{V}" . ?𝕍)
     ("\\Bbb{W}" . ?𝕎)
     ("\\Bbb{X}" . ?𝕏)
     ("\\Bbb{Y}" . ?𝕐)
     ("\\Bbb{Z}" . ?ℤ)
     ("\\mathbb{A}" . ?𝔸)
     ("\\mathbb{B}" . ?𝔹)
     ("\\mathbb{C}" . ?ℂ)
     ("\\mathbb{D}" . ?𝔻)
     ("\\mathbb{E}" . ?𝔼)
     ("\\mathbb{F}" . ?𝔽)
     ("\\mathbb{G}" . ?𝔾)
     ("\\mathbb{H}" . ?ℍ)
     ("\\mathbb{I}" . ?𝕀)
     ("\\mathbb{J}" . ?𝕁)
     ("\\mathbb{K}" . ?𝕂)
     ("\\mathbb{L}" . ?𝕃)
     ("\\mathbb{M}" . ?𝕄)
     ("\\mathbb{N}" . ?ℕ)
     ("\\mathbb{O}" . ?𝕆)
     ("\\mathbb{P}" . ?ℙ)
     ("\\mathbb{Q}" . ?ℚ)
     ("\\mathbb{R}" . ?ℝ)
     ("\\mathbb{S}" . ?𝕊)
     ("\\mathbb{T}" . ?𝕋)
     ("\\mathbb{U}" . ?𝕌)
     ("\\mathbb{V}" . ?𝕍)
     ("\\mathbb{W}" . ?𝕎)
     ("\\mathbb{X}" . ?𝕏)
     ("\\mathbb{Y}" . ?𝕐)
     ("\\mathbb{Z}" . ?ℤ)
     ("\\eps" . ?ε)
     ("\\frac{1}{2}" . "½") ("\\tfrac{1}{2}" . "½")
     ("\\frac{1}{3}" . "⅓") ("\\tfrac{1}{3}" . "⅓")
     ("\\frac{2}{3}" . "⅔") ("\\tfrac{2}{3}" . "⅔")
     ("\\frac{1}{4}" . "¼") ("\\tfrac{1}{4}" . "¼")
     ("\\frac{3}{4}" . "¾") ("\\tfrac{3}{4}" . "¾")
     ("\\frac{1}{5}" . "⅕") ("\\tfrac{1}{5}" . "⅕")
     ("\\frac{2}{5}" . "⅖") ("\\tfrac{2}{5}" . "⅖")
     ("\\frac{3}{5}" . "⅗") ("\\tfrac{3}{5}" . "⅗")
     ("\\frac{4}{5}" . "⅘") ("\\tfrac{4}{5}" . "⅘")
     ("\\frac{1}{6}" . "⅙") ("\\tfrac{1}{6}" . "⅙")
     ("\\frac{5}{6}" . "⅚") ("\\tfrac{5}{6}" . "⅚")
     ("\\frac{1}{7}" . "⅐") ("\\tfrac{1}{7}" . "⅐")
     ("\\frac{1}{8}" . "⅛") ("\\tfrac{1}{8}" . "⅛")
     ("\\frac{3}{8}" . "⅜") ("\\tfrac{3}{8}" . "⅜")
     ("\\frac{5}{8}" . "⅝") ("\\tfrac{5}{8}" . "⅝")
     ("\\frac{7}{8}" . "⅞") ("\\tfrac{7}{8}" . "⅞")
     ("\\frac{1}{9}" . "⅑") ("\\tfrac{1}{9}" . "⅑")
     ("\\frac{1}{10}" . "⅒") ("\\tfrac{1}{10}" . "⅒")
     )))

(defun my-LaTeX-mode-setup ()
  (turn-on-reftex)
  (apply #'LaTeX-add-environments
         (mapcar (lambda (env) (list env 'LaTeX-env-label))
                 '("lemma" "exercise" "example" "proposition"
                   "corollary" "remark" "definition" "theorem"
                   "proof")))
  (setq buffer-face-mode-face
        '(:height 216 :width normal :family "Andale Mono"))
  (buffer-face-mode)
  (outline-minor-mode)
  (abbrev-mode)
  (visual-line-mode)
  (setq fill-column 999999)
  (czm-setup-and-activate-tex-fold))

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
  ;; :demand                             ; otherwise, madness ensues.
  :config
  (setq TeX-data-directory (expand-file-name "elpaca/builds/auctex" user-emacs-directory))
  (setq TeX-lisp-directory TeX-data-directory)
  (add-to-list 'TeX-file-extensions "tex\\.~[0-9a-f]+~")
  (with-eval-after-load 'org-src
    (push '("latex" . LaTeX) org-src-lang-modes))
  (put 'LaTeX-narrow-to-environment 'disabled nil)
  (TeX-source-correlate-mode)
  (let ((cmds '(other-window ace-window consult-register-load)))
    (with-eval-after-load 'preview
      (dolist (cmd cmds)
        (add-to-list 'preview-auto-reveal-commands cmd)))
    (with-eval-after-load 'tex-fold
      (dolist (cmd cmds)
        (add-to-list 'TeX-fold-auto-reveal-commands cmd))))
  :hook
  (LaTeX-mode . my-LaTeX-mode-setup)
  (TeX-mode . prettify-symbols-mode)
  (prog-mode . (lambda () (setq-local TeX-master my-preview-master)))
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
   (mapcar (lambda (env) (list env 1)) '("\\\\bibliography\\b" "\\\\printindex\\b" "\\\\begin{thebibliography")))
  ;; Don't ask for section labels.  Do fold after inserting a section.
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
  ;; (TeX-fold-quotes-on-insert t)
  (TeX-fold-bib-files (list my-master-bib-file))
  (TeX-ignore-warnings "Package hyperref Warning: Token not allowed in a PDF string")
  (TeX-insert-macro-default-style 'mandatory-args-only)
  ;; (TeX-suppress-ignored-warnings t)
  (LaTeX-insert-into-comments nil)
  :custom-face
  (preview-face ((t (:background unspecified)))))

(use-package preview-tailor
  :repo-scan
  :ensure (:host github :repo "ultronozm/preview-tailor.el" :depth nil)
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
  :repo-scan
  :ensure (:host github :repo "ultronozm/czm-tex-util.el" :depth nil)
  :after latex)

(defun my/backward-word-fold-macro ()
  (interactive)
  (when TeX-fold-mode
    (save-excursion
      (backward-word)
      (TeX-fold-item 'macro))))

(defun my-yank-after-advice (&rest _)
  "Fold any yanked ref or eqref."
  (and (eq major-mode 'LaTeX-mode)
       TeX-fold-mode
       (string-match "\\\\\\(ref\\|eqref\\){\\([^}]+\\)}"
                     (current-kill 0))
       (my/backward-word-fold-macro)))

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
  (setq TeX-fold-macro-spec-list
        (seq-remove (lambda (item)
                      (and (numberp (car item))
                           (= (car item) 1)))
                    TeX-fold-macro-spec-list))
  (dolist (item '(("[{2}]||[href]" ("href"))
                  ("🌱" ("documentclass"))
                  ("🌌" ("input"))
                  ("📚" ("bibliography"))
                  ("📖" ("bibliographystyle"))
                  ("✅" ("leanok"))
                  ("❡❡ {1}" ("part" "part*"))
                  ("❡ {1}" ("chapter" "chapter*"))
                  ("§ {1}" ("section" "section*"))
                  ("§§ {1}" ("subsection" "subsection*"))
                  ("§§§ {1}" ("subsubsection" "subsubsection*"))
                  ("¶ {1}" ("paragraph" "paragraph*"))
                  ("¶¶ {1}" ("subparagraph" "subparagraph*"))
                  (1 ("emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt"
                      "textbf" "textsc" "textup"))))
    (add-to-list 'TeX-fold-macro-spec-list item))
  (dolist (item '((("🌅" . "🌇") ("document"))
                  (("⚡" . "⚡") ("minted" "minted*"))
                  (("♣" . "♣") ("results" "results*"))
                  ((TeX-fold-format-theorem-environment . "◼")
                   ("idea" "solution"))))
    (add-to-list 'TeX-fold-begin-end-spec-list item))
  ;; (dolist (item (list #'TeX-fold-quotes #'TeX-fold-dashes))
  ;;   (add-to-list 'TeX-fold-region-functions item))
  (setq TeX-fold-region-functions '(TeX-fold-verbs))
  (advice-add 'LaTeX-insert-item :after #'my/backward-word-fold-macro)
  (advice-add 'yank :after #'my-yank-after-advice)
  (TeX-fold-mode 1)
  (auctex-label-numbers-mode 1)
  (remove-hook 'LaTeX-mode-hook #'czm-setup-and-activate-tex-fold)
  (add-hook 'LaTeX-mode-hook #'TeX-fold-mode))

(use-package czm-tex-jump
  :repo-scan
  :ensure (:host github :repo "https://github.com/ultronozm/czm-tex-jump.el.git" :depth nil)
  ;; :after avy
  :after latex
  :bind
  (:map LaTeX-mode-map
        ("s-r" . czm-tex-jump))
  :hook (LaTeX-mode . czm-tex-jump-setup))

(use-package czm-tex-ref
  :repo-scan
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
   ((s-matches? (rx "Use either `` or '' as an alternative to `\"'.") msg)
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

(with-eval-after-load 'latex
  (require 'latex-flymake)
  (with-eval-after-load 'attrap
    (setcdr (assoc 'LaTeX-flymake attrap-flymake-backends-alist)
            #'czm-attrap-LaTeX-fixer-flymake)))

(use-package dynexp
  :repo-scan
  :ensure (:host github :repo "ultronozm/dynexp.el" :depth nil)
  :after latex
  :hook (LaTeX-mode . dynexp-latex-setup)
  :bind (:map LaTeX-mode-map
              ("SPC" . dynexp-space)
              ("TAB" . dynexp-next)))

(defun czm-tex-font-fold-advice (&rest _)
  "Advice to fold macros after `TeX-font' is called."
  (when TeX-fold-mode
    (save-excursion
      ;; Move to the beginning of the newly inserted macro
      (when (looking-back "[{}]" (- (point) 1))
        (backward-sexp))
      (let ((macro-start (point)))
        ;; Move to the end of the macro
        (forward-sexp)
        ;; Ensure proper fontification
        (font-lock-ensure macro-start (point))
        ;; Move back and fold
        (goto-char macro-start)
        (TeX-fold-macro)))))

(advice-add 'TeX-font :after #'czm-tex-font-fold-advice)

(use-package czm-tex-edit
  :repo-scan
  :ensure (:host github :repo "ultronozm/czm-tex-edit.el" :depth nil)
  :after latex dynexp
  ;; :demand ; should come after latex and dynexp
  :bind
  (:map LaTeX-mode-map
        ;; ("C-c t i" . czm-tex-edit-emphasize)
        ;; ("C-c t a" . czm-tex-edit-alertify)
        ;; ("C-c t b" . czm-tex-edit-bold)
        ;; ("C-c t l" . czm-tex-edit-underline)
        ;; ("C-c t u" . czm-tex-edit-unemphasize)
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
  :repo-scan
  :ensure (:host github :repo "ultronozm/auctex-cont-latexmk.el" :depth nil)
  :after latex
  :bind (:map LaTeX-mode-map ("C-c k" . auctex-cont-latexmk-toggle))
  :custom
  (auctex-cont-latexmk-command
   '("latexmk -pvc -shell-escape -pdf -view=none -e "
     ("$pdflatex=q/pdflatex %O -synctex=1 -file-line-error -interaction=nonstopmode %S/"))))

(use-package preview-auto
  :repo-scan
  :ensure (:host github :repo "ultronozm/preview-auto.el" :depth nil)
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
  :repo-scan
  :ensure (:host github :repo "ultronozm/auctex-label-numbers.el" :depth nil)
  :after latex)

(use-package library
  :repo-scan
  :after latex czm-tex-util
  :defer t
  :ensure (:host github :repo "ultronozm/library.el" :depth nil)
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
  :repo-scan
  :ensure (:host github :repo "ultronozm/tex-parens.el" :depth nil)
  :after latex
  :bind
  (:map
   LaTeX-mode-map
   ("M-i" . tex-parens-mark-inner)
   ("s-j" . tex-parens-avy-jump-to-math)
   ("C-M-j" . czm-tex-jump-back-with-breadcrumb)
   ("s-c" . tex-parens-avy-copy-math)
   ("s-e" . tex-parens-end-of-list)
   ("s-a" . tex-parens-beginning-of-list)
   ("s-E" . tex-parens-kill-to-end-of-list)
   ("s-A" . tex-parens-kill-to-beginning-of-list))
  (:repeat-map
   tex-parens-structural-edit-repeat-map
   ("n" . tex-parens-forward-list)
   ("p" . tex-parens-backward-list)
   ("u" . tex-parens-backward-up-list)
   ("n" . tex-parens-forward-list)
   ("p" . tex-parens-backward-list)
   ("u" . tex-parens-backward-up-list)
   ("M-u" . tex-parens-up-list)
   ("g" . tex-parens-down-list)
   ("M-g" . tex-parens-backward-down-list)
   :continue-only
   ("f" . tex-parens-forward-sexp)
   ("b" . tex-parens-backward-sexp)
   ("a" . beginning-of-defun)
   ("e" . end-of-defun)
   ("k" . kill-sexp)
   (">" . tex-parens-burp-right)
   ("<" . tex-parens-burp-left)
   ("C-/" . undo)
   ("r" . tex-parens-raise-sexp)
   ("/" . tex-parens-delete-pair)
   ("t" . transpose-sexps)
   ("w" . kill-region)
   ("M-w" . kill-ring-save)
   ("y" . yank)
   ("c" . lispy-clone)
   ("RET" . TeX-newline))
  :hook
  (LaTeX-mode . tex-parens-mode)
  :config
  (add-to-list 'preview-auto-reveal-commands #'czm-tex-jump-back-with-breadcrumb)
  (repeat-mode 1))

(use-package tex-item
  :repo-scan
  :ensure (:host github :repo "ultronozm/tex-item.el" :depth nil)
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

;;; sage

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
  :custom (mmm-global-mode nil)
  :config
  (face-spec-set 'mmm-default-submode-face
                 '((((background light)) (:background "#ddffff"))
                   (((background dark)) (:background "#004444")))
                 'face-defface-spec))

(use-package czm-tex-mint
  :repo-scan
  :ensure (:host github :repo "ultronozm/czm-tex-mint.el" :depth nil)
  :after latex mmm-mode
  ;; :demand t
  :custom (LaTeX-command "latex -shell-escape")
  :config (czm-tex-mint-initialize)
  :bind
  (:map czm-tex-mint-mode-map
        ("C-c C-c" . czm-tex-mint-evaluate)
        ("C-c C-l" . czm-tex-mint-evaluate-latex))
  :hook
  (mmm-sage-shell:sage-mode-enter . czm-tex-mint-enable)
  (mmm-sage-shell:sage-mode-exit . czm-tex-mint-disable))

(use-package symtex
  :repo-scan
  :ensure (:host github :repo "ultronozm/symtex.el" :depth nil)
  :after latex
  :bind
  (:map global-map ("C-c V" . symtex-process))
  (:map LaTeX-mode-map ("C-c v" . symtex-dwim)))

;;; lean

(defun czm-set-lean4-local-variables ()
  (setq preview-tailor-local-multiplier 0.7)
  (setq TeX-master my-preview-master))

(use-package lean4-mode
  :repo-scan
  :ensure (:host github :repo "ultronozm/lean4-mode" :files ("*.el" "data"))
  :diminish
  :hook
  (lean4-mode . czm-set-lean4-local-variables)
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
  :repo-scan
  :ensure (:host github :repo "ultronozm/czm-lean4.el" :depth nil)
  :after lean4-mode preview-auto flymake-overlays
  :hook
  (lean4-mode . czm-lean4-mode-hook)
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
              ("M-[" . czm-lean4-cycle-delimiter-backward))
  :bind (:map lean4-mode-map
              ("s-f" . czm-lean4-preview-fold-block))
  :custom
  (czm-lean4-info-window-height-fraction 0.4)
  (czm-lean4-info-window-width-fraction 0.47)
  (flymake-overlays-fontify-text-function #'czm-lean4-maybe-colorize)
  :config
  (advice-add 'lean4-info-buffer-redisplay :around #'czm-lean4-info-buffer-redisplay)
  (map-keymap
   (lambda (key cmd)
     (define-key lean4-mode-map (vector key) cmd))
   copilot-completion-map))

(use-package flymake-overlays
  :repo-scan
  :disabled
  :ensure (:host github :repo "ultronozm/flymake-overlays.el" :depth nil)
  :after flymake
  :bind (:map flymake-mode-map
              ;; ("C-c t" . flymake-overlays-smart-toggle)
              )
  :hook (flymake-mode . flymake-overlays-mode))

(defun czm-add-lean4-eldoc ()
  (when (with-current-buffer eldoc-icebox-parent-buffer
          (or (eq major-mode 'lean4-mode)
              (equal (buffer-name)
                     "*Lean Goal*")))
    (add-hook 'eldoc-documentation-functions #'lean4-info-eldoc-function
              nil t)
    (eldoc-mode)))

(use-package eldoc-icebox
  :repo-scan
  :ensure (:host github :repo "ultronozm/eldoc-icebox.el" :depth nil)
  :bind (("C-c C-h" . eldoc-icebox-store)
         ("C-c C-n" . eldoc-icebox-toggle-display))
  :hook
  (eldoc-icebox-post-display . shrink-window-if-larger-than-buffer)
  (eldoc-icebox-post-display . czm-lean4-fontify-buffer)
  (eldoc-icebox-post-display . czm-add-lean4-eldoc))

(let ((file (locate-user-emacs-file "init-personal.el")))
  (when (file-exists-p file)
    (load file)))
