;;; -*- lexical-binding: t; -*-

(when (string-equal system-type "darwin")
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier 'super)
  (setq ns-function-modifier 'hyper))

(when (string-equal system-type "windows-nt")
  (setq w32-lwindow-modifier 'super)
  (w32-register-hot-key [s-])
  (w32-register-hot-key [s]))

(use-package emacs
  :ensure nil
  :bind
  (("C-c g" . goto-line)
   ("C-x C-b" . ibuffer)
   ("s-b" . switch-to-buffer)
   ("s-0" . delete-window)
   ("s-1" . delete-other-windows)
   ("s-2" . split-window-below)
   ("s-3" . split-window-right)
   ("M-n" . next-error)
   ("M-p" . previous-error)
   ("M-/" . hippie-expand)
   ("C-c r" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c L" . org-insert-link-global)
   ("C-c a" . org-agenda)
   ("C-c s" . shell)
   ("C-c f" . toggle-frame-fullscreen)
   ("C-h C-f" . find-function)
   ("C-h C-v" . find-variable)
   ("C-h C-l" . find-library)
   ("H-SPC" . ediff-buffers)
   ("H-b" . abbrev-mode)
   ("H-e" . toggle-debug-on-error)
   ("H-f" . follow-mode)
   ("H-i" . overwrite-mode)
   ("H-l" . flymake-mode)
   ("H-k" . flycheck-mode)
   ("H-s" . whitespace-mode)
   ("H-v" . visual-line-mode)
   ("H-w" . which-function-mode)
   ("C-M-z" . zap-up-to-char)
   ("C-M-g" . down-list)
   ("M-+" . raise-sexp)
   ("M-_" . delete-pair)
   ("s-<left>" . previous-buffer)
   ("s-<right>" . next-buffer)
   ("s-<up>" . (lambda () (interactive) (enlarge-window 5)))
   ("s-<down>" . (lambda () (interactive) (shrink-window 5)))
   ("s-o" . other-window)
   ("s-O" . (lambda () (interactive) (other-window -1)))
   ("C-s-o" . other-frame)
   ("C-s-O" . (lambda () (interactive) (other-frame -1)))
   ("s-'" . nil)
   ("s-n" . nil)
   ("s-N" . make-frame)
   ("s-n" . outline-next-heading)
   ("s-p" . outline-previous-heading)
   ("s-q" . bury-buffer)
   ("s-k" . kill-current-buffer)
   ("s-K" . kill-buffer-and-window)
   ("s-s" . save-buffer)
   ("s-v" . view-mode)
   ("s-." . repeat)
   ("s-i" . find-init-file)
   ("<up>" . windmove-up)
   ("<down>" . windmove-down)
   ("<right>" . windmove-right)
   ("<left>" . windmove-left)
   ("s-SPC" . cycle-spacing)
   ("s-6" . (lambda () (interactive) (delete-indentation nil)))
   ("s-7" . (lambda () (interactive) (delete-indentation t)))
   ("C-x C-M-t" . transpose-regions)
   ("H-0" . tab-close)
   ("H-1" . tab-close-other)
   ("H-2" . tab-bar-new-tab)
   ("M-u" . up-list)
   ("M-i" . mark-inner)
   ("s-a" . beginning-of-list)
   ("s-e" . end-of-list)
   ("s-A" . kill-to-beginning-of-list)
   ("s-E" . kill-to-end-of-list)
   ("s-C" . nil)
   ;; "s-D" ; dired
   ;; ("s-w" switch-to-buffer)
   ;; s-f
   ("s-H" . nil)
   ("s-L" . nil)
   ;; "s-M" ; manual-entry
   ("s-S" . nil)
   ("s-c" . nil)
   ("s-g" . nil)
   ("s-h" . nil)
   ;; "s-l" ; goto-line
   ("s-m" . nil)
   ("s-u" . nil)
   ;; "s-q"
   ;; s-t
   ("s-x" . nil)
   ("s-w" . nil) ; delete frame
   ;; "s-y"
   ("s-z" . nil)
   ;; H-ACFNHMD -- macOS annoyance
   )
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (use-dialog-box nil)
  (show-paren-delay 0)
  (show-paren-style 'parenthesis)
  (ring-bell-function #'ignore)
  (initial-scratch-message nil)
  (inhibit-startup-message t)
  (echo-keystrokes 0.01)
  (mark-even-if-inactive nil)
  (tramp-default-method "ssh")
  (tramp-ssh-extra-args (list "-i" "~/.ssh/"))
  (password-cache-expiry nil)
  (enable-recursive-minibuffers t)
  (max-lisp-eval-depth 12000)
  (bookmark-save-flag 1)
  (dired-create-destination-dirs 'ask)
  (dired-isearch-filenames t)
  (dired-vc-rename-file t)
  (large-file-warning-threshold 20000000)
  (vc-follow-symlinks t)
  (view-read-only t)
  (delete-pair-blink-delay 0)
  (delete-by-moving-to-trash t)
  (help-window-select t)
  (isearch-allow-scroll t)
  (search-upper-case t)
  (doc-view-resolution 300)
  (backup-directory-alist
   `(("." . ,(expand-file-name
              (concat user-emacs-directory "backups")))))
  (auto-save-file-name-transforms
   `((".*" ,(expand-file-name
             (concat user-emacs-directory "auto-save/"))
      t)))
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (display-time-default-load-average nil)
  :config
  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq-default indent-tabs-mode nil)
  (electric-pair-mode)
  (minibuffer-depth-indicate-mode)
  (global-auto-revert-mode)
  (save-place-mode)
  (line-number-mode)
  (column-number-mode)
  (tab-bar-history-mode)
  (display-time-mode)
  (add-to-list 'auto-mode-alist '("\\.info\\'" . Info-on-current-buffer)))

(defun find-init-file (&optional arg)
  "Opens an elisp file in the ~/.emacs.d or ~/.emacs.d/lisp directory.
With prefix argument ARG, opens `user-init-file' directly."
  (interactive "P")
  (if arg
      (find-file user-init-file)
    (let* ((elisp-dir1 (expand-file-name user-emacs-directory))
           (elisp-files (append
                         (directory-files elisp-dir1 t "\\.el$")))
           (default-elisp-file (concat user-emacs-directory "init.el"))
           (selected-elisp-file (completing-read
                                 "Select elisp file: " elisp-files
                                 nil t nil nil default-elisp-file)))
      (when selected-elisp-file (find-file selected-elisp-file)))))

(defvar edebug-previous-result-raw nil) ;; Last result returned, raw.
(defun edebug-compute-previous-result (previous-value)
  "Redefinition of built-in function `edebug-compute-previous-result'.
This version saves PREVIOUS-VALUE in `edebug-previous-result-raw'."
  (if edebug-unwrap-results
      (setq previous-value
            (edebug-unwrap* previous-value)))
  (setq edebug-previous-result-raw previous-value)
  (setq edebug-previous-result
        (concat "Result: "
                (edebug-safe-prin1-to-string previous-value)
                (eval-expression-print-format previous-value))))

(defun mark-inner ()
  "Mark interior of the current list."
  (interactive)
  (condition-case nil
      (progn
        (backward-up-list)
        (down-list)
        (set-mark (point))
        (up-list)
        (backward-down-list))
    (error (message "No inner list found."))))

(defun end-of-list ()
  "Move to the end of the current list."
  (interactive)
  (let ((last (point))
        (continue t))
    (while continue
      (condition-case nil
          (progn
            (forward-sexp)
            (when (<= (point) last)
              (setq continue nil)))
        (scan-error
         (setq continue nil)))
      (setq last (point)))))

(defun beginning-of-list ()
  "Move to the beginning of the current list."
  (interactive)
  (let ((last (point))
        (continue t))
    (while continue
      (condition-case nil
          (progn
            (backward-sexp)
            (when (>= (point) last)
              (setq continue nil)))
        (scan-error
         (setq continue nil)))
      (setq last (point)))))

(defun kill-to-end-of-list ()
  "Kill text between point and end of current list."
  (interactive)
  (let ((end (save-excursion (end-of-list) (point))))
    (kill-region (point) end)))

(defun kill-to-beginning-of-list ()
  "Kill text between point and beginning of current list."
  (interactive)
  (let ((beginning (save-excursion (beginning-of-list) (point))))
    (kill-region beginning (point))))

(defun backward-down-list ()
  "Move backward down a list."
  (interactive)
  (down-list -1))

(defun czm-set-face-heights ()
  "Set the heights of various faces."
  (set-face-attribute 'default nil :height 150)
  (set-face-attribute 'mode-line nil :height 120)
  (set-face-attribute 'mode-line-inactive nil :height 120)
  (set-face-attribute 'tab-bar nil :height 120))

(czm-set-face-heights)

(use-package recentf
  :ensure nil
  :custom (recentf-max-saved-items 100)
  :config (recentf-mode))

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . outline-minor-mode))

(use-package outline
  :ensure nil
  :bind
  (:map outline-minor-mode-map
        ("C-M-<down-mouse-1>" . nil)
        ("C-M-<down-mouse-2>" . nil)
        ("C-M-<down-mouse-3>" . nil)
        ("<right-margin> S-<mouse-1>" . nil)
        ("<right-margin> <mouse-1>" . nil)
        ("<left-margin> S-<mouse-1>" . nil)
        ("<left-margin> <mouse-1>" . nil)))

(use-package repeat
  :ensure nil
  :config
  (setcdr other-window-repeat-map nil)
  (repeat-mode))

;; This could be its own package, accommodating git-friendly abbrev storage?
;; Need a good way to update the source.
(defun modify-abbrev-table (table abbrevs)
  "Define abbreviations in TABLE given by ABBREVS."
  (unless table
    (error "Abbrev table does not exist" table))  ;; Message could be improved
  (dolist (abbrev abbrevs)
    (define-abbrev table (car abbrev) (cadr abbrev) (caddr abbrev))))

(use-package abbrev
  :ensure nil
  :config
  (setq-default abbrev-mode t)
  :custom
  (abbrev-file-name (concat user-emacs-directory "abbrev_defs.el"))
  (save-abbrevs 'silently)
  :config
  (let ((abbrev-file (concat user-emacs-directory "abbrev_defs.el")))
    (when (file-exists-p abbrev-file)
      (quietly-read-abbrev-file abbrev-file)))
  (quietly-read-abbrev-file (concat user-emacs-directory "abbrev.el")))

;; (use-package emacs
;;   :ensure nil
;;   :if (not (eq window-system 'w32))
;;   :after cc-mode)

(use-package calc
  :ensure nil
  :defer t
  :custom (calc-kill-line-numbering nil))

(use-package eglot
  :ensure nil
  :defer t
  :custom (eglot-connect-timeout 120))

;; don't remember the point of this
(cl-defmethod project-root ((project (head local)))
  "TODO."
  (cdr project))

(defun czm/project-try-local (dir)
  "Determine if DIR is a non-Git project.
DIR must include a .project file to be considered a project."
  (let ((root (locate-dominating-file dir ".project")))
    (and root (cons 'local root))))

(use-package project
  :ensure nil
  :config (add-to-list 'project-find-functions 'czm/project-try-local))

(use-package foldout
  :ensure nil
  :bind ("C-x n w" . foldout-widen-to-current-fold))

(use-package calendar
  :ensure nil
  :bind (:map calendar-mode-map
              ("<left>" . nil) ("<right>" . nil)
              ("<up>" . nil) ("<down>" . nil)))

(load (locate-user-emacs-file "init-obsolete.el"))
