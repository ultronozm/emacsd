;;; -*- lexical-binding: t; -*-

(when (string-equal system-type "darwin")
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier 'super)
  (setq ns-function-modifier 'hyper))

(when (string-equal system-type "windows-nt")
  (setq w32-lwindow-modifier 'super)
  (w32-register-hot-key [s-])
  (w32-register-hot-key [s]))

(mapc (lambda (keybind)
        (let ((key (car keybind))
              (cmd (cadr keybind)))
          (global-set-key (kbd key)
                          cmd)))
      '(("C-c g" goto-line)
        ("C-x C-b" ibuffer)
        ("s-b" switch-to-buffer)
        ("s-d" find-file)
        ("s-0" delete-window)
        ("s-1" delete-other-windows)
        ("s-2" split-window-below)
        ("s-3" split-window-right)
        ;; ("C-m" newline-and-indent)
        ("M-n" next-error)
        ("M-p" previous-error)
        ("M-/" hippie-expand)
        ("C-c d" czm-dired-downloads)
        ("s-d" czm-find-math-document)
        ;; ("C-c m" compile)
        ("C-c r" org-capture)
        ;; ("C-c o" org-open-at-point-global)
        ("C-c l" org-store-link)
        ("C-c L" org-insert-link-global)
        ("C-c a" org-agenda)
        ("C-c s" shell)
        ("C-c f" toggle-frame-fullscreen)
        ("C-h C-f" find-function)
        ("C-h C-v" find-variable)
        ("C-h C-l" find-library)
        ("H-SPC" ediff-buffers)
        ("H-b" abbrev-mode)
        ("H-e" toggle-debug-on-error)
        ("H-f" follow-mode)
        ("H-i" overwrite-mode)
        ("H-l" flymake-mode)
        ("H-k" flycheck-mode)
        ("H-s" whitespace-mode)
        ("H-v" visual-line-mode)
        ("H-w" which-function-mode)
        ("C-M-z" zap-up-to-char)
        ("C-M-g" down-list)
        ("M-+" raise-sexp)
        ("M-_" delete-pair)
        ("s-<left>" previous-buffer)
        ("s-<right>" next-buffer)
        ("s-<up>" (lambda ()
                    (interactive)
                    (enlarge-window 5)))
        ("s-<down>" (lambda ()
                      (interactive)
                      (shrink-window 5)))
        ("s-o" other-window)
        ("s-O" (lambda ()
                 (interactive)
                 (other-window -1)))
        ("C-s-o" other-frame)
        ("C-s-O" (lambda ()
                   (interactive)
                   (other-frame -1)))
        ("s-'" nil)
        ("s-n" nil)
        ("s-N" make-frame)
        ("s-n" outline-next-heading)
        ("s-p" outline-previous-heading)
        ("s-q" bury-buffer)
        ("s-k" kill-current-buffer)
        ("s-K" kill-buffer-and-window)
        ("s-s" save-buffer)
        ("s-v" view-mode)
        ("s-." repeat)
        ("s-i" czm/find-lisp-file)
        ("<up>" windmove-up)
        ("<down>" windmove-down)
        ("<right>" windmove-right)
        ("<left>" windmove-left)
        ("s-SPC" cycle-spacing)
        ("s-6" (lambda () (interactive) (delete-indentation nil)))
        ("s-7" (lambda () (interactive) (delete-indentation t)))
        ("C-x C-M-t" transpose-regions)
        ("H-0" tab-close)
        ("H-1" tab-close-other)
        ("H-2" tab-bar-new-tab)
        ("M-u" up-list)
        ("M-i" czm-mark-inner)
        ("s-a" czm-beginning-of-list)
        ("s-e" czm-end-of-list)))

;; TODO: add "burp.el" functionality here, since it's very handy when
;; working with startup configs

(defun czm/find-lisp-file (&optional arg)
  "Opens an elisp file in the ~/.emacs.d or ~/.emacs.d/lisp directory.
With prefix argument ARG, opens user-init-file directly."
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

(defun czm-dired-downloads ()
  "Open the downloads directory in Dired mode."
  (interactive)
  (dired my-downloads-folder))

(defun czm-find-math-document ()
  "Find a file in the math documents folder."
  (interactive)
  (project-find-file-in nil (list my-math-folder) `(local . ,my-math-folder)))

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

(defun czm-dired-git-files ()
  "Open dired buffer with files in current git repo."
  (interactive)
  (let ((git-files (shell-command-to-string "git ls-files")))
    (setq git-files (split-string git-files "\n" t))
    (dired (cons "." git-files))))

(defun czm-delete-pair (&optional arg)
  "Delete a pair of characters enclosing ARG sexps that follow point.
A negative ARG deletes a pair around the preceding ARG sexps instead.
The option `delete-pair-blink-delay' can disable blinking.

Only difference with the usual `delete-pair' is that this version
pushes the mark somewhere useful."
  (interactive "P")
  (if arg
      (setq arg (prefix-numeric-value arg))
    (setq arg 1))
  (if (< arg 0)
      (save-excursion
        (skip-chars-backward " \t")
        (save-excursion
          (let ((close-char (char-before)))
            (forward-sexp arg)
            (unless (member (list (char-after) close-char)
                            (mapcar (lambda (p)
                                      (if (= (length p) 3) (cdr p) p))
                                    insert-pair-alist))
              (error "Not after matching pair"))
            (when (and (numberp delete-pair-blink-delay)
                       (> delete-pair-blink-delay 0))
              (sit-for delete-pair-blink-delay))
            (delete-char 1)))
        (delete-char -1))
    (save-excursion
      (skip-chars-forward " \t")
      (save-excursion
        (let ((open-char (char-after)))
          (forward-sexp arg)
          (unless (member (list open-char (char-before))
                          (mapcar (lambda (p)
                                    (if (= (length p) 3) (cdr p) p))
                                  insert-pair-alist))
            (error "Not before matching pair"))
          (when (and (numberp delete-pair-blink-delay)
                     (> delete-pair-blink-delay 0))
            (sit-for delete-pair-blink-delay))
          (delete-char -1)
          (push-mark) ; added!
          ))
      (delete-char 1))))

(advice-add 'delete-pair :override #'czm-delete-pair)

(dolist (key '(
               ;; s-a
               "s-C"
               ;; "s-D" ; dired
               ;; ("s-w" switch-to-buffer)
               ;; s-f
               "s-E"
               "s-H"
               "s-L"
               ;; "s-M" ; manual-entry
               "s-S"
               "s-c"
               "s-g"
               "s-h"
               ;; "s-l" ; goto-line
               "s-m"
               "s-u"
               ;; "s-q"
               ;; s-t
               "s-x"
               "s-w" ; delete frame
               ;; "s-y"
               "s-z"
               ))
  (global-unset-key (kbd key)))

;; H-ACFNHMD -- macOS annoyance


(use-package emacs
  :ensure nil

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

  :config
  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq-default indent-tabs-mode nil)
  (electric-pair-mode)
  (minibuffer-depth-indicate-mode)
  (global-auto-revert-mode)
  (save-place-mode)
  (line-number-mode)
  (column-number-mode)
  (tab-bar-history-mode))

(use-package emacs
  :ensure nil

  :custom
  (display-time-default-load-average nil)

  :config
  (display-time-mode))

(use-package recentf
  :ensure nil

  :custom
  (recentf-max-saved-items 100)
  :config
  (recentf-mode))

(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . outline-minor-mode))

(use-package emacs
  :ensure nil
  :after outline
  :bind (:map outline-minor-mode-map
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

(defun czm-mark-inner ()
  (interactive)
  (condition-case nil
      (progn
        (backward-up-list)
        (down-list)
        (set-mark (point))
        (up-list)
        (czm-backward-down-list))
    (error (message "No inner list found."))))

(defun czm-end-of-list ()
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

(defun czm-beginning-of-list ()
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

(defun czm-backward-down-list ()
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

(unless (eq window-system 'w32)
  (use-package emacs
    :ensure nil

    :after cc-mode

    :custom
    (abbrev-file-name (concat user-emacs-directory "abbrev_defs.el"))
    (save-abbrevs 'silently)

    :hook
    (text-mode . abbrev-mode)
    (vc-git-log-edit-mode . abbrev-mode)

    :config
    (let ((abbrev-file (concat user-emacs-directory "abbrev_defs.el")))
      (when (file-exists-p abbrev-file)
        (quietly-read-abbrev-file abbrev-file)))
    (quietly-read-abbrev-file (concat user-emacs-directory "abbrev.el"))))

(use-package calc
  :ensure nil
  :defer t
  :custom
  (calc-kill-line-numbering nil))

(use-package eglot
  :ensure nil
  :defer t
  :custom
  (eglot-connect-timeout 120))

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

  :config
  (add-to-list 'project-find-functions 'czm/project-try-local))

(defun foldout-widen-to-current-fold ()
  "Widen to the current fold level.
If in a fold, widen to that fold's boundaries.
If not in a fold, acts like `widen'."
  (interactive)
  (if foldout-fold-list
      (let* ((last-fold (car foldout-fold-list))
             (start (car last-fold))
             (end (cdr last-fold)))
        (widen)
        (narrow-to-region start
                          (if end (1- (marker-position end)) (point-max))))
    (widen)))

(use-package foldout
  :ensure nil
  :bind
  ("C-x n w" . foldout-widen-to-current-fold))

(use-package calendar
  :ensure nil
  ;; unbind arrow keys from calendar-mode
  :bind
  (:map calendar-mode-map
        ("<left>" . nil)
        ("<right>" . nil)
        ("<up>" . nil)
        ("<down>" . nil)))

(put 'erase-buffer 'disabled nil)
