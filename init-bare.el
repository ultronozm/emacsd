;;; -*- lexical-binding: t; -*-

;;; Emacs patches

;; won't be necessary in Emacs 31+
(defun delete-pair (&optional arg)
  "Delete a pair of characters enclosing ARG sexps that follow point.
A negative ARG deletes a pair around the preceding ARG sexps instead.
The option `delete-pair-blink-delay' can disable blinking.

Redefinition of the usual `delete-pair'.  This version pushes the
mark somewhere useful."
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

;; in current Emacs master, can eventually be deleted from here
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

(defun bind-key--make-continue-alias (cmd map)
  "Make an alias for CMD that can continue MAP but not enter it."
  (intern (concat (symbol-name cmd) "|" (symbol-name map))))

(defun my-bind-keys-form (args keymap)
  "Bind multiple keys at once.

Accepts keyword arguments:
:map MAP               - a keymap into which the keybindings should be
                         added
:prefix KEY            - prefix key for these bindings
:prefix-map MAP        - name of the prefix map that should be created
                         for these bindings
:prefix-docstring STR  - docstring for the prefix-map variable
:menu-name NAME        - optional menu string for prefix map
:repeat-docstring STR  - docstring for the repeat-map variable
:repeat-map MAP        - name of the repeat map that should be created
                         for these bindings. If specified, the
                         `repeat-map' property of each command bound
                         (within the scope of the `:repeat-map' keyword)
                         is set to this map.
:exit BINDINGS         - Within the scope of `:repeat-map' will bind the
                         key in the repeat map, but will not set the
                         `repeat-map' property of the bound command.
:continue BINDINGS     - Within the scope of `:repeat-map' forces the
                         same behavior as if no special keyword had
                         been used (that is, the command is bound, and
                         it's `repeat-map' property set)
:continue-only BINDINGS - Within the scope of `:repeat-map' will bind an
                         alias of the command in the repeat map, but not
                         set the `repeat-map' property of the original
                         command.
:filter FORM           - optional form to determine when bindings apply

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted)."
  (let (map
        prefix-doc
        prefix-map
        prefix
        repeat-map
        repeat-doc
        repeat-type ;; Only used internally
        filter
        menu-name
        pkg)

    ;; Process any initial keyword arguments
    (let ((cont t)
          (arg-change-func 'cddr))
      (while (and cont args)
        (if (cond ((and (eq :map (car args))
                        (not prefix-map))
                   (setq map (cadr args)))
                  ((eq :prefix-docstring (car args))
                   (setq prefix-doc (cadr args)))
                  ((and (eq :prefix-map (car args))
                        (not (memq map '(global-map
                                         override-global-map))))
                   (setq prefix-map (cadr args)))
                  ((eq :repeat-docstring (car args))
                   (setq repeat-doc (cadr args)))
                  ((and (eq :repeat-map (car args))
                        (not (memq map '(global-map
                                         override-global-map))))
                   (setq repeat-map (cadr args))
                   (setq map repeat-map))
                  ((memq (car args) '(:continue :continue-only :exit))
                   (setq repeat-type (car args)
                         arg-change-func 'cdr))
                  ((eq :prefix (car args))
                   (setq prefix (cadr args)))
                  ((eq :filter (car args))
                   (setq filter (cadr args)) t)
                  ((eq :menu-name (car args))
                   (setq menu-name (cadr args)))
                  ((eq :package (car args))
                   (setq pkg (cadr args))))
            (setq args (funcall arg-change-func args))
          (setq cont nil))))

    (when (or (and prefix-map (not prefix))
              (and prefix (not prefix-map)))
      (error "Both :prefix-map and :prefix must be supplied"))

    (when repeat-type
      (unless repeat-map
        (error ":continue(-only) and :exit require specifying :repeat-map")))

    (when (and menu-name (not prefix))
      (error "If :menu-name is supplied, :prefix must be too"))

    (unless map (setq map keymap))

    ;; Process key binding arguments
    (let (first next)
      (while args
        (if (keywordp (car args))
            (progn
              (setq next args)
              (setq args nil))
          (if first
              (nconc first (list (car args)))
            (setq first (list (car args))))
          (setq args (cdr args))))

      (cl-flet
          ((wrap (map bindings)
             (if (and map pkg (not (memq map '(global-map
                                               override-global-map))))
                 `((if (boundp ',map)
                       ,(macroexp-progn bindings)
                     (eval-after-load
                         ,(if (symbolp pkg) `',pkg pkg)
                       ',(macroexp-progn bindings))))
               bindings)))

        (append
         (when prefix-map
           `((defvar ,prefix-map)
             ,@(when prefix-doc `((put ',prefix-map 'variable-documentation ,prefix-doc)))
             ,@(if menu-name
                   `((define-prefix-command ',prefix-map nil ,menu-name))
                 `((define-prefix-command ',prefix-map)))
             ,@(if (and map (not (eq map 'global-map)))
                   (wrap map `((bind-key ,prefix ',prefix-map ,map ,filter)))
                 `((bind-key ,prefix ',prefix-map nil ,filter)))))
         (when repeat-map
           `((defvar ,repeat-map (make-sparse-keymap)
               ,@(when repeat-doc `(,repeat-doc)))))
         (wrap map
               (cl-mapcan
                (lambda (form)
                  (let ((fun (and (cdr form) (list 'function (cdr form)))))
                    (if prefix-map
                        `((bind-key ,(car form) ,fun ,prefix-map ,filter))
                      (if (and map (not (eq map 'global-map)))
                          ;; Only needed in this branch, since when
                          ;; repeat-map is non-nil, map is always
                          ;; non-nil
                          (if (eq repeat-type :continue-only)
                              (let ((alias (bind-key--make-continue-alias (cdr form) map)))
                                `((defalias ',alias ,fun)
                                  (put ',alias 'repeat-map ',map)
                                  (bind-key ,(car form) ',alias ,map ,filter)))
                            `(,@(when (and repeat-map (not (eq repeat-type :exit)))
                                  `((put ,fun 'repeat-map ',repeat-map)))
                              (bind-key ,(car form) ,fun ,map ,filter)))
                        `((bind-key ,(car form) ,fun nil ,filter))))))
                first))
         (when next
           (bind-keys-form `(,@(when repeat-map `(:repeat-map ,repeat-map))
                             ,@(if pkg
                                   (cons :package (cons pkg next))
                                 next)) map)))))))

(advice-add #'bind-keys-form :override #'my-bind-keys-form)
;;; Main config

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
   ([remap dabbrev-expand] . hippie-expand)
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

;; forget the point of this:

;; (defun czm-expand-abbrev-advice (orig-fun &rest args)
;;   (unless (and (eq major-mode 'LaTeX-mode)
;;                (nth 4 (syntax-ppss)))
;;     (apply orig-fun args)))

(use-package abbrev
  :ensure nil
  :hook ((prog-mode text-mode) . abbrev-mode)
  :custom
  (abbrev-file-name (concat user-emacs-directory "abbrev_defs.el"))
  (save-abbrevs 'silently)
  :config
  (let ((file (concat user-emacs-directory "abbrev_defs.el")))
    (when (file-exists-p file)
      (quietly-read-abbrev-file file)))
  (quietly-read-abbrev-file (concat user-emacs-directory "abbrev.el"))
  ;; (advice-add 'expand-abbrev :around #'czm-expand-abbrev-advice)
  )

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
  :defer t
  :config (add-to-list 'project-find-functions 'czm/project-try-local))

(use-package foldout
  :ensure nil
  :defer t
  :bind ("C-x n w" . foldout-widen-to-current-fold))

(use-package calendar
  :ensure nil
  :defer t
  :bind (:map calendar-mode-map
              ("<left>" . nil) ("<right>" . nil)
              ("<up>" . nil) ("<down>" . nil)))

