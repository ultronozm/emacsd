;;; -*- lexical-binding: t; -*-

(let ((file (locate-user-emacs-file "init-patches.el")))
  (when (file-exists-p file)
    (load file)))

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
  (("<up>" . windmove-up)
   ("<down>" . windmove-down)
   ("<left>" . windmove-left)
   ("<right>" . windmove-right)
   ("C-M-g" . down-list)
   ("C-M-z" . zap-up-to-char)
   ("C-c L" . org-insert-link-global)
   ("C-c a" . org-agenda)
   ("C-c f" . toggle-frame-fullscreen)
   ("C-c g" . goto-line)
   ("C-c l" . org-store-link)
   ("C-c r" . org-capture)
   ("C-c s" . shell)
   ("C-h C-f" . find-function)
   ("C-h C-l" . find-library)
   ("C-h C-v" . find-variable)
   ("C-s-O" . (lambda () (interactive) (other-frame -1)))
   ("C-s-o" . other-frame)
   ("C-x C-M-t" . transpose-regions)
   ("C-x C-b" . ibuffer)
   ("C-z" . nil)
   ("C-z c" . calendar)
   ("C-z C-c" . restart-emacs)
   ("C-z C-e" . pp-macroexpand-last-sexp)
   ("C-z C-s" . desktop-save-in-desktop-dir)
   ("C-z C-f" . desktop-read)
   ("H-0" . tab-close)
   ("H-1" . tab-close-other)
   ("H-2" . tab-bar-new-tab)
   ("H-SPC" . ediff-buffers)
   ;; H-ACFNHMD -- macOS annoyance
   ("H-b" . abbrev-mode)
   ("H-e" . toggle-debug-on-error)
   ("H-f" . follow-mode)
   ("H-i" . overwrite-mode)
   ("H-k" . flycheck-mode)
   ("H-l" . flymake-mode)
   ("H-s" . whitespace-mode)
   ("H-v" . visual-line-mode)
   ("H-w" . which-function-mode)
   ("M-+" . raise-sexp)
   ("M-_" . delete-pair)
   ("M-i" . mark-inner)
   ("M-n" . next-error)
   ("M-p" . previous-error)
   ("M-u" . up-list)
   ([remap dabbrev-expand] . hippie-expand)
   ("s-'" . expand-abbrev)
   ("s-." . repeat)
   ("s-0" . delete-window)
   ("s-1" . delete-other-windows)
   ("s-2" . split-window-below)
   ("s-3" . split-window-right)
   ("s-6" . (lambda () (interactive) (delete-indentation nil)))
   ("s-7" . (lambda () (interactive) (delete-indentation t)))
   ("s-<up>" . (lambda () (interactive) (enlarge-window 5)))
   ("s-<down>" . (lambda () (interactive) (shrink-window 5)))
   ("s-<left>" . previous-buffer)
   ("s-<right>" . next-buffer)
   ("s-SPC" . cycle-spacing)
   ("s-a" . beginning-of-list)
   ("s-A" . kill-to-beginning-of-list)
   ("s-b" . switch-to-buffer)
   ("s-c" . nil)
   ("s-C" . nil)
   ;; "s-D" ; dired
   ("s-e" . end-of-list)
   ("s-E" . kill-to-end-of-list)
   ;; s-f
   ("s-g" . maximize-window-with-clipboard)
   ("s-h" . nil)
   ("s-H" . nil)
   ("s-i" . find-init-file)
   ("s-k" . kill-current-buffer)
   ("s-K" . kill-buffer-and-window)
   ;; "s-l" ; goto-line
   ("s-L" . nil)
   ("s-m" . nil)
   ;; "s-M" ; manual-entry
   ("s-n" . outline-next-heading)
   ("s-N" . make-frame)
   ("s-o" . other-window)
   ("s-O" . (lambda () (interactive) (other-window -1)))
   ("s-p" . outline-previous-heading)
   ("s-q" . bury-buffer)
   ("s-s" . save-buffer)
   ("s-u" . nil) ; content-quoter-dwim
   ("s-v" . view-mode)
   ("s-w" . nil) ; delete frame
   ("s-x" . nil)
   ("s-y" . replace-buffer-with-clipboard)
   ("s-z" . nil))
  (:map
   emacs-lisp-mode-map
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
  ;; (dired-isearch-filenames t)
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
  (tab-bar-format
   '(
     ;; tab-bar-format-menu-bar
     ;; tab-bar-format-history
     tab-bar-format-tabs-groups
     tab-bar-format-align-right
     tab-bar-format-global))
  (recentf-max-saved-items 100)
  (calc-kill-line-numbering nil)
  (eglot-connect-timeout 120)
  (safe-local-variable-values
   '((cmake-build-project-root . "./cpp")
     (checkdoc-minor-mode . t)
     (eval outline-hide-sublevels 5)
     (eval TeX-run-style-hooks "nla-notes")))
  (diary-comment-start ";;")
  :config
  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq-default indent-tabs-mode nil)
  (setq desktop-dirname user-emacs-directory)
  (electric-pair-mode)
  (minibuffer-depth-indicate-mode)
  (global-auto-revert-mode)
  (save-place-mode)
  (column-number-mode)
  (tab-bar-history-mode)
  (display-time-mode)
  (add-to-list 'auto-mode-alist '("\\.info\\'" . Info-on-current-buffer))
  (setcdr other-window-repeat-map nil)
  (setcdr buffer-navigation-repeat-map nil)
  (repeat-mode)
  (recentf-mode)
  :hook
  (prog-mode . outline-minor-mode)
  (dired-mode . dired-hide-details-mode)
  (dired-mode . hl-line-mode))

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
  (pcase-dolist
      (`(,face . ,height)
       '((default . 150)
         (mode-line . 120)
         (mode-line-inactive . 120)
         (tab-bar . 120)))
    (set-face-attribute face nil :height height)))

(czm-set-face-heights)

(defun foldout-exit-fold-without-hiding ()
  (interactive)
  (foldout-exit-fold -1))

(use-package outline
  :ensure nil
  :defer t
  :config
  (require 'foldout)
  :bind
  (:map
   outline-minor-mode-map
   ("C-M-<down-mouse-1>" . nil)
   ("C-M-<down-mouse-2>" . nil)
   ("C-M-<down-mouse-3>" . nil)
   ("<right-margin> S-<mouse-1>" . nil)
   ("<right-margin> <mouse-1>" . nil)
   ("<left-margin> S-<mouse-1>" . nil)
   ("<left-margin> <mouse-1>" . nil))
  (:repeat-map
   outline-repeat-map
   ("n" . outline-next-heading)
   ("p" . outline-previous-heading)
   ("u" . outline-up-heading)
   ("f" . outline-forward-same-level)
   ("b" . outline-backward-same-level)
   ("<left>" . outline-promote)
   ("<right>" . outline-demote)
   ("<up>" . outline-move-subtree-up)
   ("<down>" . outline-move-subtree-down)
   ("x" . foldout-exit-fold-without-hiding)
   ("z" . foldout-zoom-subtree)
   ("a" . outline-show-all)
   ("c" . outline-hide-entry)
   ("d" . outline-hide-subtree)
   ("e" . outline-show-entry)
   ("TAB" . outline-show-children)
   ("k" . outline-show-branches)
   ("l" . outline-hide-leaves)
   ("RET" . outline-insert-heading)
   ("o" . outline-hide-other)
   ("q" . outline-hide-sublevels)
   ("s" . outline-show-subtree)
   ("t" . outline-hide-body)
   ("@" . outline-mark-subtree)
   :continue-only
   ("C-M-SPC" . outline-mark-subtree)
   ("w" . kill-region)
   ("M-w" . kill-ring-save)
   ("C-/" . undo)
   ("y" . yank)))

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
  :defer
  :hook ((prog-mode text-mode) . abbrev-mode)
  :custom
  (abbrev-file-name (concat user-emacs-directory "abbrev_defs.el"))
  (save-abbrevs 'silently)
  :config
  (let ((file (concat user-emacs-directory "abbrev_defs.el")))
    (when (file-exists-p file)
      (quietly-read-abbrev-file file)))
  (quietly-read-abbrev-file (concat user-emacs-directory "abbrev.el")))

;; don't remember the point of this
(cl-defmethod project-root ((project (head local)))
  "TODO."
  (cdr project))

(defun czm/project-try-local (dir)
  "Determine if DIR is a non-Git project.
DIR must include a .project file to be considered a project."
  (let ((root (locate-dominating-file dir ".project")))
    (and root (cons 'local root))))

(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'czm/project-try-local))

(bind-keys
 :package foldout
 ("C-x n w" . foldout-widen-to-current-fold))

(bind-keys
 :package calendar
 :map calendar-mode-map
 ("<left>" nil) ("<right>" nil) ("<up>" nil) ("<down>" nil))

(defvar maximize-window-mode-history nil
  "History of major modes used in maximize-window function.")

(defun maximize-window-with-clipboard ()
  "Create a new tab, split window, paste clipboard, and run ediff.
If region is active, narrows to the region in an indirect buffer first.
Otherwise, uses the whole buffer.  Creates a new tab, splits it vertically,
creates a new buffer with clipboard contents, uses the same major mode as
the original buffer, and runs ediff on both buffers."
  (interactive)
  (let* ((original-buffer (current-buffer))
         (original-mode major-mode)
         (clipboard-contents (current-kill 0))
         (region-active (use-region-p))
         (region-beginning (when region-active (region-beginning)))
         (region-end (when region-active (region-end)))
         (indirect-buffer (when region-active
                            (deactivate-mark)
                            (make-indirect-buffer original-buffer
                                                  (generate-new-buffer-name
                                                   (concat (buffer-name) "-region"))
                                                  t)))
         (source-buffer (or indirect-buffer original-buffer))
         (new-buffer (generate-new-buffer "*clipboard-compare*")))
    (tab-new)
    (when indirect-buffer
      (switch-to-buffer indirect-buffer)
      (narrow-to-region region-beginning region-end))
    (delete-other-windows)
    (let ((right-window (split-window-right)))
      (with-selected-window right-window
        (switch-to-buffer new-buffer)
        (insert clipboard-contents)
        (funcall original-mode))
      (let ((ediff-buf (ediff-buffers source-buffer new-buffer))
            (cleanup-function (lambda ()
                                (when indirect-buffer
                                  (kill-buffer indirect-buffer))
                                ;; (kill-buffer new-buffer)
                                (tab-bar-close-tab))))
        (with-current-buffer ediff-buf
          (add-hook 'ediff-quit-hook cleanup-function nil t))))))

(defun replace-buffer-with-clipboard ()
  "Erase buffer and replace its contents with clipboard."
  (interactive)
  (erase-buffer)
  (yank)
  (ediff-current-file))

(defun ediff-current-file--with-cleanup-advice (orig-fun &rest _args)
  "Replace `ediff-current-file' with a version that cleans up properly."
  (interactive)
  (unless (or (not (eq revert-buffer-function 'revert-buffer--default))
              (not (eq revert-buffer-insert-file-contents-function
                       'revert-buffer-insert-file-contents--default-function))
              (and buffer-file-number
                   (or (buffer-modified-p)
                       (not (verify-visited-file-modtime
                             (current-buffer))))))
    (error "Nothing to revert"))
  
  (let* ((cwc (current-window-configuration))
         (auto-save-p (and (recent-auto-save-p)
                           buffer-auto-save-file-name
                           (file-readable-p buffer-auto-save-file-name)
                           (y-or-n-p
                            "Buffer has been auto-saved recently.  Compare with auto-save file? ")))
         (file-name (if auto-save-p
                        buffer-auto-save-file-name
                      buffer-file-name))
         (revert-buf-name (concat "FILE=" file-name))
         (revert-buf (get-buffer revert-buf-name))
         (current-major major-mode))
    
    (unless file-name
      (error "Buffer does not seem to be associated with any file"))
    
    (when revert-buf
      (kill-buffer revert-buf)
      (setq revert-buf nil))
    
    (setq revert-buf (get-buffer-create revert-buf-name))
    (with-current-buffer revert-buf
      (insert-file-contents file-name)
      ;; Assume same modes:
      (funcall current-major))
    
    ;; Start ediff with our cleanly captured revert-buf
    (let ((ediff-buf (ediff-buffers revert-buf (current-buffer))))
      (with-current-buffer ediff-buf
        (add-hook 'ediff-quit-hook
                  (lambda ()
                    ;; First cleanup the ediff buffers
                    (ediff-cleanup-mess)
                    
                    ;; Now kill our revert buffer
                    (when (buffer-live-p revert-buf)
                      (kill-buffer revert-buf))
                    
                    ;; Finally restore the window configuration
                    (when (window-configuration-p cwc)
                      (set-window-configuration cwc)))
                  nil t)))))

(advice-add 'ediff-current-file :around #'ediff-current-file--with-cleanup-advice)
