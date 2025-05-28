;;; -*- lexical-binding: t; -*-

;;; basics
(setq use-package-compute-statistics t)

;; disable customization interface
(setq custom-file (locate-user-emacs-file "init-custom.el"))

(let ((file (locate-user-emacs-file "init-patches.el")))
  (when (file-exists-p file)
    (load file)))

(load (locate-user-emacs-file "init-settings.el"))

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
   ("C-x v e" . vc-ediff)
   ("C-z" . nil)
   ("C-z c" . calendar)
   ("C-z d" . eldoc-doc-buffer)
   ("C-z C-c" . restart-emacs)
   ("C-z C-e" . pp-macroexpand-last-sexp)
   ("C-z C-s" . desktop-save-in-desktop-dir)
   ("C-z C-f" . desktop-read)
   ("C-z s" . whitespace-cleanup)
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
   ("H-p" . prettify-symbols-mode)
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
   ("s-g" . clipboard-compare)
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
   ("s-v" . nil)
   ("s-w" . nil) ; delete frame
   ("s-x" . nil)
   ("s-y" . replace-buffer-with-clipboard)
   ("s-z" . nil))
  :custom
  (diff-entire-buffers nil)
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
  (delete-pair-push-mark t)
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
   '((aggressive-indent-mode)
     (cmake-build-project-root . "./cpp")
     (checkdoc-minor-mode . t)
     (eval outline-hide-sublevels 5)
     (eval TeX-run-style-hooks "nla-notes")))
  (diary-comment-start ";;")
  (mml-content-disposition-alist
   '((text (rtf . "attachment")
           (x-patch . "attachment")
           (x-diff . "attachment")
           (t . "inline"))
     (t . "attachment")))
  ;; (outline-minor-mode-use-buttons 'in-margins)
  (outline-minor-mode-use-buttons nil)
  (outline-minor-mode-cycle nil)
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
  (savehist-mode)
  :hook
  (prog-mode . outline-minor-mode)
  (dired-mode . dired-hide-details-mode)
  (dired-mode . hl-line-mode))

(defun find-init-file (&optional arg)
  "Opens an elisp file in the ~/.emacs.d or ~/.emacs.d/lisp directory.
With prefix argument ARG, opens `user-init-file' directly."
  (interactive "P")
  (if arg
      (find-file (or user-init-file
                     (expand-file-name "init.el" user-emacs-directory)))
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

(keymap-global-set "C-c d" #'czm-dired-downloads)

(defun czm-find-math-document ()
  "Find a file in the math documents folder."
  (interactive)
  (require 'project)
  (project-find-file-in nil (list my-math-folder) `(local . ,my-math-folder)))

(keymap-global-set "s-d" #'czm-find-math-document)

(defun czm-dired-drafts ()
  "Open drafts folder in Dired."
  (interactive)
  (dired message-auto-save-directory))

(keymap-global-set "C-z u" #'czm-dired-drafts)

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

(defun beginning-of-list ()
  "Move to the beginning of the current list.
Pushes a mark at the starting position."
  (interactive)
  (let ((origin (point))
        (last (point))
        (continue t))
    (while continue
      (condition-case nil
          (progn
            (backward-sexp)
            (when (>= (point) last)
              (setq continue nil)))
        (scan-error
         (setq continue nil)))
      (setq last (point)))
    (unless (= origin (point))
      (push-mark origin t))))

(defun end-of-list ()
  "Move to the end of the current list.
Pushes a mark at the starting position."
  (interactive)
  (let ((origin (point))
        (last (point))
        (continue t))
    (while continue
      (condition-case nil
          (progn
            (forward-sexp)
            (when (<= (point) last)
              (setq continue nil)))
        (scan-error
         (setq continue nil)))
      (setq last (point)))
    (unless (= origin (point))
      (push-mark origin t))))

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
  :demand t
  :config
  (require 'foldout)
  (setcdr outline-navigation-repeat-map nil)
  (setcdr outline-editing-repeat-map nil)
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
   ("<tab>" . outline-cycle)
   ("<backtab>" . outline-cycle-buffer)
   ("k" . outline-show-branches)
   ("l" . outline-hide-leaves)
   ;; ("RET" . outline-insert-heading)
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
  (add-to-list 'project-find-functions 'czm/project-try-local)
  (add-to-list 'project-switch-commands '(project-shell "Shell")))

(bind-keys
 :package foldout
 ("C-x n w" . foldout-widen-to-current-fold))

(bind-keys
 :package calendar
 :map calendar-mode-map
 ("<left>" . nil) ("<right>" . nil) ("<up>" . nil) ("<down>" . nil))

(bind-keys
 :package em-hist
 :map eshell-hist-mode-map
 ("<up>" . nil) ("<down>" . nil))

(defvar maximize-window-mode-history nil
  "History of major modes used in maximize-window function.")

(defun ediff-overwrite ()
  "Run Ediff between the current buffer (or its active region) and the clipboard.

If a region is active, the command first creates an indirect buffer that is
narrowed to that region; otherwise the whole buffer is used.  A new tab is then
opened, its frame is split vertically, and the clipboard contents are inserted
into a temporary buffer in the right window.  Both buffers share the same major
mode as the original.

Ediff is started on the two buffers and, when the Ediff session ends, all
temporary artifacts—the indirect buffer (if any), the clipboard buffer, and the
tab—are cleaned up automatically."
  (interactive)
  (let* ((original-buffer (current-buffer))
         (original-mode   major-mode)
         (clipboard-contents (current-kill 0))
         (region-active  (use-region-p))
         (region-beg     (when region-active (region-beginning)))
         (region-end     (when region-active (region-end)))
         (indirect-buffer (when region-active
                            (deactivate-mark)
                            (make-indirect-buffer
                             original-buffer
                             (generate-new-buffer-name
                              (concat (buffer-name) "-region"))
                             t)))
         (source-buffer  (or indirect-buffer original-buffer))
         (clip-buffer    (generate-new-buffer "*clipboard-compare*")))
    (tab-new)
    (when indirect-buffer
      (switch-to-buffer indirect-buffer)
      (narrow-to-region region-beg region-end))
    (delete-other-windows)
    (let ((right-window (split-window-right)))
      (with-selected-window right-window
        (switch-to-buffer clip-buffer)
        (insert clipboard-contents)
        (funcall original-mode))
      (let ((ediff-buf
             (ediff-buffers source-buffer clip-buffer))
            (cleanup
             (lambda ()
               (ediff-cleanup-mess)
               (when indirect-buffer (kill-buffer indirect-buffer))
               (tab-bar-close-tab))))
        (with-current-buffer ediff-buf
          (add-hook 'ediff-quit-hook cleanup nil t))))))

(defun diff-overwrite (&optional switches arg)
  "Unified diff between the current buffer/region and the clipboard.

• No prefix → run silently with \"-u\".  
• One C-u   → choose diff‑mode (still silent).  
• Two C-u   → additionally prompt for extra switches.

The patch shows the *current buffer’s file* as the OLD side (---) and
the clipboard as the NEW side (+++) so that
`diff-apply-hunk` (C‑c C‑a) can apply the change directly."
  (interactive
   (let ((arg current-prefix-arg))
     (list (and (>= (prefix-numeric-value (or arg 0)) 16)
                (read-from-minibuffer "Extra diff switches: "
                                      nil nil nil 'diff-switches-history))
           arg)))
  ;; ───────────────────────────────── gather input ─────────────────────────
  (let* ((orig-buf   (current-buffer))
         (file-name  (or (buffer-file-name orig-buf)
                         (user-error "Buffer isn’t visiting a file")))
         (regionp    (use-region-p))
         (src-buf    (if regionp
                         (let ((ib (make-indirect-buffer
                                    orig-buf
                                    (generate-new-buffer-name
                                     (concat (buffer-name) "-region"))
                                    t)))
                           (deactivate-mark)
                           (with-current-buffer ib
                             (narrow-to-region (region-beginning)
                                               (region-end)))
                           ib)
                       orig-buf))
         (clip-file  (make-temp-file "diff-overwrite-clip-"))
         (src-file   (make-temp-file "diff-overwrite-src-"))
         ;; first label = first file (old)   second label = second file (new)
         (labels     (list "--label" file-name "--label" "clipboard"))
         (switches   (append '("-u") labels (when switches (list switches)))))
    (unwind-protect
        (progn
          ;; write temp files ------------------------------------------------
          (with-temp-file clip-file (insert (current-kill 0)))
          (with-current-buffer src-buf
            (write-region (point-min) (point-max) src-file nil 'silent))
          ;; run diff --------------------------------------------------------
          (let* ((ret (diff src-file clip-file switches 'noasync))
                 (diff-buf (if (windowp ret) (window-buffer ret) ret)))
            ;; `diff` already enabled diff-mode; just add mapping so C‑c C‑a
            ;; knows where to patch without asking.
            (with-current-buffer diff-buf
              (setq-local diff-remembered-files-alist
                          (list (cons (list file-name "clipboard")
                                      file-name))))
            (pop-to-buffer diff-buf)))
      ;; cleanup ------------------------------------------------------------
      (delete-file clip-file) (delete-file src-file)
      (when (and regionp (buffer-live-p src-buf))
        (kill-buffer src-buf)))))

(defun clipboard-compare (&optional arg)
  "Comp4re the clipboard with the current buffer (or its active region).

No prefix ARG  →  call `ediff-overwrite` (side‑by‑side Ediff).

Any prefix ARG →  call `diff-overwrite`.
                  *One* C‑u shows the diff with default switches;
                  *two* C‑u’s lets `diff-overwrite` prompt for extra switches,
                  because the raw prefix it receives is (16)."
  (interactive "P")
  (if arg
      (let ((current-prefix-arg arg))      ; forward the exact prefix
        (call-interactively #'diff-overwrite))
    (ediff-overwrite)))

(defun replace-buffer-with-clipboard ()
  "Erase buffer and replace its contents with clipboard."
  (interactive)
  (erase-buffer)
  (yank)
  (ediff-current-file))

(advice-add 'describe-char :around
            (lambda (orig-fun &rest args)
              (let ((help-window-select nil))
                (apply orig-fun args))))

(defvar ediff-saved-window-configuration nil
  "Window configuration saved before Ediff was started.")

(defun ediff-save-window-configuration ()
  "Save the current window configuration for later restoration."
  (setq ediff-saved-window-configuration (current-window-configuration)))

(defun ediff-make-configuration-local ()
  "Make saved window configuration local to this Ediff control buffer.
This allows multiple Ediff sessions to each restore their own window configuration."
  (when (and ediff-control-buffer
             (buffer-live-p ediff-control-buffer)
             ediff-saved-window-configuration)
    (with-current-buffer ediff-control-buffer
      (make-local-variable 'ediff-saved-window-configuration))))

(defun ediff-restore-window-configuration ()
  "Restore the window configuration saved before Ediff started."
  (when (window-configuration-p ediff-saved-window-configuration)
    (run-with-timer
     0.01 nil (lambda ()
                (set-window-configuration ediff-saved-window-configuration)
                (setq ediff-saved-window-configuration nil)))))

(defun ediff-kill-temporary-file-buffer ()
  (when (and (buffer-live-p ediff-buffer-A)
             (or (string-prefix-p "FILE=" (buffer-name ediff-buffer-A))
                 (string-prefix-p "UNDO=" (buffer-name ediff-buffer-A))))
    (kill-buffer ediff-buffer-A)))

(add-hook 'ediff-before-setup-hook #'ediff-save-window-configuration)
(add-hook 'ediff-startup-hook #'ediff-make-configuration-local)
(add-hook 'ediff-quit-hook #'ediff-restore-window-configuration)
(add-hook 'ediff-cleanup-hook #'ediff-kill-temporary-file-buffer)

(defun project-format-patch-last-commit (&optional arg)
  "Create a patch file from the last commit in the current project.
With a prefix argument ARG, prompt for a specific commit.
The patch is saved in the project root directory and opened in a buffer."
  (interactive "P")
  (let* ((pr (project-current t))
         (root (project-root pr))
         (default-directory root)
         (commit (if arg
                     (vc-read-revision "Format patch from commit: " nil 'Git "HEAD")
                   "HEAD"))
         (git-output nil))
    
    (when (vc-git--empty-db-p)
      (user-error "No commits exist in this Git repository"))
    
    (message "Generating patch from %s..." commit)
    (setq git-output 
          (with-temp-buffer
            (if (zerop (vc-git--call t "format-patch" "-1" commit))
                (buffer-string)
              (user-error "Failed to generate patch from %s" commit))))
    
    (let ((filename (string-trim git-output)))
      (find-file (expand-file-name filename root))
      (diff-mode)
      (message "Patch saved as %s" filename))))

(defun create-directory-with-git-repo (dir)
  "Create directory DIR, initialize a Git repository, and open in Dired.
Interactively prompts for the directory to create."
  (interactive
   (list (read-file-name "Create directory with Git repo: "
                         default-directory default-directory
                         nil nil)))
  (make-directory dir t)
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (vc-create-repo 'Git)
    (dired default-directory)))

(bind-keys
 :package project
 :map project-prefix-map
 ("P" . project-format-patch-last-commit))

(with-eval-after-load 'smerge-mode
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map 'smerge-basic-map)))
   smerge-basic-map))

(bind-keys
 :package doc-view
 :map doc-view-mode-map
 ("C-c g" . doc-view-goto-page))

(with-eval-after-load 'tex-mode
  (mapc
   (lambda (sym) (add-to-list 'tex--prettify-symbols-alist sym))
   '(("\\eps" . ?ε)
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
     ("\\end{multline}" . ?⎭))))

(defun vc-git-amend ()
  (interactive)
  (vc-checkin nil 'git)
  (vc-git-log-edit-toggle-amend))

(defun my/vc-deduce-backend-advice (orig-fun)
  "Advice for `vc-deduce-backend` to handle indirect buffers."
  (or (funcall orig-fun) 'Git))

(advice-add 'vc-deduce-backend :around #'my/vc-deduce-backend-advice)

(bind-keys
 :repeat-map python-indent-repeat-map
 ("<" . python-indent-shift-left)
 (">" . python-indent-shift-right))

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
  (czm-create-scratch-file my-scratch-tex-dir "tex"))

(defun czm-create-scratch-sage ()
  "Create new scratch sage file."
  (interactive)
  (czm-create-scratch-file my-scratch-sage-dir "sage"))

(auto-insert-mode)
(add-to-list 'auto-insert-alist
             '("\\.tex\\'" . czm-setup-tex-file))

(tool-bar-mode 0)
(scroll-bar-mode 0)
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

(add-hook 'modus-themes-post-load-hook #'czm-set-face-heights)

(defvar czm--modus-vivendi-tinted-active nil)

(defun czm-toggle-dark-mode ()
  "Toggle between light and dark modes.
In dark mode:
- Uses modus-vivendi-tinted theme
- Enables dark mode for PDF viewing
In light mode:
- Uses default Emacs theme
- Disables dark mode for PDF viewing"
  (interactive)
  (if czm--modus-vivendi-tinted-active
      (progn
        (disable-theme 'modus-vivendi-tinted)
        (when (fboundp #'global-pdf-view-midnight-minor-mode)
          (global-pdf-view-midnight-minor-mode -1))
        (setq czm--modus-vivendi-tinted-active nil))
    (disable-theme 'modus-vivendi-tinted)
    (load-theme 'modus-vivendi-tinted t)
    (when (fboundp #'global-pdf-view-midnight-minor-mode)
      (global-pdf-view-midnight-minor-mode 1))
    (setq czm--modus-vivendi-tinted-active t))
  (czm-set-face-heights))

(keymap-global-set "H-t" #'czm-toggle-dark-mode)

(setopt use-package-verbose t
        use-package-minimum-reported-time 0.1)

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

(bind-keys
 :repeat-map sentence-repeat-map
 ("e" . forward-sentence)
 ("a" . backward-sentence)
 :continue-only
 ("M-h" . mark-end-of-sentence)
 ("h" . mark-end-of-sentence)
 ("k" . kill-sentence)
 ("w" . kill-region)
 ("M-w" . kill-ring-save)
 ("y" . yank)
 ("t" . transpose-sentences)
 ("C-l" . recenter-top-bottom))

(font-lock-add-keywords 'Info-mode '((" -- \\([^:]+\\): \\_<\\(.+\\)\\_>" . 2)))
(font-lock-add-keywords 'Info-mode '(("‘\\<\\([^’]+\\)\\>’" . 1)))

(unless user-init-file
  (message "Running bare init (emacs -q/-Q detected), skipping rest of init.el")
  (throw 'quit-init nil))

;;; use-package keyword :repo-scan, for packages that I develop

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

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
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

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))

(keymap-global-set "s-r" #'elpaca-rebuild)

(elpaca-wait)

;; (error)

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
  (exec-path-from-shell-initialize)
  (setopt elpaca-makeinfo-executable (executable-find "makeinfo"))
  (setopt elpaca-install-info-executable (executable-find "install-info"))
  (unless elpaca-makeinfo-executable
    (warn "makeinfo executable not found even after exec-path-from-shell"))
  (unless elpaca-install-info-executable
    (warn "install-info executable not found even after exec-path-from-shell")))

(elpaca-wait)

;;; lots of packages

(use-package diminish
  :demand t
  :config
  (diminish 'abbrev-mode "Ab")
  (diminish 'visual-line-mode)
  (dolist (mode '(reftex-mode
                  whitespace-mode))
    (with-eval-after-load 'mode
      (diminish mode)))
  (with-eval-after-load 'eldoc
    (diminish 'eldoc-mode))
  (with-eval-after-load 'outline
    (diminish 'outline-minor-mode))
  (with-eval-after-load 'follow
    (diminish 'follow-mode)))

(use-package aggressive-indent
  :defer t
  :diminish
  :hook
  ((emacs-lisp-mode LaTeX-mode rust-mode) . aggressive-indent-mode))

;; Remove "%n" from mode-line-modes -- I know when I'm narrowing.
(setq mode-line-modes (delete "%n" mode-line-modes))

(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "E")))
(add-hook 'lisp-interaction-mode-hook (lambda () (setq mode-name "LI")))

(with-eval-after-load 'tex-mode
  (add-hook 'LaTeX-mode-hook
            (lambda () (setq TeX-base-mode-name "L"))))

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
  (cl-letf*
      ((bounds (if (use-region-p)
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
                 :depth nil
                 :inherit nil :pin t)
  :bind (("s-@" . czm-misc-split-window-below-variant)
         ("s-#" . czm-misc-split-window-right-variant)
         ("s-4" . czm-misc-double-split-window-below-and-delete)
         ("s-5" . czm-misc-double-split-window-right-and-delete)
         ("s-6" . czm-misc-delete-indentation-nil)
         ("s-7" . czm-misc-delete-indentation-t)
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
  :bind
  (("C-c M-x" . consult-mode-command)
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
  (:map project-prefix-map
        ("g" . consult-ripgrep))
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
          (apply (if (bound-and-true-p vertico-mode)
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  (add-to-list 'project-switch-commands '(consult-ripgrep "Ripgrep"))
  (setq project-switch-commands
        (cl-remove 'project-find-regexp project-switch-commands :key #'car)))

(with-eval-after-load 'consult-imenu
  (require 'cl-lib)
  (when-let ((config (alist-get 'emacs-lisp-mode consult-imenu-config)))
    (let ((types (plist-get config :types)))
      (cl-pushnew '(?l "LLM Tools" font-lock-function-name-face) types
                  :test (lambda (a b) (eq (car a) (car b))))
      (setf (plist-get config :types) types))))

(defun my-consult-ripgrep--advice-around (orig-fun &rest args)
  "Around advice for `consult-ripgrep'.
If interactively called with C-0 (which results in the DIR
argument being the integer 0), change DIR to `default-directory`.
Otherwise, call ORIG-FUN with the original ARGS."
  (let ((dir (car args))
        (initial (cadr args)))
    (if (equal dir 0)
        (apply orig-fun default-directory initial nil)
      (apply orig-fun args))))

(advice-add 'consult-ripgrep :around #'my-consult-ripgrep--advice-around)

(defun consult-ripgrep-org-notes ()
  "Search through org note files with ripgrep."
  (interactive)
  (let ((consult-ripgrep-args (concat consult-ripgrep-args " -C3")))
    (consult--grep "Ripgrep files"
                   #'consult--ripgrep-make-builder
                   (directory-files "~/doit/" 'full "^log.*\\.org$")
                   nil)))

(defun consult-ripgrep-todo-notes ()
  "Search through org note files with ripgrep."
  (interactive)
  (consult--grep "Ripgrep files"
                 #'consult--ripgrep-make-builder
                 '("~/doit/todo.org" "~/doit/projects.org" "~/.emacs.d/diary")
                 nil))

(defun consult-ripgrep-config-files ()
  "Search through configuration files with ripgrep."
  (interactive)
  (consult-ripgrep-files '("~/.emacs.d/init.el" "~/.emacs.d/init-personal.el")))

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
  :ensure (:host github :repo "ubolonton/info-colors" :inherit nil)
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
  :repo-scan
  :after latex
  :ensure (:host github :repo "ultronozm/outline-skip.el"
                 :depth nil
                 :inherit nil :pin t)
  :hook (LaTeX-mode . outline-skip-mode))

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

(use-package rust-mode
  :defer t
  :hook
  (rust-mode . eglot-ensure))

(use-package xr
  :defer t
  :ensure (:host github :repo "mattiase/xr"))

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

(use-package emacs-src-redirect
  :repo-scan
  :defer 1
  :ensure (:host github :repo "ultronozm/emacs-src-redirect.el" :depth nil
                 :inherit nil :pin t)
  :config
  (emacs-src-redirect-mode))

(defun isearch-forward-enclosing-defun ()
  "Start an incremental search for the name of the enclosing defun."
  (interactive)
  (end-of-defun)
  (beginning-of-defun)
  (down-list)
  (forward-sexp 2)
  (isearch-forward-symbol-at-point))

(keymap-global-set "M-s q" #'isearch-forward-enclosing-defun)

(defun czm-xref-restrict-to-project-advice (orig-fun &rest args)
  "Advice to restrict xref searches to the current project root."
  (let ((project-vc-external-roots-function #'ignore))
    (apply orig-fun args)))

(define-minor-mode czm-xref-project-only-mode
  "Toggle xref searches between project-only and including external roots."
  :global t
  :lighter nil
  (if czm-xref-project-only-mode
      (advice-add 'xref-find-references :around #'czm-xref-restrict-to-project-advice)
    (advice-remove 'xref-find-references #'czm-xref-restrict-to-project-advice)))

(czm-xref-project-only-mode)

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
               ("l" . flymake-show-buffer-diagnostics))
  :config
  (with-eval-after-load 'preview
    (dolist (cmd '(flymake-goto-next-error flymake-goto-prev-error))
      (add-to-list 'preview-auto-reveal-commands cmd)))
  (with-eval-after-load 'tex-fold
    (dolist (cmd '(flymake-goto-next-error flymake-goto-prev-error))
      (add-to-list 'TeX-fold-auto-reveal-commands cmd))))

(defun flymake-preserve-multiline--fix-entries (orig-fun diags project-root)
  "Show complete diagnostic messages in Flymake's diagnostic buffer.
Advice for `flymake--tabulated-entries-1' that prevents truncation
of multiline diagnostic messages."
  (cl-letf (((symbol-function 'flymake-diagnostic-oneliner)
             (lambda (diag &optional _) (flymake-diagnostic-text diag))))
    (funcall orig-fun diags project-root)))

(define-minor-mode flymake-preserve-multiline-mode
  "Show full multiline messages in Flymake diagnostics buffer.
When enabled, the Flymake diagnostics buffer will show complete
diagnostic messages, including line breaks, instead of truncating
them at the first newline."
  :global t
  :group 'flymake
  (if flymake-preserve-multiline-mode
      (advice-add 'flymake--tabulated-entries-1 :around
                  #'flymake-preserve-multiline--fix-entries)
    (advice-remove 'flymake--tabulated-entries-1
                   #'flymake-preserve-multiline--fix-entries)))

(use-package attrap
  :repo-scan
  :ensure (:host github
                 :repo "ultronozm/attrap.el"
                 :remotes (("upstream" :repo "jyp/attrap"))
                 :depth nil
                 :inherit nil
                 :pin t)
  :defer t
  :after flycheck
  :config
  (setq saved-match-data nil)
  (define-key flycheck-command-map "f" 'attrap-flycheck)
  (add-to-list 'attrap-flymake-backends-alist
               '(eglot-flymake-backend . attrap-python-fixer)))

(use-package treesit-auto
  :defer t
  :custom
  (treesit-auto-install 'prompt)
  (global-treesit-auto-modes
   '(yaml-mode yaml-ts-mode wgsl-mode wgsl-ts-mode wat-mode wat-ts-mode
               wat-mode wat-ts-wast-mode vue-mode vue-ts-mode vhdl-mode
               vhdl-ts-mode verilog-mode verilog-ts-mode typst-mode
               typst-ts-mode typescript-mode typescript-ts-mode
               typescript-tsx-mode tsx-ts-mode toml-mode conf-toml-mode
               toml-ts-mode surface-mode surface-ts-mode sql-mode
               sql-ts-mode scala-mode scala-ts-mode rust-mode rust-ts-mode
               ruby-mode ruby-ts-mode ess-mode r-ts-mode python-mode
               python-ts-mode protobuf-mode protobuf-ts-mode perl-mode
               perl-ts-mode org-mode org-ts-mode nushell-mode
               nushell-ts-mode nix-mode nix-ts-mode markdown-mode
               poly-markdown-mode markdown-ts-mode makefile-mode
               makefile-ts-mode magik-mode magik-ts-mode lua-mode
               lua-ts-mode latex-mode latex-ts-mode kotlin-mode
               kotlin-ts-mode julia-mode julia-ts-mode js-json-mode
               json-ts-mode js2-mode javascript-mode js-mode js-ts-mode
               java-mode java-ts-mode janet-mode janet-ts-mode sgml-mode
               mhtml-mode html-ts-mode heex-mode heex-ts-mode go-mod-mode
               go-mod-ts-mode go-mode go-ts-mode glsl-mode glsl-ts-mode
               elixir-mode elixir-ts-mode dockerfile-mode
               dockerfile-ts-mode dart-mode dart-ts-mode css-mode
               css-ts-mode common-lisp-mode
               commonlisp-ts-mode cmake-mode cmake-ts-mode clojurec-mode
               clojurescript-mode clojure-mode clojure-ts-mode csharp-mode
               csharp-ts-mode blueprint-mode
               blueprint-ts-mode bibtex-mode bibtex-ts-mode sh-mode
               bash-ts-mode awk-mode awk-ts-mode))
  (treesit-auto-langs
   '(awk bash bibtex blueprint c-sharp clojure cmake commonlisp css dart
         dockerfile elixir glsl go gomod heex html janet java javascript
         json julia kotlin latex lua magik make markdown nix nu org perl
         proto python r ruby rust scala sql surface toml tsx typescript
         typst verilog vhdl vue wast wat wgsl yaml))
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package eglot
  :bind
  (:map eglot-mode-map
        ("C-c C-q" . eglot-code-action-quickfix)
        ("C-c C-a" . eglot-code-actions)))

(use-package consult-abbrev
  :repo-scan
  :defer t
  :ensure (:host github :repo "ultronozm/consult-abbrev.el" :depth nil
                 :inherit nil :pin t))

(defun foldout-exit-fold-without-hiding ()
  (interactive)
  (foldout-exit-fold -1))

(use-package czm-spell
  :repo-scan
  :defer 10
  :ensure (:host github :repo "ultronozm/czm-spell.el" :depth nil
                 :inherit nil :pin t)
  ;; :after latex
  :bind ("s-;" . czm-spell-then-abbrev))

(use-package typescript-mode
  :ensure t
  :defer t)

(use-package julia-ts-mode
  :ensure (:host github :repo "dhanak/julia-ts-mode"
                 :depth nil
                 :inherit nil)
  :mode "\\.jl$")

(use-package eglot-jl
  :defer t
  :config
  (eglot-jl-init))

(use-package nerd-icons
  :defer t
  :ensure t)

(use-package nerd-icons-completion
  :defer t
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :defer t
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; (use-package markdown-ts-mode)
(add-to-list 'major-mode-remap-defaults '(markdown-mode))

(defun project-claude-code ()
  "Start vterm in the current project's root and run the `claude' program."
  (interactive)
  (let* ((pr (project-current t))
         (default-directory (project-root pr))
         (default-project-shell-name (project-prefixed-buffer-name "claude")))
    (if (get-buffer default-project-shell-name)
        (pop-to-buffer default-project-shell-name (append display-buffer--same-window-action
                                                          '((category . comint))))
      (dlet ((vterm-buffer-name default-project-shell-name))
        (vterm)
        (vterm-send-string "claude\n")))))

(use-package vterm
  :bind (:map project-prefix-map
              ("l" . project-claude-code))
  :config
  (add-to-list 'project-switch-commands
               '(project-claude-code "Claude Code" nil)))

(use-package debbugs
  :defer t
  :ensure (debbugs
           :host github :repo "emacs-mirror/debbugs"
           :branch "externals/debbugs"
           :source "GNU ELPA"
           :files (:defaults (:exclude ".git" "dir") "Debbugs.wsdl")
           :inherit nil)
  :custom
  (debbugs-gnu-mail-backend 'rmail)
  (debbugs-cache-expiry nil))

(defun my/disable-vertico-for-debbugs-search (orig-fun &rest args)
  "Temporarily disable Vertico while executing `debbugs-gnu-search'."
  (let ((old-completing-read-function completing-read-function)
        (old-completion-in-region-function completion-in-region-function))
    (unwind-protect
        (progn
          (setq completing-read-function #'completing-read-default
                completion-in-region-function #'completion--in-region)
          (apply orig-fun args))
      (setq completing-read-function old-completing-read-function
            completion-in-region-function old-completion-in-region-function))))

(advice-add 'debbugs-gnu-search :around #'my/disable-vertico-for-debbugs-search)

;;; pdf

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :ensure (:host github :repo "vedang/pdf-tools"
                 :depth nil
                 :remotes (("orgtre" :repo "orgtre/pdf-tools"))
                 :inherit nil :pin t)
  :custom
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (global-auto-revert-ignore-modes '(pdf-view-mode))
  (pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
  (pdf-annot-tweak-tooltips nil)
  :bind
  (:map pdf-view-mode-map
        ("j" . pdf-view-jump-to-register)
        ("y" . image-previous-line)
        ("<down>" . nil)
        ("<up>" . nil)
        ("<remap> <scroll-up-command>" . pdf-view-scroll-up-or-next-page)
        ("<remap> <scroll-down-command>" . pdf-view-scroll-down-or-previous-page)
        ("C-c g" . pdf-view-goto-page)
        ("s-t" . pdf-view-jump-to-register)
        ("s-T" . pdf-view-position-to-register))
  :config
  (pdf-tools-install :no-query)
  (require 'pdf-occur)
  (add-to-list 'display-buffer-alist
               '("\\*Edit Annotation .*\\.pdf\\*"
                 (display-buffer-reuse-mode-window
                  display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.5)
                 (reusable-frames . visible))))

(defun my/pdf-annot-setup (_a)
  (LaTeX-mode)
  (setq TeX-master my-preview-master)
  (preview-auto-mode))

(setq pdf-annot-edit-contents-setup-function #'my/pdf-annot-setup)

(use-package doc-view-follow
  :repo-scan
  :defer t
  :ensure (:host github :repo "ultronozm/doc-view-follow.el" :depth nil
                 :inherit nil :pin t)
  :custom (doc-view-follow-hijack t))

(use-package pdf-extract
  :defer t
  :repo-scan
  :ensure (:host github :repo "ultronozm/pdf-extract.el"
                 :inherit nil :pin t))

(use-package pdf-tools-org-extract
  :repo-scan
  :after pdf-annot
  :demand
  :ensure (:host github :repo "ultronozm/pdf-tools-org-extract.el"
                 :inherit nil :pin t)
  :bind (:map pdf-view-mode-map
              ("C-c C-a e" . pdf-tools-org-extract-annotations)))

(define-minor-mode global-pdf-view-midnight-minor-mode
  "Toggle PDF-View-Midnight mode in all PDF buffers.
When enabled, automatically turns on `pdf-view-midnight-minor-mode'
in all current and future PDF buffers."
  :global t
  :init-value nil
  (let ((process-pdf-buffers
         (lambda (enable)
           (dolist (buf (buffer-list))
             (with-current-buffer buf
               (when (derived-mode-p 'pdf-view-mode)
                 (pdf-view-midnight-minor-mode (if enable 1 -1))))))))
    (if global-pdf-view-midnight-minor-mode
        (progn
          (funcall process-pdf-buffers t)
          (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode))
      (funcall process-pdf-buffers nil)
      (remove-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode))))

;;; translation

(use-package go-translate
  :defer t
  :config
  (setq gt-langs '(da en fr de))
  ;; (setq gt-default-translator (gt-translator :engines (gt-google-engine)))
  (setq gt-default-translator 
        (gt-translator :engines (gt-google-engine)))
  (setq gt-default-translator 
        (gt-translator 
         :taker (gt-taker :langs '(en da) :text 'paragraph)  ;; Set up for paragraph translation
         :engines (gt-google-engine)                         ;; Google engine has good TTS support
         :render (gt-buffer-render)))
  )

(defun gt-eldoc-documentation-function (callback)
  "Return translation at point via eldoc's callback mechanism."
  (condition-case nil
      (let ((translator (clone gt-default-translator)))
        (oset translator render
              (lambda (fn)
                (let (result)
                  (funcall fn
                           (lambda (s) (setq result s)))
                  (when (and callback result)
                    (funcall callback result
                             :face 'font-lock-doc-face)))))
        (gt-start translator))
    (error nil))
  nil)

;;;###autoload
(define-minor-mode gt-eldoc-mode
  "Minor mode to show translations via eldoc."
  :lighter " GT-ElDoc"
  (if gt-eldoc-mode
      (add-hook 'eldoc-documentation-functions #'gt-eldoc-documentation-function nil t)
    (remove-hook 'eldoc-documentation-functions #'gt-eldoc-documentation-function t)))

;;; org

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

(defvar my/last-src-language "elisp"
  "The most recently used language in source blocks.")

(defun my/org-escape-org-elements (text)
  "Escape org syntax elements in TEXT.
Prevents asterisks at the beginning of a line from becoming headings
and protects #+begin/#+end blocks from being interpreted as org syntax."
  (let ((processed-text text))
    ;; Escape headings (lines starting with asterisks)
    (setq processed-text (replace-regexp-in-string "^\\(\\*+\\)" ",\\1" processed-text))
    ;; Escape #+begin_ and #+end_ blocks
    (setq processed-text (replace-regexp-in-string "^\\(#\\+begin_\\)" ",\\1" processed-text))
    (setq processed-text (replace-regexp-in-string "^\\(#\\+end_\\)" ",\\1" processed-text))
    processed-text))

(defun my/org-insert-block-with-yank (block-type &optional language)
  "Insert an org block of BLOCK-TYPE with yanked content.
If LANGUAGE is provided, use it for source blocks.
Automatically clean up extra newlines at boundaries and escape org syntax."
  (push-mark)
  (let* ((raw-content (string-trim (current-kill 0)))
         (escaped-content (my/org-escape-org-elements raw-content)))
    (if (equal block-type "src")
        (progn
          (setq my/last-src-language language)
          (insert (format "#+begin_src %s\n%s\n#+end_src" language escaped-content)))
      (insert (format "#+begin_%s\n%s\n#+end_%s" block-type escaped-content block-type)))
    (org-edit-special)
    (org-edit-src-exit)
    (forward-line 2)
    (newline)))

(defun my/org-insert-example-with-yank ()
  "Insert an example block with yanked content.
The content is escaped to prevent org syntax interpretation."
  (interactive)
  (my/org-insert-block-with-yank "example"))

(defun my/org-insert-src-with-yank (language)
  "Insert a source block with yanked content and specified LANGUAGE.
The content is escaped to prevent org syntax interpretation."
  (interactive
   (list (read-string (format "Language (%s): " my/last-src-language)
                      nil nil my/last-src-language)))
  (my/org-insert-block-with-yank "src" language))


(defun czm-org-preview-setup ()
  "Set up org-mode buffer for use with preview-auto-mode."
  (setq-local TeX-master my-preview-master)
  (setq-local preview-tailor-local-multiplier 0.7))

(defvar my/org-tex-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'dynexp-space)
    (define-key map (kbd "TAB") 'dynexp-next)
    map)
  "Keymap for my/org-tex-mode.")

(define-minor-mode my/org-tex-mode
  "Minor mode for using LaTeX abbrevs in other major modes."
  :lighter " LaTeXAbbrev"
  :keymap my/org-tex-mode-map
  (if my/org-tex-mode
      ;; When enabling the mode
      (when (boundp 'LaTeX-mode-abbrev-table)
        (setq local-abbrev-table LaTeX-mode-abbrev-table))
    ;; When disabling the mode
    (setq local-abbrev-table
          (symbol-value (derived-mode-abbrev-table-name major-mode)))))

(use-package org
  :ensure nil
  :hook
  (org-mode . visual-line-mode)
  (org-mode . (lambda () (setq fill-column 999999)))
  (org-mode . abbrev-mode)
  (org-mode . czm-org-preview-setup)
  :custom
  (org-hide-emphasis-markers t)
  (org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((agenda "")
       (todo "TODO")
       (tags "CLOSED>=\"<today>\""
             ((org-agenda-overriding-header "\nCompleted today\n")))))
     ("y" "Year view"
      ((agenda ""
               ((org-agenda-files (list my-projects-file))
                (org-agenda-span 365)
                (org-agenda-start-on-weekday nil)
                (org-agenda-start-day (format-time-string "%Y-%m-%d"))
                (org-agenda-prefix-format
                 '((agenda . "  %-12:c%?-12t%6e  %s")))
                (org-agenda-show-all-dates nil)
                (diary-show-holidays-flag nil)
                (org-agenda-include-diary t))))
      ((org-agenda-skip-function
        '(org-agenda-skip-entry-if 'todo 'done))))))
  (org-default-notes-file my-todo-file)
  (org-directory "~/")
  (org-agenda-files `(,my-todo-file ,my-projects-file))
  (org-goto-auto-isearch nil)
  (org-agenda-include-diary t)
  (org-babel-load-languages '((latex . t) (emacs-lisp . t)
                              (python . t) (R . t)
                              (shell . t) (sql . t)
                              (sage . t)))
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
  (org-agenda-skip-deadline-if-done t)
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
        ("M-}" . org-forward-paragraph)
        ("C-c C-' e" . my/org-insert-example-with-yank)
        ("C-c C-' s" . my/org-insert-src-with-yank)
        ("H-o" . #'my/org-tex-mode))
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
  (:map org-mode-map
        ("C-c M-n" . org-forward-element)
        ("C-c M-p" . org-backward-element)
        :repeat-map org-element-repeat-map
        ("n" . org-forward-element)
        ("p" . org-backward-element)
        ("u" . org-up-element)
        ("g" . org-down-element)
        :continue-only
        ("h" . org-mark-element)
        ("t" . org-transpose-element)
        ("w" . kill-region)
        ("M-w" . kill-ring-save)
        ("y" . yank)
        ("C-/" . undo)
        ("f" . org-drag-element-forward)
        ("b" . org-drag-element-backward))
  :config
  (require 'ob-shell)
  (dolist (item '(("m" . org-babel-mark-block)
                  ("\C-m" . org-babel-mark-block)))
    (add-to-list 'org-babel-key-bindings item))
  (pcase-dolist (`(,key . ,def) org-babel-key-bindings)
    (define-key org-babel-map key def))
  (add-to-list 'org-speed-commands '("S" . my/org-schedule-and-refile) t)
  (add-to-list 'org-src-lang-modes '("lean" . lean4))
  (add-to-list 'org-src-lang-modes '("tex" . latex))
  (add-to-list 'org-src-lang-modes '("cmake" . cmake-ts))
  (add-to-list 'org-src-lang-modes '("yaml" . yaml-ts))
  )

(defun my/org-archive-done-tasks ()
  "Archive all done tasks in the current buffer."
  (interactive)
  (org-archive-all-matches
   (lambda (_beg _end)
     (looking-at
      (concat
       "^\\*+ "
       (regexp-opt '("DONE" "CANCELED")))))))

(use-package org-modern
  :defer t
  :ensure t
  :config
  (global-org-modern-mode 1))

(use-package org-appear
  :ensure t
  :hook
  (org-mode . org-appear-mode))

;;; mail

(defun mml-attach-buffer-or-file (buffer &optional type description disposition filename)
  "Attach BUFFER's underlying file if it has one. Otherwise attach BUFFER's contents.
TYPE, DESCRIPTION, and DISPOSITION are optional MIME properties.
If BUFFER is visiting a file and is modified, the user is prompted to save it.
For non–file buffers, FILENAME is prompted for and used as the suggested name."
  (interactive
   (let* ((buf (read-buffer "Attach from buffer: " (buffer-name (current-buffer)) t))
          (bufobj (get-buffer buf))
          (has-file (buffer-file-name bufobj))
          (no-prompt current-prefix-arg)
          (type (if no-prompt
                    (or (and has-file (mm-default-file-type has-file))
                        "application/octet-stream")
                  (mml-minibuffer-read-type
                   (or (and has-file (file-name-nondirectory has-file))
                       buf))))
          (desc (unless no-prompt
                  (mml-minibuffer-read-description)))
          (disp (if no-prompt
                    (if has-file
                        (mml-content-disposition type (file-name-nondirectory has-file))
                      "attachment")
                  (mml-minibuffer-read-disposition
                   type
                   nil
                   (and has-file (file-name-nondirectory has-file)))))
          (fname (unless has-file
                   (read-string "Filename for attachment: "
                                (concat buf ".txt")))))
     (if has-file
         (list buf type desc disp)
       (list buf type desc disp fname))))
  (let ((has-file (buffer-file-name (get-buffer buffer))))
    (if has-file
        (progn
          (with-current-buffer buffer
            (when (and (buffer-modified-p)
                       (y-or-n-p (format "Buffer `%s' is modified. Save before attaching? "
                                         buffer)))
              (save-buffer)))
          (mml-attach-file has-file type description disposition))
      (mml-attach-buffer buffer type description disposition filename))))

(defun my-compose-mail-with-attachments (&optional arg)
  "Prepare a message with current buffer as an attachment.
With prefix ARG, attach all visible buffers instead."
  (interactive "P")
  (let* ((buffers (if arg
                      (seq-uniq (mapcar #'window-buffer (window-list)))
                    (list (current-buffer))))
         (count (length buffers)))
    (compose-mail)
    (save-excursion
      (rfc822-goto-eoh)
      (forward-line)
      (open-line 2)
      (dolist (buffer buffers)
        (mml-attach-buffer-or-file buffer)))))

(bind-keys
 :package message
 :map message-mode-map
 ("C-c RET a" . mml-attach-buffer-or-file))

(defun my-rmail-mode-hook ()
  (setq-local preview-tailor-local-multiplier 0.6)
  (setq-local TeX-master my-preview-master))

(use-package rmail
  :ensure nil
  :defer t
  :bind
  ("C-z r" . (lambda ()
               (interactive)
               (let ((current-prefix-arg '(4)))
                 (call-interactively #'rmail))))
  ("C-z R" . rmail)
  (:map rmail-mode-map
        ("S" . czm-mail-refile-and-store-link))
  :hook (rmail-mode . my-rmail-mode-hook)
  :custom
  (rmail-mime-attachment-dirs-alist `((".*" ,my-downloads-folder)))
  (rmail-file-name (expand-file-name "inbox.rmail" my-mail-folder))
  (rmail-movemail-program "movemail")
  (rmail-primary-inbox-list (list my-mail-inbox))
  (rmail-automatic-folder-directives
   `((,(expand-file-name "bug-gnu-emacs.rmail" my-mail-folder)
      "sender" "bug-gnu-emacs-bounces")
     (,(expand-file-name "emacs-devel.rmail" my-mail-folder)
      "sender" "emacs-devel-bounces")
     (,(expand-file-name "arxiv.rmail" my-mail-folder)
      "subject" "math daily Subj-class mailing"
      "from" "arXiv\\.org")
     (,(expand-file-name "receipts.rmail" my-mail-folder)
      "from" "noreply@github.com"
      "subject" "Payment Receipt")
     (,(expand-file-name "receipts.rmail" my-mail-folder)
      "from" "invoice+statements@mail.anthropic.com"
      "subject" "Your receipt from Anthropic")
     (,(expand-file-name "receipts.rmail" my-mail-folder)
      "from" "googleplay-noreply@google.com"
      "subject" "Your Google Play Order Receipt")))
  (rmail-secondary-file-directory (file-name-as-directory my-mail-folder))
  (rmail-secondary-file-regexp "^.*\\.rmail$")
  (rmail-default-file (expand-file-name "scheduled.rmail" my-mail-folder))
  (rmail-remote-password-required t)
  (rmail-remote-password
   (let ((auth-info (car (auth-source-search
                          :host my-mail-host-imap
                          :port my-mail-port
                          :user my-mail-user
                          :max 1))))
     (when auth-info
       (let ((secret (plist-get auth-info :secret)))
         (if (functionp secret)
             (funcall secret)
           secret)))))
  (rmail-displayed-headers "^\\(?:Cc\\|Date\\|From\\|Subject\\|To\\|Sender\\):")
  (rmail-delete-after-output t)
  :config
  (add-to-list 'auto-mode-alist '("\\.rmail$" . rmail-mode)))

(defun my-always-enable-rmail-font-lock (&rest _)
  "Ensure font-lock-mode is enabled after rmail runs."
  ;; Check we're actually in rmail-mode, in case rmail errored out.
  (when (derived-mode-p 'rmail-mode)
    (font-lock-mode 1)))

(advice-add 'rmail :after #'my-always-enable-rmail-font-lock)

(use-package sendmail
  :ensure nil
  :defer t
  :config
  (setq
   mail-host-address my-mail-host
   sendmail-program "msmtp"
   message-send-mail-function 'message-send-mail-with-sendmail
   message-default-mail-headers
   (let ((file (abbreviate-file-name
                (expand-file-name "sent.rmail" my-mail-folder))))
     (format "Fcc: %s\n" file))))

(use-package message
  :ensure nil
  :mode ("\\*message\\*-[0-9]\\{8\\}-[0-9]\\{6\\}\\'" . message-mode)
  :custom
  (message-make-forward-subject-function #'message-forward-subject-fwd))

(defun czm-mail-message-tab ()
  "Use `mail-abbrev-insert-alias' in headers, otherwise `message-tab'."
  (interactive)
  (if (message-point-in-header-p)
      (call-interactively #'mail-abbrev-insert-alias)
    (message-tab)))

(use-package message
  :defer t
  :ensure nil
  :bind
  (:map message-mode-map
        ("TAB" . czm-mail-message-tab)))

(use-package mairix
  :ensure nil
  :defer t
  :bind
  ("C-z m" . mairix-transient-menu)
  :config
  (defun mairix-display-syntax-help ()
    "Display mairix search syntax help in a buffer."
    (interactive)
    (with-help-window "*Mairix Syntax Help*"
      (princ "Mairix Search Syntax:\n\n")
      (princ "word          : match word in message body and major headers\n")
      (princ "t:word        : match word in To: header\n")
      (princ "c:word        : match word in Cc: header\n")
      (princ "f:word        : match word in From: header\n")
      (princ "a:word        : match word in To:, Cc: or From: headers (address)\n")
      (princ "s:word        : match word in Subject: header\n")
      (princ "b:word        : match word in message body\n")
      (princ "m:word        : match word in Message-ID: header\n")
      (princ "n:word        : match word in name of attachment\n")
      (princ "F:flags       : match message flags (s=seen,r=replied,f=flagged,-=negate)\n")
      (princ "p:substring   : match substring of path\n")
      (princ "d:start-end   : match date range\n")
      (princ "z:low-high    : match messages in size range\n")
      (princ "\nAdvanced syntax:\n")
      (princ "bs:word       : match word in Subject: header or body\n")
      (princ "s:word1,word2 : match both words in Subject:\n")
      (princ "s:word1/word2 : match either word or both words in Subject:\n")
      (princ "s:~word       : match messages not containing word in Subject:\n")
      (princ "s:substring=  : match substring in any word in Subject:\n")
      (princ "s:^substring= : match left-anchored substring in any word in Subject:\n")
      (princ "s:substring=2 : match substring with <=2 errors in any word in Subject:\n")))

  (require 'transient)
  (transient-define-prefix
   mairix-transient-menu ()
   "Mairix search commands."
   :info-manual "(mairix-el)Top"
   ["Search"
    ("RET" "Search" mairix-search)
    ("w" "Widget search" mairix-widget-search)
    ("f" "Search by sender" mairix-search-from-this-article)
    ("t" "Search thread" mairix-search-thread-this-article)
    ("b" "Search based on article" mairix-widget-search-based-on-article)]
   ["Saved Searches"
    ("s" "Use saved search" mairix-use-saved-search)
    ("S" "Save last search" mairix-save-search)
    ("e" "Edit saved searches" mairix-edit-saved-searches)]
   ["Database"
    ("u" "Update database" mairix-update-database)]
   ["Help"
    ("?" "Show syntax help" mairix-display-syntax-help)
    ("i" "Info docs" (lambda ()
                       (interactive)
                       (info "(mairix-el)")))]))

(use-package czm-mail
  :repo-scan
  :after rmail
  :demand
  :ensure (:host github :repo "ultronozm/czm-mail.el" :depth nil
                 :inherit nil :pin t)
  :bind
  ("C-c C-@" . czm-mail-mailrc-add-entry)
  (:map rmail-mode-map
        ("S" . czm-mail-refile-and-store-link))
  (:map message-mode-map
        ("TAB" . czm-mail-message-tab)
        ("C-c C-a" . czm-mail-insert-diff-as-attachment))
  :custom
  (czm-mail-refile-file (expand-file-name "scheduled.rmail" my-mail-folder))
  :config
  (czm-mail-setup))

;;; ai stuff

(use-package copilot
  :ensure (:host github
                 :repo "zerolfx/copilot.el"
                 :files ("*.el" "dist")
                 :depth nil
                 :remotes (("ultronozm" :repo "ultronozm/copilot.el"))
                 :inherit nil
                 :pin t)
  :diminish " Co"
  :hook
  ((prog-mode LaTeX-mode git-commit-setup) . copilot-mode)
  (emacs-lisp-mode . (lambda () (setq tab-width 1)))
  (lean4-mode . (lambda () (setq tab-width 2)))
  (c++-mode . (lambda () (setq tab-width 4)))
  (rust-mode . (lambda () (setq tab-width 4)))
  ;; (sage-shell:sage-mode . (lambda () (setq tab-width 4)))
  (python-mode . (lambda () (setq tab-width 4)))
  (sage-mode . (lambda () (setq tab-width 4)))
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

(use-package plz
  :defer t
  :ensure (:host github :repo "alphapapa/plz.el"
                 :depth nil
                 :inherit nil))

(use-package plz-event-source
  :defer t
  :ensure (:host github :repo "r0man/plz-event-source"
                 :depth nil
                 :inherit nil))


(use-package llm
  :defer t
  :ensure (:host github
                 :repo "ahyatt/llm"
                 :depth nil
                 :remotes (("ultronozm" :repo "ultronozm/llm"))
                 :inherit nil
                 :pin t)
  ;; :init
  ;; (require 'llm-openai)
  ;; (require 'llm-gemini)
  ;; (require 'llm-ollama)
  :custom
  (llm-warn-on-nonfree nil)
  (llm-log t)
  :config
  (add-to-list 'warning-suppress-types '(llm)))

(use-package llm-tool-collection
  :defer t
  :after llm
  :ensure (:host github
                 :repo "skissue/llm-tool-collection"
                 :depth nil
                 :remotes (("ultronozm" :repo "ultronozm/llm-tool-collection"))
                 :inherit nil
                 :pin t))

(use-package ai-org-chat
  :repo-scan
  :ensure (:host github :repo "ultronozm/ai-org-chat.el"
                 :inherit nil
                 :pin t
                 :depth nil)
  :defer t
  :bind
  (:map global-map
        ("s-/" . ai-org-chat-new)
        ("s-<return>" . ai-org-chat-simple-respond))
  (:map project-prefix-map
        ("a" . ai-org-chat-project))
  (:map ai-org-chat-minor-mode-map
        ("s-<return>" . ai-org-chat-respond)
        ("C-c n" . ai-org-chat-branch)
        ("C-c e" . ai-org-chat-compare))
  :custom
  (ai-org-chat-user-name my-first-name)
  (ai-org-chat-dir my-scratch-gpt-dir)
  (ai-org-chat-content-wrapper #'ai-org-chat--wrap-xml)
  :config
  (add-hook 'llm-tool-collection-post-define-functions #'ai-org-chat-register-tool-spec)
  (mapcar #'ai-org-chat-register-tool-spec (llm-tool-collection-get-all))
  (require 'exec-path-from-shell)
  (ai-org-chat-select-model "sonnet 4")
  (add-hook 'ai-org-chat-response-finished-functions
            #'ai-org-chat-auto-format-response
            t)
  (add-to-list 'display-buffer-alist
               '("\\*ai-chat\\.org\\*\\|--ai-chat\\.org"
                 (display-buffer-same-window)
                 (inhibit-same-window . nil))))

;; I use the following functions to provide context to ai-org-chat

(defun my/agenda-for-today ()
  "Return string containing agenda for today."
  (interactive)
  (save-window-excursion
    (require 'org-agenda)
    (let ((org-agenda-span 'day)
          (org-agenda-prefix-format
           '((agenda . "  %-12:c%?-12t%6e  %s"))))
      (org-agenda nil "a")
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun my/current-date-and-time ()
  "Return string describing current date and time."
  (format-time-string "%A, %B %d, %Y at %I:%M %p"))

(defun my/agenda-for-week ()
  "Return string containing full agenda for the next seven days."
  (interactive)
  (save-window-excursion
    (require 'org-agenda)
    (let ((org-agenda-span 'day)
          (org-agenda-start-on-weekday nil) ; start from today regardless of weekday
          (org-agenda-start-day (format-time-string "%Y-%m-%d"))
          (org-agenda-ndays 7)
          (org-agenda-prefix-format
           '((agenda . "  %-12:c%?-12t%6e  %s"))))
      (org-agenda nil "a")
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun my/filter-diary-contents ()
  "Return diary contents without holiday entries."
  (with-temp-buffer
    (insert-file-contents diary-file)
    (goto-char (point-min))
    (keep-lines "^[^&]" (point-min) (point-max))
    (buffer-string)))

(defun my/with-filtered-diary (fn)
  "Execute FN with a filtered version of the diary.
Temporarily creates and uses a diary file without holiday entries."
  (let ((filtered-contents (my/filter-diary-contents)))
    (with-temp-file "/tmp/temp-diary"
      (insert filtered-contents))
    (let ((diary-file "/tmp/temp-diary"))
      (funcall fn))))

(defun my/projects-for-year ()
  "Return string containing projects.org agenda for next year.
Skips empty days and diary holidays."
  (interactive)
  (save-window-excursion
    (require 'org-agenda)
    (let ((org-agenda-files (list my-projects-file))
          (org-agenda-span 365)
          (org-agenda-start-on-weekday nil)
          (org-agenda-start-day (format-time-string "%Y-%m-%d"))
          (org-agenda-prefix-format
           '((agenda . "  %-12:c%?-12t%6e  %s")))
          (org-agenda-include-diary t)
          (diary-show-holidays-flag nil)
          (org-agenda-show-all-dates nil))
      (my/with-filtered-diary
       (lambda ()
         (org-agenda nil "a")
         (buffer-substring-no-properties (point-min) (point-max)))))))

(use-package content-quoter
  :repo-scan
  :ensure (:host github :repo "ultronozm/content-quoter.el"
                 :depth nil
                 :inherit nil :pin t)
  :bind ("s-u" . content-quoter-dwim)
  :defer t)

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
         (filename-tool
          (llm-make-tool
           :function (lambda (callback suggested-name)
                       (funcall callback suggested-name))
           :name "suggest_filename"
           :description "Suggest a better filename for the current file."
           :args '((:name "suggested_name"
                          :description "The suggested new filename."
                          :type string
                          :required t))
           :async t))
         (final-cb
          (lambda (response)
            (with-current-buffer buffer
              (if-let* ((suggestion-pair (and (listp response)
                                              (car response)))
                        (suggested-name (and (equal (car suggestion-pair)
                                                    "suggest_filename")
                                             (cdr suggestion-pair))))
                  (let* ((current-dir (file-name-directory (buffer-file-name)))
                         (new-path (read-file-name "Rename file to: "
                                                   current-dir
                                                   nil
                                                   nil
                                                   suggested-name)))
                    (when (y-or-n-p (format "Rename '%s' to '%s'? "
                                            (buffer-file-name)
                                            new-path))
                      (require 'dired-aux)
                      (dired-rename-file (buffer-file-name) new-path 1)
                      (message "File renamed to '%s'" (file-name-nondirectory new-path))))
                (user-error "Failed to get a valid filename suggestion from LLM")))))
         (error-cb
          (lambda (err msg)
            (message "Error: %s - %s" err msg))))

    (llm-chat-async ai-org-chat-provider
                    (llm-make-chat-prompt
                     prompt
                     :tools (list filename-tool))
                    final-cb
                    error-cb)))




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
    ;; (statement-cont . llvm-lineup-statement) ; Guessed value
    (statement-cont . ++)                ; Guessed value
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
    (innamespace . 0)
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
  (:map c-mode-base-map
        ("C-c C-c" . compile)
        ("C-c C-r" . recompile))
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
         ("s-m c" . cmake-build-clean)
         ("s-m i" . cmake-build-open-project-data))
  :custom
  (cmake-build-override-compile-keymap nil)
  (cmake-build-export-compile-commands t)
  (cmake-build-options "-j 1")
  (cmake-build-options "-j 2")
  (cmake-build-options "-j 16")
  (cmake-build-options "-j 8 --verbose"))

(use-package czm-cpp
  :repo-scan
  :ensure (:host github :repo "ultronozm/czm-cpp.el" :files ("*.el" "template") :depth nil
                 :inherit nil :pin t)
  :defer t
  :custom
  (czm-cpp-scratch-directory my-scratch-cpp-dir))

(add-to-list 'auto-mode-alist '("\\.ixx\\'" . c++-mode))

(use-package c-ts-mode
  :disabled
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
  ;; (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  ;; (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
  :config
  (require 'treesit-auto)
  (setq c-ts-mode-indent-offset 2)
  (setq c-ts-mode-indent-style #'my--c-ts-indent-style))

(use-package cmake-ts-mode
  :ensure nil
  :defer t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :config
  (require 'treesit-auto))

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

(setq auth-sources '("~/.authinfo.gpg"))

(use-package forge
  :ensure
  :after magit)

(defun czm-file-is-tex-or-bib (file)
  "Return t if FILE is a .tex or .bib file."
  (or (string-suffix-p ".tex" file)
      (string-suffix-p ".bib" file)))

(use-package publish
  :repo-scan
  :ensure (:host github :repo "ultronozm/publish.el" :depth nil)
  :defer t
  :custom
  (publish-repo-root my-publish-math-repo)
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

(use-package llm-vc-commit
  :repo-scan
  :ensure (:host github :repo "ultronozm/llm-vc-commit.el" :depth nil)
  :after log-edit
  :bind (:map log-edit-mode-map
              ("C-c C-r" . llm-vc-commit-generate-message))
  :config
  (setq llm-vc-commit-contribute-file
        (expand-file-name "CONTRIBUTE" "~/gnu-emacs/"))
  (require 'llm-claude)
  (require 'content-quoter)
  (setq llm-vc-commit-model
        (make-llm-claude
         :key (exec-path-from-shell-getenv "ANTHROPIC_KEY")
         :chat-model "claude-sonnet-4-20250514")))

(use-package diff-hl
  :defer t
  :bind ("H-d" . diff-hl-mode))

(use-package repo-scan
  :repo-scan
  :ensure (:host github :repo "ultronozm/repo-scan.el" :depth nil)
  :defer t)

;;; latex

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

(defun my-TeX-view-master ()
  "View the entire TeX document, ignoring any active region.
Unlike `TeX-view', this command always views the master file,
even if you've recently performed operations on a region.
This is useful when you want to ensure you're viewing the
complete document rather than just a previewed region."
  (interactive)
  (let ((TeX-current-process-region-p nil))
    (call-interactively #'TeX-view)))

(use-package latex
  :ensure (auctex
           :host nil
           :repo "git@git.savannah.gnu.org:/srv/git/auctex.git"
           :depth nil
           :inherit nil
           :pin t
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
  (advice-add 'text-scale-adjust :after (lambda (&rest _) (preview-clearout-buffer)))
  (advice-add 'global-text-scale-adjust :after (lambda (&rest _) (preview-clearout-buffer)))
  :hook
  (LaTeX-mode . my-LaTeX-mode-setup)
  (TeX-mode . prettify-symbols-mode)
  (prog-mode . (lambda () (setq-local TeX-master my-preview-master)))
  :bind
  (:map LaTeX-mode-map
        ("C-c m" . latex-math-from-calc)
        ("C-c C-g" . czm-latex-calc-grab)
        ("C-c C-n" . nil) ; TeX-normal-mode
        ("C-c #" . nil)
        ([remap next-error])
        ([remap previous-error])
        ("M-n" . next-error)
        ("M-p" . previous-error)
        ("C-c C-v" . my-TeX-view-master))
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

(defun czm-copy-standard-tex-files ()
  "Copy standard TeX files to the current directory."
  (interactive)
  ;; ask the user if he really wants to copy files into the current directory
  (if (y-or-n-p (format "Copy standard TeX files to %s? " default-directory))
      (let ((files (list my-common-tex-file my-master-bib-file)))
        (dolist (file files)
          (let ((source (expand-file-name file))
                (dest (expand-file-name (file-name-nondirectory file)
                                        default-directory)))
            (copy-file source dest t))))
    (message "Aborted.")))

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
  :ensure (:host github :repo "https://github.com/ultronozm/czm-tex-jump.el.git" :depth nil
                 :inherit nil :pin t)
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
      (when (looking-at "}")
        (forward-char))
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
  :ensure (:host github :repo "ultronozm/czm-tex-edit.el" :depth nil
                 :inherit nil :pin t)
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
        ("C-<return>" . czm-tex-edit-return)
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
   ("s-A" . tex-parens-kill-to-beginning-of-list)
   ("C-c p =" . tex-parens-increase-delimiter-size)
   ("C-c p -" . tex-parens-decrease-delimiter-size))
  (:repeat-map
   tex-parens-delimiter-size-repeat-map
   ("=" . tex-parens-increase-delimiter-size)
   ("-" . tex-parens-decrease-delimiter-size))
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
  :defer t
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

(defun my/setup-sage ()
  "Set up completion for Sage mode."
  (setq-local completion-styles '(basic))
  (setq-local corfu-sort-function 'nil)
  (corfu-mode)
  (setq-local orderless-component-separator (rx (or "_" ".")))
  (setq-local gud-pdb-command-name "sage -python -m pdb"))

(use-package sage
  :ensure (:host nil :repo "https://codeberg.org/ultronozm/sage-mode"
                 :remotes
                 (("upstream" :repo "https://codeberg.org/rahguzar/sage-mode"))
                 :depth nil
                 :main "sage.el"
                 :inherit nil)
  ;; :demand t
  :config
  (add-hook 'sage-mode-hook #'my/setup-sage)
  (add-hook 'sage-shell-mode-hook #'my/setup-sage)
  (add-to-list 'org-src-lang-modes '("sage" . sage))
  (sage-extend-semantic-python-filepatterns)
  :custom
  (sage-rich-output t))

(with-eval-after-load 'eglot
  (add-to-list
   'eglot-server-programs
   '((python-mode) . ("sage" "-python" "-m" "pylsp"))))

(defun my/sage-debug-current-file ()
  (interactive)
  (let ((gud-pdb-command-name
         (concat "sage -python -m pdb "
                 (shell-quote-argument
                  (file-relative-name (buffer-file-name))))))
    (call-interactively #'pdb)))

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
  :disabled
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
  (setq-local preview-tailor-local-multiplier 0.7)
  (setq-local TeX-master my-preview-master))

(use-package lean4-mode
  :repo-scan
  :ensure (:host github
                 :repo "ultronozm/lean4-mode"
                 :files ("*.el" "data")
                 :remotes (("bustercopley" :repo "bustercopley/lean4-mode")
                           ("leanprover-community" :repo "leanprover-community/lean4-mode"))
                 :inherit nil
                 :pin t)
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
  (add-to-list 'lean4-workspace-roots "~/.elan/toolchains/leanprover--lean4---v4.15.0-rc1/src/lean/")
  (font-lock-add-keywords 'lean4-mode '(("`\\<\\([^`]+\\)\\>`" 1 'font-lock-constant-face prepend)))
  :defer t)

(use-package czm-lean4
  :repo-scan
  :ensure (:host github :repo "ultronozm/czm-lean4.el" :depth nil
                 :inherit nil :pin t)
  :after lean4-mode
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
  :config
  (with-eval-after-load 'flymake
    (setopt flymake-overlays-fontify-text-function #'czm-lean4-maybe-colorize)
    (require 'flymake-overlays))
  (advice-add 'lean4-info-buffer-redisplay :around #'czm-lean4-info-buffer-redisplay)
  (advice-add 'lean4-info-buffer-redisplay :after #'czm-lean4--goal-overlay-update-adapter)
  (map-keymap
   (lambda (key cmd)
     (define-key lean4-mode-map (vector key) cmd))
   copilot-completion-map))

(use-package flymake-overlays
  :repo-scan
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

(setopt python-indent-guess-indent-offset nil)

(use-package clipdiff
  :repo-scan
  :defer t
  :ensure (:host github :repo "ultronozm/clipdiff.el" :depth nil))

(use-package cython-mode
  :ensure t
  :defer t)

