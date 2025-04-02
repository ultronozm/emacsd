;;; -*- lexical-binding: t; -*-

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

(keymap-global-set "C-c d" #'czm-dired-downloads)

(defun czm-find-math-document ()
  "Find a file in the math documents folder."
  (interactive)
  (require 'project)
  (project-find-file-in nil (list my-math-folder) `(local . ,my-math-folder)))

(keymap-global-set "s-d" #'czm-find-math-document)

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
  (add-to-list 'project-find-functions 'czm/project-try-local)
  (add-to-list 'project-switch-commands '(project-shell "Shell")))

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
                                (ediff-cleanup-mess)
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
             (string-prefix-p "FILE=" (buffer-name ediff-buffer-A)))
    (kill-buffer ediff-buffer-A)))

(add-hook 'ediff-before-setup-hook #'ediff-save-window-configuration)
(add-hook 'ediff-startup-hook #'ediff-make-configuration-local)
(add-hook 'ediff-quit-hook #'ediff-restore-window-configuration)
(add-hook 'ediff-cleanup-hook #'ediff-kill-temporary-file-buffer)

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

(bind-keys
 :package message
 :map message-mode-map
 ("C-c RET a" . mml-attach-buffer-or-file))

(defun project-format-patch-last-commit ()
  "Create a patch file from the last commit in the current project.
  The patch is saved in the project root directory and opened in a buffer."
  (interactive)
  (let* ((pr (project-current t))
         (root (project-root pr))
         (default-directory root)
         (git-output nil))
    
    ;; Make sure there's something to create a patch from
    (when (vc-git--empty-db-p)
      (user-error "No commits exist in this Git repository"))
    
    ;; Run format-patch to generate the patch
    (message "Generating patch...")
    (setq git-output 
          (with-temp-buffer
            (if (zerop (vc-git--call t "format-patch" "-1" "HEAD"))
                (buffer-string)
              (user-error "Failed to generate patch"))))
    
    ;; Extract filename from git output and open the file
    (let ((filename (string-trim git-output)))
      (find-file (expand-file-name filename root))
      (diff-mode)
      (message "Patch saved as %s" filename))))

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

(with-eval-after-load 'tex-mode
  (mapc
   (lambda (sym) (add-to-list 'tex--prettify-symbols-alist sym))
   '(
     ;; Accented characters
     ("{\\'a}" . ?á)
     ("{\\'e}" . ?é)
     ("{\\'i}" . ?í)
     ("{\\'o}" . ?ó)
     ("{\\'u}" . ?ú)
     ("{\\'A}" . ?Á)
     ("{\\'E}" . ?É)
     ("{\\'I}" . ?Í)
     ("{\\'O}" . ?Ó)
     ("{\\'U}" . ?Ú)
     ("{\\`a}" . ?à)
     ("{\\`e}" . ?è)
     ("{\\`i}" . ?ì)
     ("{\\`o}" . ?ò)
     ("{\\`u}" . ?ù)
     ("{\\`A}" . ?À)
     ("{\\`E}" . ?È)
     ("{\\`I}" . ?Ì)
     ("{\\`O}" . ?Ò)
     ("{\\`U}" . ?Ù)
     ("{\\^a}" . ?â)
     ("{\\^e}" . ?ê)
     ("{\\^i}" . ?î)
     ("{\\^o}" . ?ô)
     ("{\\^u}" . ?û)
     ("{\\^A}" . ?Â)
     ("{\\^E}" . ?Ê)
     ("{\\^I}" . ?Î)
     ("{\\^O}" . ?Ô)
     ("{\\^U}" . ?Û)
     ("{\\\"a}" . ?ä)
     ("{\\\"e}" . ?ë)
     ("{\\\"i}" . ?ï)
     ("{\\\"o}" . ?ö)
     ("{\\\"u}" . ?ü)
     ("{\\\"A}" . ?Ä)
     ("{\\\"E}" . ?Ë)
     ("{\\\"I}" . ?Ï)
     ("{\\\"O}" . ?Ö)
     ("{\\\"U}" . ?Ü)
     ("{\\~a}" . ?ã)
     ("{\\~n}" . ?ñ)
     ("{\\~o}" . ?õ)
     ("{\\~A}" . ?Ã)
     ("{\\~N}" . ?Ñ)
     ("{\\~O}" . ?Õ)
     ("{\\c{c}}" . ?ç)
     ("{\\c{C}}" . ?Ç)
     ("{\\o}" . ?ø)
     ("{\\O}" . ?Ø)
     ("{\\aa}" . ?å)
     ("{\\AA}" . ?Å)
     ("{\\ae}" . ?æ)
     ("{\\AE}" . ?Æ)
     ("{\\ss}" . ?ß)
     ("{\\l}" . ?ł)
     ("{\\L}" . ?Ł)
     ("{\\i}" . ?ı)
     ("{\\j}" . ?ȷ)
     ("{\\oe}" . ?œ)
     ("{\\OE}" . ?Œ)

     ("\\eps" . ?ε)
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
