;;; -*- lexical-binding: t; -*-

(setopt use-package-compute-statistics t)
(setopt use-package-verbose t)
(setopt use-package-minimum-reported-time 0.1)

;;; profile

(defconst cfg-profile
  (cond
   ((and (eq system-type 'gnu/linux) (not (display-graphic-p))) 'cfg-lean)
   ;; or match hostnames explicitly:
   ;; ((member (system-name) '("vm-hostname")) 'vm-lean)
   (t 'cfg-full)))

(defconst cfg-lean (eq cfg-profile 'cfg-lean))
(defconst cfg-full (eq cfg-profile 'cfg-full))

(defmacro use-package-full (name &rest args)
  (declare (indent defun))
  `(when cfg-full
     (use-package ,name ,@args)))

;; disable customization interface
(setq custom-file (locate-user-emacs-file "init-custom.el"))

(let ((file (locate-user-emacs-file "init-patches.el")))
  (when (file-exists-p file)
    (load file)))

(load (locate-user-emacs-file "init-settings.el"))

;;; optional settings defaults

(defvar my-todo-file nil
  "Primary todo Org file path, or nil when not configured.")

(defvar my-projects-file nil
  "Projects Org file path, or nil when not configured.")

(defvar my-log-file nil
  "Org log/journal file path, or nil when not configured.")

(defvar my-face-heights nil
  "Face height alist used by `czm-set-face-heights'.")

(defun my-setting-string (sym)
  "Return SYM value when it is a non-empty string, else nil."
  (let ((val (and (boundp sym) (symbol-value sym))))
    (and (stringp val) (> (length val) 0) val)))

(defun my-setting-files (&rest syms)
  "Return configured file paths from SYMS, omitting unset values."
  (delq nil (mapcar #'my-setting-string syms)))

(defun my/maybe-set-preview-master-local ()
  "Set local `TeX-master' from `my-preview-master' when enabled."
  (interactive)
  (when (bound-and-true-p my-preview-master)
    (setq-local TeX-master my-preview-master)))

(when (string-equal system-type "darwin")
  (setq ns-command-modifier nil)
  (setq ns-right-control-modifier 'super)
  (setq ns-function-modifier 'hyper)
  (setq ns-right-option-modifier 'super))

(when (string-equal system-type "windows-nt")
  (setq w32-lwindow-modifier 'super)
  (setq w32-apps-modifier 'hyper)
  (w32-register-hot-key [s-])
  (w32-register-hot-key [s]))

;;; core commands

(defun my-other-window-backward ()
  "Select the previous window."
  (interactive)
  (other-window -1))

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

(defun foldout-exit-fold-without-hiding ()
  (interactive)
  (foldout-exit-fold -1))

(when cfg-full
  (defun czm-find-math-document ()
    "Find a file in the math documents folder."
    (interactive)
    (require 'project)
    (let* ((root (file-name-as-directory (expand-file-name my-math-folder)))
           (project (or (project-current nil root)
                        (cons 'transient root))))
      (project-find-file-in nil (list root) project))))

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

(defun czm-create-scratch-markdown ()
  "Create new scratch markdown buffer."
  (interactive)
  (czm-create-scratch-file my-scratch-markdown-dir "md"))

(defun czm-create-scratch-tex ()
  "Create new scratch LaTeX buffer."
  (interactive)
  (czm-create-scratch-file my-scratch-tex-dir "tex"))

(defun czm-create-scratch-sage ()
  "Create new scratch sage file."
  (interactive)
  (czm-create-scratch-file my-scratch-sage-dir "sage"))

(defun czm-tmp-buffer-tex ()
  "Create new temporary LaTeX buffer."
  (interactive)
  (let ((buf (generate-new-buffer "*tmp-tex*")))
    (switch-to-buffer buf)
    (when (fboundp 'TeX-mode)
      (LaTeX-mode)
      (my/maybe-set-preview-master-local))))

(defvar czm--modus-vivendi-tinted-active nil)

(defun czm-set-face-heights ()
  "Set the heights of various faces."
  (pcase-dolist
      (`(,face . ,height)
       my-face-heights)
    (set-face-attribute face nil :height height)))

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

(when cfg-full
  (czm-set-face-heights))

(add-hook 'modus-themes-post-load-hook #'czm-set-face-heights)

(defun my/eval-expression-and-copy (exp &optional insert-value no-truncate char-print-limit copy-to-kill-ring)
  "Like `eval-expression', but with C-u C-u copies result to kill ring.
When called with a prefix argument of 16 (interactively, C-u C-u),
the result is copied to the kill ring in addition to being displayed.
All other prefix arguments work as in `eval-expression'."
  (interactive
   (let* ((prefix-arg current-prefix-arg)
          (copy-p (equal prefix-arg '(16)))
          ;; If copying, pass nil as prefix to eval-expression logic
          (current-prefix-arg (if copy-p nil prefix-arg))
          (args (cons (read--expression "Eval: ")
                      (eval-expression-get-print-arguments current-prefix-arg))))
     (append args (list copy-p))))
  
  (let ((result (eval-expression exp insert-value no-truncate char-print-limit)))
    (when copy-to-kill-ring
      (let ((result-str (with-temp-buffer
                          (prin1 result (current-buffer))
                          (buffer-string))))
        (kill-new result-str)
        (message "Result copied to kill ring: %s" result-str)))
    result))

(defun fill-previous-paragraph ()
  "Fill the previous paragraph."
  (interactive)
  (save-excursion
    (forward-line -1)
    (fill-paragraph)))

(defun ediff-current-kill ()
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

(defun diff-current-kill (&optional switches arg)
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
         (clip-file  (make-temp-file "diff-current-kill-clip-"))
         (src-file   (make-temp-file "diff-current-kill-src-"))
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
  "Compare the clipboard with the current buffer (or its active region).

No prefix ARG  →  call `ediff-current-kill` (side‑by‑side Ediff).

Any prefix ARG →  call `diff-current-kill`.
                  *One* C‑u shows the diff with default switches;
                  *two* C‑u’s lets `diff-current-kill` prompt for extra switches,
                  because the raw prefix it receives is (16)."
  (interactive "P")
  (if arg
      (let ((current-prefix-arg arg))      ; forward the exact prefix
        (call-interactively #'diff-current-kill))
    (ediff-current-kill)))

(defun my/save-clipboard-to-kill-ring ()
  "Save current system clipboard to kill ring without yanking."
  (interactive)
  (when-let* ((text (gui-get-selection 'CLIPBOARD)))
    (kill-new text)
    (message "Clipboard saved to kill ring")))

(defun my-reload-elisp-dir (dir &optional hard)
  "Reload all .el files in DIR.
With prefix arg HARD (\\[universal-argument]), unload features first."
  (interactive "DDirectory: \nP")
  (let* ((files (directory-files dir t "\\.el\\'"))
         (files (seq-remove (lambda (f)
                              (string-match-p
                               "\\(?:-autoloads\\.el\\|-pkg\\.el\\)\\'" f))
                            files))
         (features
          (delete-dups
           (apply #'append
                  (mapcar
                   (lambda (file)
                     (with-temp-buffer
                       (insert-file-contents file)
                       (let (out)
                         (goto-char (point-min))
                         (while (re-search-forward "^(provide '\\([^)\n]+\\))" nil t)
                           (push (intern (match-string 1)) out))
                         out)))
                   files)))))
    (when hard
      (dolist (feat features)
        (ignore-errors (unload-feature feat t))))
    (let ((load-prefer-newer t))
      (dolist (f files)
        (load (file-name-sans-extension f) nil 'nomessage)))
    (message "Reloaded %d files from %s%s"
             (length files) dir (if hard " (hard)" ""))))

(defun copy-file-name-line-number-at-point (&optional absolute)
  "Copy the file name and line number at point to the kill ring.
The format is FILE:LINE, e.g. \"foo.el:42\".

By default, use a project-relative path if the file belongs to a
project, otherwise a plain file name.  With a prefix argument ABSOLUTE,
use the absolute path instead."
  (interactive "P")
  (let* ((file (or (buffer-file-name)
                   (user-error "Buffer is not visiting a file")))
         (line (line-number-at-pos))
         (name (cond
                (absolute file)
                ((and (fboundp 'project-current)
                      (project-current))
                 (file-relative-name file (project-root (project-current))))
                (t (file-name-nondirectory file)))))
    (kill-new (format "%s:%d" name line))
    (message "%s:%d" name line)))

;;; keymaps

(defvar-keymap my-window-map
  :doc "Window and buffer keymap."
  "h" #'windmove-left
  "j" #'windmove-down
  "k" #'windmove-up
  "l" #'windmove-right
  "0" #'delete-window
  "1" #'delete-other-windows
  "2" #'split-window-below
  "3" #'split-window-right
  "-" #'shrink-window-if-larger-than-buffer
  "=" #'balance-windows
  "^" #'enlarge-window
  "}" #'enlarge-window-horizontally
  "{" #'shrink-window-horizontally
  "v" #'shrink-window
  "o" #'other-window
  "O" #'my-other-window-backward
  "b" #'tab-bar-history-back
  "f" #'tab-bar-history-forward
  "B" #'consult-buffer
  "i" #'consult-bookmark
  "n" #'next-buffer
  "p" #'previous-buffer
  "q" #'bury-buffer
  "x" #'kill-current-buffer
  "X" #'kill-buffer-and-window)

(defun my-window-map-dispatch ()
  (interactive)
  (set-transient-map
   my-window-map
   t
   nil
   'message
   ;; "Window: h/j/k/l move, 0/1/2/3 layout, o/O win, b/f tab-hist, n/p buf, x/X kill, B/i switch/bookmark"
   ))

(keymap-global-set "C-c w" #'my-window-map-dispatch)

(defvar structural-edit-map (make-sparse-keymap)
  "Structural editing keymap.")

(defvar tex-parens-structural-edit-map (make-sparse-keymap)
  "Structural editing keymap used by `tex-parens-mode'.")

(bind-keys
 :map structural-edit-map
 ("n" . forward-list)
 ("p" . backward-list)
 ("u" . backward-up-list)
 ("M-u" . up-list)
 ("g" . down-list)
 ("f" . forward-sexp)
 ("b" . backward-sexp)
 ("a" . beginning-of-list)
 ("e" . end-of-list)
 ("[" . beginning-of-defun)
 ("]" . end-of-defun)
 ("M-g" . backward-down-list)
 ("A" . kill-to-beginning-of-list)
 ("E" . kill-to-end-of-list)
 ("i" . mark-inner)
 ("k" . kill-sexp)
 ("x" . eval-last-sexp)
 ("/" . delete-pair)
 ("t" . transpose-sexps)
 ("w" . kill-region)
 ("M-w" . kill-ring-save)
 ("y" . yank)
 ("C-M-SPC" . mark-sexp)
 ("RET" . newline-and-indent))

(put 'recenter-top-bottom 'repeat-continue t)

(defun my-structural-edit-dispatch ()
  "Activate a structural editing keymap for the current context."
  (interactive)
  (let ((map (cond
              ((and (bound-and-true-p tex-parens-mode)
                    (boundp 'tex-parens-structural-edit-map))
               tex-parens-structural-edit-map)
              ((boundp 'structural-edit-map)
               structural-edit-map))))
    (unless map
      (user-error "No structural editing map is available here"))
    (set-transient-map
     map
     t
     nil
     'message
     ;; "Struct: n/p/u/g lists, a/e list bounds, A/E kill bounds, [/] defun, m/j/+ etc mode extras"
     )))

(defvar-keymap my-outline-map
  "n" #'outline-next-heading
  "p" #'outline-previous-heading
  "u" #'outline-up-heading
  "f" #'outline-forward-same-level
  "b" #'outline-backward-same-level
  "<left>" #'outline-promote
  "<right>" #'outline-demote
  "<up>" #'outline-move-subtree-up
  "<down>" #'outline-move-subtree-down
  "x" #'foldout-exit-fold-without-hiding
  "z" #'foldout-zoom-subtree
  "a" #'outline-show-all
  "c" #'outline-hide-entry
  "d" #'outline-hide-subtree
  "e" #'outline-show-entry
  "<tab>" #'outline-cycle
  "<backtab>" #'outline-cycle-buffer
  "k" #'outline-show-branches
  "l" #'outline-hide-leaves
  "o" #'outline-hide-other
  "q" #'outline-hide-sublevels
  "s" #'outline-show-subtree
  "t" #'outline-hide-body
  "@" #'outline-mark-subtree
  "C-M-SPC" #'outline-mark-subtree
  "w" #'kill-region
  "M-w" #'kill-ring-save
  "C-/" #'undo
  "y" #'yank)

(defun my-outline-map-dispatch ()
  "Activate `my-outline-map' transiently."
  (interactive)
  (set-transient-map
   my-outline-map
   t
   nil
   "Outline: n/p/u/f/b move, a/c/d/e/s show-hide, arrows move/promote, x/z foldout"))

(keymap-global-set "C-c o" #'my-outline-map-dispatch)
(keymap-global-set "C-c j" #'my-structural-edit-dispatch)
(keymap-global-set "H-t" #'czm-toggle-dark-mode)
(keymap-global-set "<remap> <eval-expression>" #'my/eval-expression-and-copy)

(when cfg-full
  (keymap-global-set "s-d" #'czm-find-math-document))

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
 ("q" . fill-previous-paragraph))

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
 ("t" . transpose-sentences))

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
   ("C-S-s-o" . (lambda () (interactive) (other-frame -1)))
   ("C-s-o" . other-frame)
   ("C-x C-M-t" . transpose-regions)
   ("C-x C-b" . ibuffer)
   ("C-x Q" . bury-buffer)
   ("C-x v /"  . vc-log-search)
   ("C-z" . nil)
   ("C-z c" . calendar)
   ("C-z d" . eldoc-doc-buffer)
   ("C-z C-c" . restart-emacs)
   ("C-z C-e" . pp-macroexpand-last-sexp)
   ("C-z C-s" . desktop-save-in-desktop-dir)
   ("C-z C-f" . desktop-read)
   ("C-z y" . my/save-clipboard-to-kill-ring)
   ("C-z s" . whitespace-cleanup)
   ("H-0" . tab-close)
   ("H-1" . tab-close-other)
   ("H-2" . tab-bar-new-tab)
   ("C-z A" . tab-recent)
   ("H-SPC H-SPC" . ediff-buffers)
   ("H-SPC SPC" . ediff-buffers)
   ("H-SPC d" . diff-buffer-with-file)
   ("H-SPC e" . ediff-current-file)
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
   ("s-=" . text-scale-adjust)
   ("s--" . text-scale-adjust)
   ("s-0" . delete-window)
   ("s-1" . delete-other-windows)
   ("s-2" . split-window-below)
   ("s-3" . split-window-right)
   ("s-6" . (lambda () (interactive) (delete-indentation nil)))
   ("s-7" . (lambda () (interactive) (delete-indentation t)))
   ("s-<backspace>" . backward-kill-sentence)
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
   ("s-z" . nil))
  :custom
  (Man-notify-method 'pushy)
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
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (display-time-default-load-average nil)
  (recentf-max-saved-items 100)
  (calc-kill-line-numbering nil)
  (eglot-connect-timeout 120)
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
  (revert-without-query '("\\.pdf$"))
  :config
  (setopt auto-save-file-name-transforms
          `((".*" ,(expand-file-name
                    (let ((dir-name (expand-file-name "auto-save" user-emacs-directory)))
                      (unless (file-exists-p dir-name)
                        (make-directory dir-name))
                      dir-name))
             t)))
  (setopt mail-dont-reply-to-names
          (concat
           (and (stringp user-mail-address)
                (> (length user-mail-address) 0)
                (concat "\\`" (regexp-quote user-mail-address) "\\'\\|"))
           "@noreply"))
  (setopt isearch-lazy-count t)
  (setopt vc-handled-backends '(Git))
  (setopt safe-local-variable-directories
          '("/Users/au710211/repos/nla-main/"
            "/Users/au710211/repos/nla-prep/"))
  (setopt safe-local-variable-values
          '((aggressive-indent-mode)
            (cmake-build-project-root . "./cpp")
            (checkdoc-minor-mode . t)
            (eval TeX-run-style-hooks "nla-notes")
            (eval outline-hide-sublevels 5)
            (eval defun agent-shell-run-all-tests nil
                  "Run all agent-shell tests in batch mode." (interactive)
                  (let
                      ((test-dir
                        (expand-file-name "tests/"
                                          (project-root (project-current t)))))
                    (dolist
                        (file (directory-files-recursively test-dir "\\.el$"))
                      (unless (string-match-p "/\\." file) (load file)))
                    (if noninteractive
                        (ert-run-tests-batch-and-exit "^agent-shell")
                      (ert "^agent-shell"))))))
  (setopt vc-deduce-backend-nonvc-modes t)
  (setopt ispell-save-corrections-as-abbrevs t)
  (setopt tab-bar-format
          '(
            ;; tab-bar-format-menu-bar
            ;; tab-bar-format-history
            tab-bar-format-tabs-groups
            tab-bar-format-align-right
            tab-bar-format-global))
  (setopt backup-directory-alist
          `(("." . ,(expand-file-name
                     (concat user-emacs-directory "backups")))))
  (setopt version-control t)
  (setopt kept-new-versions 100)
  (setopt kept-old-versions 100)
  (setopt newsticker-url-list
          '(("r/emacs" "https://www.reddit.com/r/emacs.rss" nil nil nil)))
  (setopt repeat-keep-prefix t)
  (setopt dired-deletion-confirmer #'yes-or-no-p)
  (setopt set-mark-command-repeat-pop t)
  (setopt mailcap-user-mime-data
          '((text-mode "text/plain" nil)
            (diff-mode "text/x-patch" nil)))
  (setopt diff-default-read-only t)
  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  ;; (fset 'yes-or-no-p 'y-or-n-p)
  (setq-default indent-tabs-mode nil)
  (setq desktop-dirname user-emacs-directory)
  (auto-insert-mode)
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
  (add-to-list 'savehist-additional-variables 'register-alist)
  (vc-auto-revert-mode)
  :hook
  (prog-mode . outline-minor-mode)
  (dired-mode . dired-hide-details-mode)
  (dired-mode . dired-omit-mode)
  (dired-mode . hl-line-mode))


;;; display

(setq display-buffer-base-action
      '((display-buffer-reuse-window display-buffer-same-window)
        (inhibit-same-window . nil)))

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(?:.*-\\)?compilation\\*\\(?:<[^>]+>\\)?\\'"
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom)
               (reusable-frames . visible)
               (window-height . 0.3)))

;; Detect if a Help buffer is being displayed for
;; `what-cursor-position' and, if so, display it in a side window on
;; the right instead of the default behavior.

(defun my-help-from-what-cursor-position-p (buf-name _action)
  "Return non-nil if BUF-NAME is a Help buffer for `what-cursor-position'."
  (when (string= buf-name "*Help*")
    (with-current-buffer buf-name
      (save-excursion
        (goto-char (point-min))
        (looking-at "^position:")))))

(add-to-list 'display-buffer-alist
             '(my-help-from-what-cursor-position-p
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.3)
               (select . nil)))

;;; built-in package tweaks

(use-package outline
  :ensure nil
  :demand t
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
   ("<left-margin> <mouse-1>" . nil)))

;; This could be its own package, accommodating git-friendly abbrev storage?
;; Need a good way to update the source.
(defun modify-abbrev-table (table abbrevs)
  "Define abbreviations in TABLE given by ABBREVS."
  (unless table
    (error "Abbrev table does not exist: %S" table))
  (dolist (abbrev abbrevs)
    (define-abbrev table (car abbrev) (cadr abbrev) (caddr abbrev))))

(use-package-full abbrev
  :ensure nil
  :defer
  :hook ((prog-mode text-mode erc-mode) . abbrev-mode)
  :custom
  (abbrev-file-name (concat user-emacs-directory "abbrev_defs.el"))
  (save-abbrevs 'silently)
  :config
  (let ((file (concat user-emacs-directory "abbrev_defs.el")))
    (when (file-exists-p file)
      (quietly-read-abbrev-file file)))
  (quietly-read-abbrev-file (concat user-emacs-directory "abbrev.el")))

(with-eval-after-load 'dired
  (setcdr dired-jump-map nil))

(with-eval-after-load 'project
  (setopt project-vc-extra-root-markers '(".project"))
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

(advice-add 'describe-char :around
            (lambda (orig-fun &rest args)
              (let ((help-window-select nil))
                (apply orig-fun args))))

(with-eval-after-load 'diff-mode
  (keymap-set diff-read-only-map "[" #'diff-file-prev)
  (keymap-set diff-read-only-map "]" #'diff-file-next))

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
  "Restore the window configuration saved before Ediff started.

This restores the *layout*, but tries not to clobber the final point
positions you navigated to during the Ediff session."
  (when (window-configuration-p ediff-saved-window-configuration)
    (let* ((saved ediff-saved-window-configuration)
           ;; Capture the user-visible state at the end of the Ediff session.
           ;; This prevents `set-window-configuration' from resetting point
           ;; back to where it was when the session started.
           (states
            (delq nil
                  (mapcar
                   (lambda (win)
                     (when (window-live-p win)
                       (list (window-buffer win)
                             (window-point win)
                             (window-start win))))
                   (list (and (boundp 'ediff-window-A) ediff-window-A)
                         (and (boundp 'ediff-window-B) ediff-window-B)
                         (and (boundp 'ediff-window-C) ediff-window-C)
                         (and (boundp 'ediff-window-Ancestor)
                              ediff-window-Ancestor))))))
      (run-with-timer
       0.01 nil
       (lambda ()
         (set-window-configuration saved)
         (dolist (st states)
           (pcase-let ((`(,buf ,pt ,start) st))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (let ((p (and (integerp pt)
                               (min (point-max) (max (point-min) pt))))
                       (s (and (integerp start)
                               (min (point-max) (max (point-min) start)))))
                   (when p (goto-char p))
                   (dolist (w (get-buffer-window-list buf nil t))
                     (when (window-live-p w)
                       (when s (set-window-start w s t))
                       (when p (set-window-point w p)))))))))
         (setq ediff-saved-window-configuration nil))))))

(defun ediff-kill-temporary-file-buffer ()
  (when (and (buffer-live-p ediff-buffer-A)
             (or (string-prefix-p "FILE=" (buffer-name ediff-buffer-A))
                 (string-prefix-p "UNDO=" (buffer-name ediff-buffer-A))))
    (kill-buffer ediff-buffer-A)))

(add-hook 'ediff-before-setup-hook #'ediff-save-window-configuration)
(add-hook 'ediff-startup-hook #'ediff-make-configuration-local)
(add-hook 'ediff-quit-hook #'ediff-restore-window-configuration)
(add-hook 'ediff-cleanup-hook #'ediff-kill-temporary-file-buffer)

(with-eval-after-load 'tex-mode
  (mapc
   (lambda (sym) (add-to-list 'tex--prettify-symbols-alist sym))
   '(("\\eps" . ?ε)
     ("\\ " . 9141) ; Literal ?⎵ breaks indentation
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

(font-lock-add-keywords 'Info-mode '((" -- \\([^:]+\\): \\_<\\(.+\\)\\_>" . 2)))
(font-lock-add-keywords 'Info-mode '(("‘\\<\\([^’]+\\)\\>’" . 1)))

(use-package doc-view
  :ensure nil
  :bind
  (:map doc-view-mode-map
        ("C-c g" . doc-view-goto-page)
        ("<down>" . nil)
        ("<up>" . nil)))

;;; tramp

(use-package-full tramp
  :ensure nil
  :config
  ;; Use a login shell remotely so PATH/tool setup is available to Eglot/LSP.
  ;; Keep startup files quiet: any output from shell init can break TRAMP parsing.
  ;; If TRAMP login gets flaky, revisit `tramp-remote-shell-args` first.
  (setq tramp-default-remote-shell "/bin/bash"
        tramp-remote-shell-args '("-lc")
        tramp-verbose 1
        tramp-use-connection-share nil)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; Make async remote processes (used by Eglot/LSP) reliable over SSH:
  ;; - enable direct async processes
  ;; - run ssh without allocating a TTY
  (require 'seq)
  (setq tramp-connection-properties
        (append
         (seq-remove (lambda (elt)
                       (and (stringp (nth 0 elt))
                            (string-prefix-p "/ssh:" (nth 0 elt))
                            (equal (nth 1 elt) "direct-async")))
                     tramp-connection-properties)
         (list '("/ssh:.*:" "direct-async" ("-T")))))

  (connection-local-set-profile-variables
   'tramp-ssh-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'tramp-ssh-direct-async-process))

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

(defun my-rmail-mode-hook ()
  (setq-local preview-tailor-local-multiplier 0.6)
  (my/maybe-set-preview-master-local))

(defun my-rmail-refile-and-store-link ()
  "Refile current message and store an org link to it."
  (interactive)
  (rmail-output rmail-default-file)
  (my-rmail-store-link-to-last-message rmail-default-file))

(defun my-rmail-store-link-to-last-message (file-name)
  "Store an org link to the last message in FILE-NAME."
  (let ((buffer (find-file-noselect file-name)))
    (with-current-buffer buffer
      (rmail-last-message)
      (require 'org)
      (org-store-link nil t))))

(defun my-rmail-summary-output-and-store-link (&optional file-name n)
  "Append message(s) to FILE-NAME and store an org link to the last one.
This mirrors `rmail-summary-output' but also stores a link."
  (interactive
   (progn (require 'rmailout)
          (list (rmail-output-read-file-name)
                (prefix-numeric-value current-prefix-arg))))
  (let ((target (or file-name rmail-default-file)))
    (rmail-summary-output target n)
    (my-rmail-store-link-to-last-message target)))

(defun my-rmail-summary-goto-msg-and-select (&optional n nowarn)
  "Go to message N and show it in the current window's Rmail buffer.
This keeps summary navigation commands in the summary window while making
`RET' replace the summary window with the main Rmail buffer."
  (interactive
   (list (and current-prefix-arg
              (prefix-numeric-value current-prefix-arg))
         nil))
  (when (rmail-summary-goto-msg n nowarn t)
    (let ((msg (rmail-summary-msg-number))
          (buf (and (boundp 'rmail-buffer) rmail-buffer)))
      (when (buffer-live-p buf)
        (switch-to-buffer buf)
        (rmail-show-message msg)))))

(use-package-full rmail
  :ensure nil
  :defer t
  :bind
  ("C-z r" . rmail)
  (:map rmail-mode-map
        ("S" . my-rmail-refile-and-store-link)
        ("q" . rmail-bury)
        ("M-m" . nil))
  :hook (rmail-mode . my-rmail-mode-hook)
  :custom
  (rmail-summary-address-width 35)
  (rmail-summary-sender-function #'rmail-summary-name-or-address)
  (rmail-summary-recipient-function #'rmail-summary-recipient-names)
  (rmail-mime-attachment-dirs-alist `((".*" ,my-downloads-folder)))
  (rmail-mime-save-action (lambda (file) (dired-jump nil file)))
  (rmail-file-name (expand-file-name "inbox.rmail" my-mail-folder))
  (rmail-movemail-program "movemail")
  (rmail-primary-inbox-list (list my-mail-inbox))
  (rmail-automatic-folder-directives
   `((,(expand-file-name "arxiv.rmail" my-mail-folder)
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
  (add-to-list 'auto-mode-alist '("\\.rmail$" . rmail-mode))
  (add-to-list 'auto-mode-alist '("\\.mbox$" . rmail-mode))
  (with-eval-after-load 'rmailsum
    (define-key rmail-summary-mode-map "O"
                #'my-rmail-summary-output-and-store-link)
    (define-key rmail-summary-mode-map (kbd "RET")
                #'my-rmail-summary-goto-msg-and-select))
  (defun my-always-enable-rmail-font-lock (&rest _)
    "Ensure font-lock-mode is enabled after rmail runs."
    ;; Check we're actually in rmail-mode, in case rmail errored out.
    (when (derived-mode-p 'rmail-mode)
      (font-lock-mode 1)))
  (advice-add 'rmail :after #'my-always-enable-rmail-font-lock)
  (defun my-display-buffer-rmail-unsent-p (buffer-or-name _action)
    "Return non-nil for compose buffers opened from Rmail."
    (let ((name (if (bufferp buffer-or-name)
                    (buffer-name buffer-or-name)
                  buffer-or-name)))
      (and (stringp name)
           (or (string-prefix-p "*unsent" name)
               (string= name "*mail*"))
           (with-current-buffer (window-buffer (selected-window))
             (derived-mode-p 'rmail-mode 'rmail-summary-mode)))))
  (add-to-list 'display-buffer-alist
               '(my-display-buffer-rmail-unsent-p
                 display-buffer-same-window
                 (inhibit-same-window . nil))))

(use-package-full sendmail
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


(use-package-full message
  :ensure nil
  :mode ("\\*message\\*-[0-9]\\{8\\}-[0-9]\\{6\\}\\'" . message-mode)
  :config
  (defun my-mail-message-tab ()
    "Use `mail-abbrev-insert-alias' in headers, otherwise `message-tab'."
    (interactive)
    (if (message-point-in-header-p)
        (call-interactively #'mail-abbrev-insert-alias)
      (message-tab)))
  (defun my-message-insert-debbugs-cc (address)
    "Insert an X-Debbugs-Cc header with ADDRESS."
    (interactive (list (read-string "X-Debbugs-Cc: " nil nil "bug-gnu-emacs@gnu.org")))
    (save-excursion
      (message-add-header (format "X-Debbugs-Cc: %s" address))))
  :bind (:map message-mode-map
              ("C-c RET a" . mml-attach-buffer-or-file)
              ("C-c C-f C-z" . my-message-insert-debbugs-cc)
              ("TAB" . my-mail-message-tab))
  :custom
  (message-make-forward-subject-function #'message-forward-subject-fwd))

(use-package-full mairix
  :ensure nil
  :defer t
  :bind
  ("C-z m" . mairix-transient-menu)
  :config
  (cl-pushnew '("Path" "p" "Path") mairix-widget-fields-list)

  (defconst mairix-syntax-help-text
    "Mairix Search Syntax:

word          : match word in message body and major headers
t:word        : match word in To: header
c:word        : match word in Cc: header
f:word        : match word in From: header
a:word        : match word in To:, Cc: or From: headers (address)
s:word        : match word in Subject: header
b:word        : match word in message body
m:word        : match word in Message-ID: header
n:word        : match word in name of attachment
F:flags       : match message flags (s=seen,r=replied,f=flagged,-=negate)
p:substring   : match substring of path
d:start-end   : match date range
z:low-high    : match messages in size range

Advanced syntax:
bs:word       : match word in Subject: header or body
s:word1,word2 : match both words in Subject:
s:word1/word2 : match either word or both words in Subject:
s:~word       : match messages not containing word in Subject:
s:substring=  : match substring in any word in Subject:
s:^substring= : match left-anchored substring in any word in Subject:
s:substring=2 : match substring with <=2 errors in any word in Subject:"
    "Help text for mairix search syntax.")

  (defun mairix-display-syntax-help ()
    "Display mairix search syntax help in a buffer."
    (interactive)
    (with-help-window "*Mairix Syntax Help*"
      (princ mairix-syntax-help-text)))

  (require 'transient)
  (transient-define-prefix
    mairix-transient-menu ()
    "Mairix search commands."
    :info-manual "(mairix-el)Top"
    [["Search"
      ("RET" "Prompted search" mairix-search)
      ("f"   "Search by sender" mairix-search-from-this-article)
      ("t"   "Search thread" mairix-search-thread-this-article)]
     ["Widget search"
      ("w" "Widget search" mairix-widget-search)
      ("b" "Widget based on article" mairix-widget-search-based-on-article)]
     ["Saved searches"
      ("s" "Use saved search" mairix-use-saved-search)
      ("S" "Save last search" mairix-save-search)
      ("e" "Edit saved searches" mairix-edit-saved-searches)]]
    [:class transient-row
            ("u" "Update database" mairix-update-database)
            ("?" "Show syntax help" mairix-display-syntax-help)
            ("i" "Info docs" (lambda ()
                               (interactive)
                               (info "(mairix-el)")))]))

;;; quit

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

(defvar elpaca-installer-version 0.11)
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
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
  ;; ;; With this next option, it's important that PATH is set up inside
  ;; ;; .zshenv rather than .zshrc.
  ;; (setq exec-path-from-shell-arguments nil)
  (setopt exec-path-from-shell-variables
          '("PATH" "MANPATH"
            "OPENAI_KEY" "OPENAI_API_KEY"
            "ANTHROPIC_KEY"
            "GEMINI_KEY"
            "DEEPSEEK_KEY"
            "GITHUB_MCP_PAT"
            "TWITCH_OAUTH_TOKEN"))
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
  (with-eval-after-load 'buffer-face-mode
    (diminish 'buffer-face-mode))
  (with-eval-after-load 'outline
    (diminish 'outline-minor-mode))
  (with-eval-after-load 'follow
    (diminish 'follow-mode)))

(defun my/aggressive-indent-latex-tuning ()
  "Reduce aggressive-indent churn in LaTeX buffers."
  (setq-local aggressive-indent-sit-for-time 0.12)
  (setq-local aggressive-indent-protected-current-commands
              (cons 'dynexp-space aggressive-indent-protected-current-commands)))

(use-package aggressive-indent
  :defer t
  :diminish
  :hook
  ((emacs-lisp-mode LaTeX-mode rust-mode) . aggressive-indent-mode)
  (LaTeX-mode . my/aggressive-indent-latex-tuning))

;; Remove "%n" from mode-line-modes -- I know when I'm narrowing.
(setq mode-line-modes (delete "%n" mode-line-modes))

(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "E")))
(add-hook 'lisp-interaction-mode-hook (lambda () (setq mode-name "LI")))

(with-eval-after-load 'tex-mode
  (add-hook 'LaTeX-mode-hook
            (lambda () (setq TeX-base-mode-name "L"))))
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

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun avy-action-kill-whole-line (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (kill-whole-line)
          (avy-resume))
      (select-window
       (cdr
        (ring-ref avy-ring 0))))
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
   ("C-c /" . avy-goto-char-timer)
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
	 ("C-x j" . czm-misc-dired-popup)
	 (:map minibuffer-local-map
               ("C-c d" . czm-misc-insert-date)))
  :init
  (keymap-set my-window-map "4" #'czm-misc-double-split-window-below-and-delete)
  (keymap-set my-window-map "5" #'czm-misc-double-split-window-right-and-delete))

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

(use-package corfu
  :ensure t
  :hook
  (lean4-mode . corfu-mode)
  (emacs-lisp-mode . corfu-mode)
  (eval-expression-minibuffer-setup . corfu-mode)
  :config
  (setq
   corfu-auto-delay 0.05
   corfu-auto t
   )
  :bind
  (:map
   corfu-map
   ("`" . corfu-next)
   ("~" . corfu-previous)
   ([remap next-line] . nil)
   ([remap move-beginning-of-line] . nil)
   ([remap move-end-of-line] . nil)
   ([remap beginning-of-buffer] . nil)
   ([remap end-of-buffer] . nil)
   ([remap scroll-up-command] . nil)
   ([remap scroll-down-command] . nil)
   ("<tab>" . nil)
   ("TAB" . nil)
   ("C-a" . nil)
   ("RET" . nil)
   ("C-M-i" . corfu-complete)
   ("C-M-RET" . corfu-insert)
   ("M-RET" . nil)
   ;; ("M-." . corfu-show-location)
   ;; ("M-h" . nil)
   ([remap next-line] . nil)
   ([remap previous-line] . nil)
   ;; ("M-." . corfu-info-location)
   ;; ("C-h" . corfu-info-documentation)
   ))

(use-package vertico
  :ensure t
  :config (vertico-mode))

(use-package marginalia
  :ensure t
  :demand
  :config (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic)))

(use-package consult
  :ensure t
  :bind
  (("C-c M-x" . consult-mode-command)
   ("C-c i" . consult-info)
   ([remap repeat-complex-command] . consult-complex-command)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap bookmark-jump] . consult-bookmark)
   ("s-i" . consult-bookmark)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap jump-to-register] . consult-register-load)
   ([remap point-to-register] . consult-register-store)
   ("C-x r RET" . consult-register)
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
        ("g" . consult-ripgrep)
        ("B" . my/consult-project-buffer-global))
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
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   consult-source-project-recent-file-hidden
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if (bound-and-true-p vertico-mode)
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(consult-ripgrep "Ripgrep")))
  (setq project-switch-commands
        (cl-remove 'project-find-regexp project-switch-commands :key #'car)))

(defun my/consult-buffer-global ()
  "Run `consult-buffer' with all buffers."
  (interactive)
  (let ((consult-buffer-list-function #'buffer-list))
    (consult-buffer)))

(defun my/consult-buffer-local ()
  (interactive)
  (let ((consult-buffer-list-function #'bufferlo-local-buffers)
        (consult-buffer-sources '(consult-source-buffer)))
    (consult-buffer)))

(defun my/consult-project-buffer-global ()
  "Run `consult-project-buffer' without local `bufferlo' filtering."
  (interactive)
  (let ((consult-buffer-list-function #'buffer-list))
    (consult-project-buffer)))

(use-package bufferlo
  :ensure t
  :after consult
  :demand t
  :bind
  (("C-x B" . my/consult-buffer-global)
   ("C-x b" . my/consult-buffer-local)
   ("s-b" . my/consult-buffer-local))
  :config
  (setq consult-buffer-list-function #'bufferlo-local-buffers)
  (bufferlo-mode 1))

(with-eval-after-load 'consult-imenu
  (require 'cl-lib)
  (when-let* ((config (alist-get 'emacs-lisp-mode consult-imenu-config)))
    (let ((types (plist-get config :types)))
      (cl-pushnew '(?l "LLM Tools" font-lock-function-name-face) types
                  :test (lambda (a b) (eq (car a) (car b))))
      (setf (plist-get config :types) types))))

(defun consult-ripgrep-current-directory ()
  "Run consult-ripgrep in current directory.
Use \" -- ...\" in the minibuffer to append ripgrep flags."
  (interactive)
  (consult-ripgrep default-directory))

(defun consult-ripgrep--files (prompt files &optional default-extra-args)
  "Search FILES with PROMPT.
If DEFAULT-EXTRA-ARGS is non-nil, append them to `consult-ripgrep-args'."
  (let ((consult-ripgrep-args (if default-extra-args
                                  (concat consult-ripgrep-args " " default-extra-args)
                                consult-ripgrep-args)))
    (consult--grep prompt #'consult--ripgrep-make-builder files nil)))

(defun consult-ripgrep-org-logs ()
  "Search log-*.org files."
  (interactive)
  (consult-ripgrep--files
   "Ripgrep org notes"
   (directory-files "~/doit/" 'full "^log.*\\.org$")
   "-C3"))

(defun consult-ripgrep-todo-notes ()
  "Search todo note files."
  (interactive)
  (let ((files
         (append
          (my-setting-files 'my-todo-file 'my-projects-file)
          (list "~/doit/reference.org"
                (expand-file-name "diary" user-emacs-directory)))))
    (consult-ripgrep--files
     "Ripgrep todo notes"
     files
     nil)))

(defun consult-ripgrep-config-files ()
  "Search config files."
  (interactive)
  (consult-ripgrep--files
   "Ripgrep config files"
   (mapcar (lambda (file)
             (expand-file-name file user-emacs-directory))
           '("~/.emacs.d/init.el" "~/.emacs.d/init-personal.el"))
   nil))

(defcustom my-python-venv-ripgrep-libs '("numpy" "matplotlib")
  "Top-level packages under site-packages to search."
  :type '(repeat string))

(defun my-python-venv--project-root ()
  (or (locate-dominating-file default-directory ".venv")
      (user-error "No .venv found above %S" default-directory)))

(defun my-python-venv--site-packages ()
  (let* ((root (my-python-venv--project-root))
         (libdir (expand-file-name ".venv/lib" root))
         (pyver (car (seq-filter (lambda (d) (string-prefix-p "python" d))
                                 (directory-files libdir nil "\\`python")))))
    (unless pyver (user-error "No python* dir under %s" libdir))
    (expand-file-name (concat ".venv/lib/" pyver "/site-packages") root)))

(defun my-consult-ripgrep-python-venv-libs ()
  (interactive)
  (let* ((dir (my-python-venv--site-packages))
         (globs (mapconcat (lambda (p) (format "--glob %s/**" (shell-quote-argument p)))
                           my-python-venv-ripgrep-libs
                           " "))
         (consult-ripgrep-args
          (concat consult-ripgrep-args " --no-ignore-parent " globs)))
    (consult-ripgrep dir)))

(defvar-keymap my-ripgrep-map
  :doc "Keymap for enhanced consult-ripgrep commands."
  "r" #'consult-ripgrep
  "d" #'consult-ripgrep-current-directory
  "l" #'consult-ripgrep-org-logs
  "t" #'consult-ripgrep-todo-notes
  "c" #'consult-ripgrep-config-files
  "v" #'my-consult-ripgrep-python-venv-libs)

(keymap-global-set "M-s r" my-ripgrep-map)

(use-package info-colors
  :ensure (:host github :repo "ubolonton/info-colors" :inherit nil)
  :hook (Info-selection . info-colors-fontify-node))

(use-package ace-window
  :config
  ;; https://karthinks.com/software/emacs-window-management-almanac/#aw-select-the-completing-read-for-emacs-windows
  (defun ace-window-prefix ()
    "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let (window type)
         (setq
          window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
          type 'reuse)
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))
  (when cfg-full
    (keymap-global-set "C-x o" #'ace-window-prefix))
  :bind
  ("C-x 4 o" . ace-window-prefix))

;; https://www.jamescherti.com/emacs-customize-ellipsis-outline-minor-mode/
(defun my-outline-set-global-ellipsis (ellipsis)
  "Apply the ellipsis ELLIPSIS to outline mode globally."
  (let* ((face-offset (* (face-id 'shadow) (ash 1 22)))
         (value (vconcat (mapcar (lambda (c) (+ face-offset c)) ellipsis))))
    (set-display-table-slot standard-display-table 'selective-display value)))

(my-outline-set-global-ellipsis " ▼ ")

(use-package-full outline-skip
  :repo-scan
  :after latex
  :ensure (:host github :repo "ultronozm/outline-skip.el"
                 :depth nil
                 :inherit nil :pin t)
  :hook (LaTeX-mode . outline-skip-mode))

(use-package-full perfect-margin
  :defer t
  :diminish
  :bind ("H-m" . perfect-margin-mode))

(use-package easy-kill
  :bind
  (([remap kill-ring-save] . easy-kill)
   ;; ([remap mark-sexp] . easy-mark)
   ))

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

(use-package-full rust-mode
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
  :ensure t
  :commands (lispy-multiline
             lispy-split
             lispy-join
             lispy-slurp-or-barf-right
             lispy-slurp-or-barf-left
             lispy-splice
             lispy-comment
             lispy-clone
             lispy-tab
             lispy-move-up
             lispy-move-down)
  :bind
  (:map
   emacs-lisp-mode-map
   (";" . czm-lispy-comment-maybe)
   ("M-1" . lispy-describe-inline)
   ("M-2" . lispy-arglist-inline))
  :init
  (bind-keys
   :map structural-edit-map
   :repeat-map structural-edit-map
   :continue-only
   ("m" . lispy-multiline)
   ("j" . lispy-split)
   ("+" . lispy-join)
   (">" . lispy-slurp-or-barf-right)
   ("<" . lispy-slurp-or-barf-left)
   ("/" . lispy-splice)
   (";" . lispy-comment)
   ("c" . lispy-clone)
   ("<tab>" . lispy-tab)
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

(use-package-full flycheck
  :ensure t
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

(use-package-full flycheck-package
  :ensure t
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

(use-package-full attrap
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

(defun my/treesit-auto-enable-current-buffer ()
  "Install and activate the tree-sitter mode for the current buffer."
  (interactive)
  (require 'treesit-auto)
  (let* ((treesit-language-source-alist (treesit-auto--build-treesit-source-alist))
         (recipe (or (treesit-auto--get-mode-recipe)
                     (treesit-auto--get-buffer-recipe)))
         (lang (and recipe (treesit-auto-recipe-lang recipe)))
         (ts-mode (and recipe (treesit-auto-recipe-ts-mode recipe))))
    (unless recipe
      (user-error "No treesit-auto recipe for %s" major-mode))
    (dolist (req-lang (ensure-list (treesit-auto-recipe-requires recipe)))
      (unless (treesit-ready-p req-lang t)
        (treesit-auto--prompt-to-install-package req-lang)))
    (unless (treesit-ready-p lang t)
      (treesit-auto--prompt-to-install-package lang))
    (treesit-auto-add-to-auto-mode-alist (list lang))
    (unless (fboundp ts-mode)
      (user-error "Mode not available: %S" ts-mode))
    (funcall ts-mode)))

(use-package treesit-auto
  :defer t
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs
   '(awk bash bibtex blueprint c-sharp clojure cmake commonlisp css dart
         dockerfile elixir glsl go gomod heex html janet java javascript
         json julia kotlin latex lua magik make markdown nix nu org perl
         proto python r ruby rust scala sql surface toml tsx typescript
         typst verilog vhdl vue wast wat wgsl yaml)))

(use-package eglot
  :ensure nil
  :bind
  (:map eglot-mode-map
        ("C-c C-q" . eglot-code-action-quickfix)
        ("C-c C-a" . eglot-code-actions)))

(use-package-full consult-abbrev
  :repo-scan
  :defer t
  :ensure (:host github :repo "ultronozm/consult-abbrev.el" :depth nil
                 :inherit nil :pin t))

(use-package czm-spell
  :repo-scan
  :defer 10
  :ensure (:host github :repo "ultronozm/czm-spell.el" :depth nil
                 :inherit nil :pin t)
  ;; :after latex
  :bind ("s-;" . czm-spell-then-abbrev)
  ;; :bind ("s-;" . czm-spell-correct-backward-lines)
  :config
  (with-eval-after-load 'tex-ispell
    (TeX-ispell-skip-setcar
     '(("\\\\eqref"  ispell-tex-arg-end 1)
       ("\\\\mathrm" ispell-tex-arg-end 1)))))

(use-package typescript-mode
  :ensure t
  :defer t)

(use-package-full julia-ts-mode
  :ensure (:host github :repo "dhanak/julia-ts-mode"
                 :depth nil
                 :inherit nil)
  :mode "\\.jl$")

(use-package-full eglot-jl
  :defer t
  :config
  (eglot-jl-init))

(use-package-full nerd-icons
  :defer t
  :ensure t)

(use-package-full nerd-icons-completion
  :defer t
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package-full nerd-icons-dired
  :defer t
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package-full dired-du
  :defer t
  ;; important to use `dired-omit-mode' to avoid performance issues
  ;; with `..'
  :custom
  (dired-du-bind-count-sizes nil)
  (dired-du-bind-human-toggle nil)
  (dired-du-bind-mode t))

(add-to-list 'major-mode-remap-defaults '(markdown-mode))

(use-package-full debbugs
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

(use-package-full bug-reference
  :ensure nil
  :config
  (defun czm/bug-reference-vc-log-local-only ()
    (unless (file-remote-p default-directory)
      (bug-reference-mode 1)))
  (add-hook 'vc-git-region-history-mode-hook #'czm/bug-reference-vc-log-local-only)
  (add-hook 'vc-git-log-view-mode-hook #'czm/bug-reference-vc-log-local-only)
  (keymap-set bug-reference-map "C-c C-o" #'bug-reference-push-button))

(defun czm/vc-dir-remote-tweaks ()
  (when (file-remote-p default-directory)
    (setq-local vc-dir-show-outgoing-count nil)
    (setq-local vc-git-show-stash nil)))

(add-hook 'vc-dir-mode-hook #'czm/vc-dir-remote-tweaks)

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
(advice-add 'debbugs-gnu-bugs :around #'my/disable-vertico-for-debbugs-search)

;;; embark

(use-package embark
  :repo-scan
  :ensure (:host github
                 :repo "ultronozm/embark"
                 :remotes (("upstream" :repo "oantolin/embark"))
                 :depth nil
                 :inherit nil
                 :pin t)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (defun my-embark-dwim-or-act (arg)
    "Run `embark-dwim'; with prefix ARG run `embark-act'."
    (interactive "P")
    (if arg
        (call-interactively #'embark-act)
      (call-interactively #'embark-dwim)))
  :bind
  (("C-." . embark-act)
   ("C-c ." . embark-act)
   ("M-." . my-embark-dwim-or-act)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t
  ;; only need to install it, embark loads it after consult if found
  :after (embark consult))

(defun my-embark-copy-library-path (library)
  "Copy the file path of Emacs Lisp LIBRARY to the kill ring."
  (interactive "sLocate library: ")
  (let ((path (find-library-name library)))
    (unless path
      (user-error "Cannot locate library `%s'" library))
    (kill-new path)
    (message "%s" path)))

(defconst my-embark-file-line-location-regexp
  "\\`\\(.+\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\'"
  "Regexp matching FILE:LINE and FILE:LINE:COLUMN locations.")

(defun my-embark--sanitize-file-line-location (location)
  "Drop trailing punctuation from LOCATION."
  (replace-regexp-in-string "[.,;!?)}\\]>\"']+\\'" "" location))

(defun my-embark--parse-file-line-location (location)
  "Parse LOCATION and return a list (FILE LINE COLUMN)."
  (setq location (my-embark--sanitize-file-line-location location))
  (unless (and (stringp location)
               (string-match my-embark-file-line-location-regexp location))
    (user-error "Not a file location: %s" location))
  (let ((file (expand-file-name (match-string 1 location)))
        (line (max 1 (string-to-number (match-string 2 location))))
        (column (and (match-string 3 location)
                     (string-to-number (match-string 3 location)))))
    (list file line column)))

(defun my-embark--visit-file-line-location (location finder)
  "Open LOCATION with FINDER and move point to its line and column."
  (pcase-let ((`(,file ,line ,column)
               (my-embark--parse-file-line-location location)))
    (unless (file-exists-p file)
      (user-error "File does not exist: %s" file))
    (when (fboundp 'xref-push-marker-stack)
      (xref-push-marker-stack))
    (funcall finder file)
    (goto-char (point-min))
    (forward-line (1- line))
    (when column
      (move-to-column (max 0 (1- column))))))

(defun my-embark-find-file-line (location)
  "Visit LOCATION in the current window."
  (interactive "sFind location (FILE:LINE[:COLUMN]): ")
  (my-embark--visit-file-line-location location #'find-file))

(defun my-embark-find-file-line-other-window (location)
  "Visit LOCATION in another window."
  (interactive "sFind location in other window (FILE:LINE[:COLUMN]): ")
  (my-embark--visit-file-line-location location #'find-file-other-window))

(defun my-embark-target-file-line-at-point ()
  "Target FILE:LINE or FILE:LINE:COLUMN text at point."
  (when-let* ((raw (thing-at-point 'filename t))
              (location (my-embark--sanitize-file-line-location
                         (substring-no-properties raw)))
              (bounds (bounds-of-thing-at-point 'filename))
              ((string-match my-embark-file-line-location-regexp location))
              (file (expand-file-name (match-string 1 location)))
              ((file-exists-p file)))
    `(file-line ,location ,@bounds)))

(defun my-embark--insert-target-finder-before (finder before)
  "Insert FINDER in `embark-target-finders' before BEFORE."
  (setq embark-target-finders
        (let ((finders (remq finder embark-target-finders))
              (result nil)
              (inserted nil))
          (dolist (fn finders)
            (when (and (not inserted) (eq fn before))
              (push finder result)
              (setq inserted t))
            (push fn result))
          (unless inserted
            (push finder result))
          (nreverse result))))

(defvar my-embark-file-line-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "RET" #'my-embark-find-file-line)
    (keymap-set map "f" #'my-embark-find-file-line)
    (keymap-set map "o" #'my-embark-find-file-line-other-window)
    map)
  "Keymap for Embark actions on FILE:LINE targets.")

(with-eval-after-load 'embark
  (set-keymap-parent my-embark-file-line-map embark-general-map)
  (add-to-list 'embark-keymap-alist '(file-line my-embark-file-line-map))
  (my-embark--insert-target-finder-before
   #'my-embark-target-file-line-at-point
   #'embark-target-file-at-point)
  (keymap-set embark-library-map "W" #'my-embark-copy-library-path))

;;; pdf

(use-package-full pdf-tools
  ;; :disabled
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :ensure (:host github :repo "vedang/pdf-tools"
                 :depth nil
                 :inherit nil :pin t
                 ;; :remotes (("orgtre" :repo "orgtre/pdf-tools"))
                 ;; pdf-tools builds `epdfinfo' from `build/server'.  With Elpaca's
                 ;; default symlinks, building would write artifacts into the git
                 ;; checkout under `repos/'.  Copy server sources into the build dir
                 ;; so building stays under `builds/'.
                 :post-build
                 (let* ((repo default-directory)
                        (pkg (file-name-nondirectory (directory-file-name repo)))
                        (build (expand-file-name (file-name-as-directory pkg) elpaca-builds-directory))
                        (src (expand-file-name "server" repo))
                        (dst (expand-file-name "build/server" build)))
	                  (when (file-directory-p src)
	                    (when (file-exists-p dst)
	                      (cond
	                       ((file-symlink-p dst) (delete-file dst))
	                       ((file-directory-p dst) (delete-directory dst t))
	                       (t (delete-file dst))))
		                   (make-directory (file-name-directory dst) t)
		                   (copy-directory src dst t t t)
		                   ;; Always regenerate autotools outputs in the copied build
		                   ;; tree (if available). This avoids brittle version checks
		                   ;; like `aclocal-1.16` and keeps all generated files under
		                   ;; `elpaca/builds/`.
		                   (when (executable-find "autoreconf")
		                     (let ((default-directory dst))
		                       (unless (zerop (call-process "autoreconf" nil "*elpaca pdf-tools autoreconf*" t "-i"))
		                         (error "pdf-tools: autoreconf failed in %s" default-directory)))))))
  :custom
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
  ;; (pdf-annot-tweak-tooltips nil)
  :bind
  (:map pdf-view-mode-map
        ("j" . pdf-view-jump-to-register)
        ("y" . image-previous-line)
        ("<down>" . nil)
        ("<up>" . nil)
        ("<remap> <scroll-up-command>" . pdf-view-scroll-up-or-next-page)
        ("<remap> <scroll-down-command>" . pdf-view-scroll-down-or-previous-page)
        ("C-c g" . pdf-view-goto-page)
        ([remap jump-to-register] . pdf-view-jump-to-register)
        ([remap point-to-register] . pdf-view-position-to-register)
        ("s-t" . pdf-view-jump-to-register)
        ("s-T" . pdf-view-position-to-register))
  :config
  (add-to-list 'global-auto-revert-ignore-modes 'pdf-view-mode)
  (pdf-tools-install :no-query)
  (require 'pdf-occur)
  ;; (add-hook 'pdf-view-mode-hook #'pdf-view-roll-minor-mode)
  )

(defun my/pdf-annot-setup (_a)
  (LaTeX-mode)
  (my/maybe-set-preview-master-local)
  (preview-auto-mode))

(setq pdf-annot-edit-contents-setup-function #'my/pdf-annot-setup)

(use-package-full doc-view-follow
  :repo-scan
  :defer t
  :ensure (:host github :repo "ultronozm/doc-view-follow.el" :depth nil
                 :inherit nil :pin t)
  :custom (doc-view-follow-hijack t))

(use-package-full pdf-extract
  :defer t
  :repo-scan
  :ensure (:host github :repo "ultronozm/pdf-extract.el"
                 :inherit nil :pin t))

(use-package-full pdf-tools-org-extract
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
                 (ignore-errors
                   (pdf-view-midnight-minor-mode (if enable 1 -1)))))))))
    (if global-pdf-view-midnight-minor-mode
        (progn
          (funcall process-pdf-buffers t)
          (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode))
      (funcall process-pdf-buffers nil)
      (remove-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode))))

;;; translation

(use-package-full gt
  :defer t
  :config
  (setq gt-langs '(da en fr de))
  (setq gt-default-translator 
        (gt-translator 
         :taker (gt-taker :langs '(da en fr de) :text 'paragraph)  ; Match your gt-langs
         :engines (gt-google-engine)
         :render (gt-buffer-render)))
  (setopt gt-buffer-render-window-config
          '((display-buffer-reuse-window display-buffer-below-selected)
            (window-height . 0.25)))
  :bind
  (("C-z t" . gt-translate)
   ("C-z T" . gt-eldoc-mode)
   :map view-mode-map
   ("t" . gt-translate)))

;; (gt-start
;;  (gt-translator
;;   :taker (gt-taker :langs '(fr en) :text 'buffer)
;;   :engines (gt-google-engine)
;;   :render (gt-buffer-render)))

(defun gt-eldoc-documentation-function (callback)
  "Return translation at point via eldoc's callback mechanism."
  (condition-case nil
      (let* ((translator (clone gt-default-translator))
             (render-fn
              (lambda (fn)
                (let (result)
                  (funcall fn
                           (lambda (s) (setq result s)))
                  (when (and callback result)
                    (funcall callback result
                             :face 'font-lock-doc-face)))))
             (slot-set nil))
        (dolist (slot '(render renderer))
          (unless slot-set
            (condition-case nil
                (progn
                  (setf (slot-value translator slot) render-fn)
                  (setq slot-set t))
              (invalid-slot-name nil))))
        (when slot-set
          (gt-start translator)))
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

(defun my/set-TeX-master-preview ()
  (interactive)
  (my/maybe-set-preview-master-local))

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
  (my/maybe-set-preview-master-local)
  (setq-local preview-tailor-local-multiplier 0.7))

(defun my-markdown-hook ()
  (my/maybe-set-preview-master-local)
  (setq-local preview-tailor-local-multiplier 0.7))

(add-hook 'markdown-mode-hook #'my-markdown-hook)

(defun czm-diff-preview-setup ()
  "Set up diff buffer for use with preview-auto-mode."
  (my/maybe-set-preview-master-local)
  (setq-local preview-tailor-local-multiplier 0.7))

(add-hook 'diff-mode-hook #'czm-diff-preview-setup)

(defvar-local my/texlike--saved-local-abbrev-table nil)

(define-minor-mode my/texlike-mode
  "TeX-like abbrevs + navigation anywhere."
  :lighter " TeXlike"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "SPC") #'dynexp-space)
            (define-key map (kbd "TAB") #'dynexp-next)
            map)
  (if my/texlike-mode
      (progn
        (setq my/texlike--saved-local-abbrev-table local-abbrev-table)
        (setq-local abbrev-mode t)
        (when (boundp 'LaTeX-mode-abbrev-table)
          (let ((table (make-abbrev-table)))
            (abbrev-table-put table :parents (delq nil (list local-abbrev-table LaTeX-mode-abbrev-table)))
            (setq-local local-abbrev-table table)))
        (when (fboundp 'tex-parens-mode)
          (tex-parens-mode 1)))
    (when (fboundp 'tex-parens-mode)
      (tex-parens-mode 0))
    (setq-local local-abbrev-table my/texlike--saved-local-abbrev-table)))

(keymap-global-set "H-o" #'my/texlike-mode)

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
   (append
    '(("n" "Agenda and all TODOs"
       ((agenda "")
        (todo "TODO")
        (tags "CLOSED>=\"<today>\""
              ((org-agenda-overriding-header "\nCompleted today\n"))))))
    (when-let* ((projects-file (my-setting-string 'my-projects-file)))
      `(("y" "Year view"
         ((agenda ""
                  ((org-agenda-files (list ,projects-file))
                   (org-agenda-span 365)
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-start-day (format-time-string "%Y-%m-%d"))
                   (org-agenda-prefix-format
                    '((agenda . "  %-12:c%?-12t%6e  %s")))
                   (org-agenda-show-all-dates nil)
                   (diary-show-holidays-flag nil)
                   (org-agenda-include-diary t))))
         ((org-agenda-skip-function
           '(org-agenda-skip-entry-if 'todo 'done))))))))
  (org-default-notes-file
   (or (my-setting-string 'my-todo-file)
       (expand-file-name "notes.org" user-emacs-directory)))
  (org-directory "~/")
  (org-agenda-files (my-setting-files 'my-todo-file 'my-projects-file))
  (org-goto-auto-isearch nil)
  (org-agenda-include-diary t)
  (org-babel-load-languages '((latex . t) (emacs-lisp . t)
                              (python . t) (R . t)
                              (shell . t) (sql . t)))
  (org-babel-python-command "python3")
  (org-confirm-babel-evaluate nil)
  (org-link-elisp-confirm-function nil)
  (org-enforce-todo-dependencies t)
  (org-hide-leading-stars t)
  (org-list-allow-alphabetical t)
  (org-refile-targets '((nil :level . 1)
                        ("~/doit/reference.org" :level . 1)))
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
   (let ((todo-file (my-setting-string 'my-todo-file))
         (log-file (my-setting-string 'my-log-file)))
     (append
      (when todo-file
        `(("i" "Inbox" entry (file+headline ,todo-file "Inbox")
           "* %?\n  %i")))
      (when log-file
        `(("j" "Journal" entry (file+datetree ,log-file)
           "* %?\nEntered on %U\n")))
      (when todo-file
        `(("a" "Inbox (annotated)" entry (file+headline ,todo-file "Inbox")
           "* %?\n%a")
          ("k" "Interruptions" entry (file+headline ,todo-file "Interruptions")
           "* %?\n%U\n" :clock-in t :clock-resume t)))
      '(("d" "Diary" entry (file+datetree simple-journal-db-file)
         "* %U \n%?%i\n" :tree-type week)))))
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
        ("M-{" . org-backward-paragraph)
        ("M-}" . org-forward-paragraph)
        ("C-c C-' e" . my/org-insert-example-with-yank)
        ("C-c C-' s" . my/org-insert-src-with-yank))
  (:repeat-map
   org-paragraph-repeat-map
   ("]" . org-forward-paragraph)
   ("}" . org-forward-paragraph)
   ("[" . org-backward-paragraph)
   ("{" . org-backward-paragraph)
   :continue-only
   ("M-h" . mark-paragraph)
   ("h" . mark-paragraph)
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
  (setopt org-file-apps '((auto-mode . emacs) ("\\.x?html?\\'" . default)
                          ("\\.xlsx\\'" . system)
                          ("\\.docx\\'" . system)))
  (add-hook 'org-src-mode-hook #'hack-dir-local-variables-non-file-buffer)
  (add-hook 'org-src-mode-hook #'my/maybe-set-preview-master-local)
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
  (add-to-list 'display-buffer-alist
               '("\\`CAPTURE-"
                 (display-buffer-same-window)
                 (inhibit-same-window . nil))))

(defun my/org-archive-done-tasks ()
  "Archive all done tasks in the current buffer."
  (interactive)
  (org-archive-all-matches
   (lambda (_beg _end)
     (looking-at
      (concat
       "^\\*+ "
       (regexp-opt '("DONE" "CANCELED")))))))

(use-package-full org-modern
  :after org
  :demand
  :ensure t
  :config
  (global-org-modern-mode 1)
  (setopt org-modern-star 'fold
          org-modern-fold-stars
          '(("▶" . "▼")
            ("▷" . "▽")
            ("▸" . "▾")
            ("▹" . "▿")
            ("▸" . "▾"))))

(use-package-full org-appear
  :ensure t
  :hook
  (org-mode . org-appear-mode))

(use-package-full org-remark
  :after org
  :demand
  :config
  (with-eval-after-load 'embark
    (keymap-set embark-region-map "C-m" #'org-remark-mark)))

;;; more mail

(use-package-full czm-mail
  :repo-scan
  :after rmail
  :demand
  :ensure (:host github :repo "ultronozm/czm-mail.el" :depth nil
                 :inherit nil :pin t)
  :bind
  ("C-z C-@" . czm-mail-mailrc-add-entry)
  (:map rmail-mode-map
        ("S" . czm-mail-refile-and-store-link))
  (:map message-mode-map
        ("TAB" . czm-mail-message-tab)
        ("C-c C-a" . czm-mail-insert-diff-as-attachment))
  :custom
  (czm-mail-refile-file (expand-file-name "scheduled.rmail" my-mail-folder))
  :config
  (czm-mail-setup))

;;; shells (external)

(use-package-full eat-tmux
  :repo-scan
  :after project
  :ensure (:host github :repo "ultronozm/eat-tmux.el" :depth nil
                 :inherit nil)
  :bind (:map project-prefix-map ("t" . eat-tmux-project))
  :init (add-to-list 'project-switch-commands '(eat-tmux-project "Tmux" nil)))

;; (load "~/repos/eat-tmux-orchestrator/eat-tmux-orchestrator.el")

;; (keymap-set project-prefix-map "T" #'eat-tmux-orchestrator)

(use-package-full eat
  :ensure (eat :inherit elpaca-menu-non-gnu-elpa)
  :config
  (add-hook 'eat-mode-hook #'abbrev-mode)
  (let ((pass-through-key [?\C-\\])
        (non-bound-key [?\C-z]))
    (setq-default
     eat-semi-char-non-bound-keys
     (cons non-bound-key
           (seq-remove (lambda (k) (equal k pass-through-key))
                       eat-semi-char-non-bound-keys))
     eat-eshell-semi-char-non-bound-keys
     (cons non-bound-key
           (seq-remove (lambda (k) (equal k pass-through-key))
                       eat-eshell-semi-char-non-bound-keys)))
    (eat-update-semi-char-mode-map)
    (eat-eshell-update-semi-char-mode-map)
    (eat-reload))
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

;;; ai stuff

(use-package-full copilot
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
  (LaTeX-mode . (lambda () (setq tab-width 2)))
  :config
  (add-to-list 'warning-suppress-types '(copilot copilot-exceeds-max-char))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (add-to-list 'copilot-indentation-alist '(lean4-mode 2))
  (with-eval-after-load 'copilot-nes
    (keymap-unset copilot-nes-mode-map "TAB")
    (keymap-unset copilot-nes-mode-map "<tab>")
    (keymap-set copilot-nes-mode-map "C-c c" #'copilot-nes-accept)
    (keymap-set copilot-nes-mode-map "C-c k" #'copilot-nes-dismiss))
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
        ([remap zap-to-char] . copilot-accept-completion-to-char)
        ([remap zap-up-to-char] . copilot-accept-completion-up-to-char)
        ("C-M-`" . copilot-accept-completion-by-paragraph)
        ("C-M-<down>" . copilot-next-completion)
        ("C-M-<up>" . copilot-previous-completion)))

;; (keymap-set copilot-completion-map "<remap> <zap-to-char>" #'copilot-accept-completion-to-char)
;; (keymap-set copilot-completion-map "<remap> <zap-up-to-char>" #'copilot-accept-completion-up-to-char)

(use-package-full plz
  :defer t
  :ensure (:host github :repo "alphapapa/plz.el"
                 :depth nil
                 :inherit nil))

(use-package-full plz-event-source
  :defer t
  :ensure (:host github :repo "r0man/plz-event-source"
                 :depth nil
                 :inherit nil))


(use-package-full llm
  :defer t
  :ensure (:host github
                 :repo "ahyatt/llm"
                 :depth nil
                 :remotes (("ultronozm" :repo "ultronozm/llm"))
                 :inherit nil)
  ;; :init
  ;; (require 'llm-openai)
  ;; (require 'llm-gemini)
  ;; (require 'llm-ollama)
  :custom
  (llm-warn-on-nonfree nil)
  (llm-log t)
  :config
  (add-to-list 'warning-suppress-types '(llm)))

(use-package-full llm-tool-collection
  :defer t
  :after llm
  :ensure (:host github
                 :repo "skissue/llm-tool-collection"
                 :depth nil
                 :remotes (("ultronozm" :repo "ultronozm/llm-tool-collection"))
                 :inherit nil
                 :pin t))

(use-package-full ai-org-chat
  :repo-scan
  :ensure (:host github :repo "ultronozm/ai-org-chat.el"
                 :inherit nil
                 :pin t
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
  (ai-org-chat-content-wrapper #'ai-org-chat--wrap-xml)
  :config
  (add-hook 'llm-tool-collection-post-define-functions #'ai-org-chat-register-tool-spec)
  (mapcar #'ai-org-chat-register-tool-spec (llm-tool-collection-get-all))
  (require 'exec-path-from-shell)
  (ai-org-chat-select-model "sonnet 4.6")
  (add-hook 'ai-org-chat-response-finished-functions
            #'ai-org-chat-auto-format-response
            t))

(use-package-full gptel
  :ensure (:host github :repo "karthink/gptel"
                 :remotes (("ultronozm" :repo "ultronozm/gptel"))
                 :inherit nil
                 :pin t)
  :after exec-path-from-shell
  :defer t
  :bind
  (("C-c <return>" . gptel-send)
   ("C-z g" . gptel-rewrite))
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-log-level nil)
  :config
  (require 'exec-path-from-shell)
  (setopt gptel-rewrite-default-action 'accept)
  (setq gptel-backend
        (gptel-make-anthropic
            "Claude"
          :stream t
          :key (lambda () (exec-path-from-shell-getenv "ANTHROPIC_KEY"))
          :models '(claude-sonnet-4-6)))
  (setq gptel-model 'claude-sonnet-4-6))

(use-package-full gptel-quick
  :defer 2
  :ensure (:host github :repo "karthink/gptel-quick" :inherit nil)
  :demand
  :config
  (with-eval-after-load 'embark
    (define-key embark-general-map (kbd "?") #'gptel-quick)))

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
    (let ((projects-file (my-setting-string 'my-projects-file)))
      (unless projects-file
        (user-error "Set my-projects-file in init-settings.el to use this command"))
      (let ((org-agenda-files (list projects-file))
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
           (buffer-substring-no-properties (point-min) (point-max))))))))

(use-package-full content-quoter
  :repo-scan
  :ensure (:host github :repo "ultronozm/content-quoter.el"
                 :depth nil
                 :inherit nil :pin t)
  :bind ("s-u" . content-quoter-dwim)
  :defer t)

;;;; mcp

(use-package mcp-server-lib)

(use-package-full life-mail-mcp
  :load-path "~/repos/life-mail-mcp"
  :after mcp-server-lib
  :config
  (setopt life-mail-mcp-default-mail-file
          (expand-file-name "~/mail/inbox.rmail"))
  (life-mail-mcp-init))

(use-package-full elisp-dev-mcp
  :ensure (:host github :repo "laurynas-biveinis/elisp-dev-mcp"
                 :depth nil)
  :after mcp-server-lib
  :config
  (elisp-dev-mcp-enable))

(use-package-full mcp
  :ensure t
  :custom (mcp-hub-servers
           `(("filesystem" . (:command "npx"
                                       :args ("-y" "@modelcontextprotocol/server-filesystem")
                                       :roots ("/home/lizqwer/MyProject/")))
             ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
             ("qdrant" . (:url "http://localhost:8000/sse"))
             ("graphlit" . (
                            :command "npx"
                            :args ("-y" "graphlit-mcp-server")
                            :env (
                                  :GRAPHLIT_ORGANIZATION_ID "your-organization-id"
                                  :GRAPHLIT_ENVIRONMENT_ID "your-environment-id"
                                  :GRAPHLIT_JWT_SECRET "your-jwt-secret")))
             ("life-mail"
              . (:command "/Users/au710211/.emacs.d/emacs-mcp-stdio.sh"
                          :args ("--init-function=life-mail-mcp-init"
                                 "--stop-function=life-mail-mcp-stop")
                          :env (:EMACS_MCP_DEBUG_LOG "/tmp/life-mail.log")))))
  :config (require 'mcp-hub)
  :hook (after-init . mcp-hub-start-all-server))

;;;; agent-shell

;;;;; supporting packages

(use-package-full shell-maker
  :ensure (:host github :repo "xenodium/shell-maker"
                 :depth nil
                 :inherit nil
                 :pin t)
  :config
  (setopt shell-maker-transcript-default-path
          (my/agent-shell-transcripts-dir))
  (setopt shell-maker-transcript-default-filename
          (lambda ()
            (format "%s--transcript.txt"
                    (format-time-string "%Y%m%dT%H%M%S")))))

(use-package-full acp
  :ensure (:host github :repo "xenodium/acp.el"
                 :depth nil
                 :inherit nil
                 :pin t))

(defconst my/mcp-server-life-mail
  '((name . "life-mail")
    (command . "/Users/au710211/.emacs.d/emacs-mcp-stdio.sh")
    (args . ("--init-function=life-mail-mcp-init"
             "--stop-function=life-mail-mcp-stop"))
    (env  . (((name . "EMACS_MCP_DEBUG_LOG")
              (value . "/tmp/life-mail.log"))))))

(defconst my/mcp-server-elisp-dev
  '((name . "elisp-dev")
    (command . "/Users/au710211/.emacs.d/emacs-mcp-stdio.sh")
    (args . ("--init-function=elisp-dev-mcp-enable"
             "--stop-function=elisp-dev-mcp-disable"))
    (env  . (((name . "EMACS_MCP_DEBUG_LOG")
              (value . "/tmp/elisp-dev-mcp.log"))))))

(defun my/mcp-server-github (&optional token)
  (let ((pat (or token
                 (exec-path-from-shell-getenv "GITHUB_MCP_PAT")
                 (getenv "GITHUB_MCP_PAT"))))
    `((name . "github")
      (type . "http")
      (url . "https://api.githubcopilot.com/mcp")
      (headers . ,(when (and pat (not (string-empty-p pat)))
                    `(((name . "Authorization")
                       (value . ,(format "Bearer %s" pat)))))))))

(defconst my/mcp-server-fetch
  '((name . "fetch")
    (command . "uvx")
    (args . ("mcp-server-fetch"))))

(defconst my/mcp-server-filesystem
  '((name . "filesystem")
    (command . "npx")
    (args . ("-y" "@modelcontextprotocol/server-filesystem"
             "/home/lizqwer/MyProject/"))))

(defconst my/mcp-server-qdrant
  '((name . "qdrant")
    (type . "sse")
    (url . "http://localhost:8000/sse")))

;;;;; container support

(defconst my/agent-shell-workspace-root "/Users/Shared/workspace/"
  "Path prefix for projects that should run agent executables as `runner'.")

(defconst my/agent-shell-runner-prefix
  '("ssh" "-i" "~/.ssh/runner_localhost" "runner@localhost" "--")
  "Command prefix for running agent-shell backends as the `runner' user.")

(defconst my/agent-shell-sandbox-host "sandbox"
  "TRAMP host name for the sandbox remote.")

(defconst my/agent-shell-sandbox-runner-prefix
  '("ssh" "-T" "sandbox" "--")
  "Command prefix for running agent-shell backends on the sandbox remote.")

(defconst my/agent-shell-sandbox-codex-acp-command
  '("bash" "--login" "-lc" "codex-acp")
  "Run Codex ACP via a sandbox login shell so nvm-managed PATH is loaded.")

(defconst my/agent-shell-sandbox-claude-acp-command
  '("bash" "--login" "-lc" "claude-agent-acp")
  "Run Claude ACP via a sandbox login shell so nvm-managed PATH is loaded.")

(defun my/agent-shell--resolve-sandbox-path (path)
  "Translate between TRAMP paths and bare remote paths for sandbox.
Strips the TRAMP prefix when sending paths to the agent (e.g. CWD),
and prepends it when receiving paths from the agent (e.g. file reads)."
  (let ((tramp-prefix (concat "/ssh:" my/agent-shell-sandbox-host ":")))
    (cond
     ;; Emacs -> agent: strip TRAMP prefix
     ((string-prefix-p tramp-prefix path)
      (string-remove-prefix tramp-prefix path))
     ;; Agent -> Emacs: prepend TRAMP prefix
     ((and (file-name-absolute-p path)
           (not (file-remote-p path)))
      (concat tramp-prefix path))
     (t path))))

(defun my/agent-shell--set-container-runner-from-default-directory ()
  "Set `agent-shell-container-command-runner' based on `default-directory'.
Buffers rooted under `my/agent-shell-workspace-root' run agent backends
and shell tool calls as the `runner' user via SSH-to-localhost.
Buffers on the sandbox remote run agent backends on that host via SSH.
Other buffers run locally."
  (when (boundp 'agent-shell-container-command-runner)
    (let* ((dir (and (stringp default-directory)
                     (expand-file-name default-directory)))
           (in-workspace (and dir
                              (not (file-remote-p dir))
                              (file-in-directory-p dir my/agent-shell-workspace-root)))
           (on-sandbox (and dir
                            (equal (file-remote-p dir 'host)
                                   my/agent-shell-sandbox-host))))
      (setq-local agent-shell-container-command-runner
                  (cond
                   (in-workspace my/agent-shell-runner-prefix)
                   (on-sandbox my/agent-shell-sandbox-runner-prefix)
                   (t nil)))
      (when on-sandbox
        (when (boundp 'agent-shell-openai-codex-acp-command)
          (setq-local agent-shell-openai-codex-acp-command
                      my/agent-shell-sandbox-codex-acp-command))
        (when (boundp 'agent-shell-anthropic-claude-acp-command)
          (setq-local agent-shell-anthropic-claude-acp-command
                      my/agent-shell-sandbox-claude-acp-command))
        ;; Avoid migration warnings/errors from legacy variables.
        (when (boundp 'agent-shell-openai-codex-command)
          (setq-local agent-shell-openai-codex-command nil))
        (when (boundp 'agent-shell-anthropic-claude-command)
          (setq-local agent-shell-anthropic-claude-command nil))
        ;; Avoid remote icon cache fetches (via TRAMP temporary-file-directory)
        ;; which can fail when remote shell startup emits extra output.
        (when (boundp 'agent-shell-header-style)
          (setq-local agent-shell-header-style 'text)))
      (when (boundp 'agent-shell-text-file-capabilities)
        (setq-local agent-shell-text-file-capabilities
                    (not (or in-workspace on-sandbox))))
      (when (boundp 'agent-shell-path-resolver-function)
        (setq-local agent-shell-path-resolver-function
                    (when on-sandbox
                      #'my/agent-shell--resolve-sandbox-path)))
      (when (boundp 'agent-shell-anthropic-default-session-mode-id)
        (setq-local agent-shell-anthropic-default-session-mode-id
                    (when (or in-workspace on-sandbox) "bypassPermissions"))))))

(add-hook 'agent-shell-mode-hook
          #'my/agent-shell--set-container-runner-from-default-directory)

;;;;; region support

(defcustom my/agent-shell-discuss-text-template
  (string-join
   '("Let's discuss this excerpt."
     ""
     "File/buffer: %f"
     "Lines: %b-%e"
     ""
     "```"
     "%x"
     "```\n\n")
   "\n")
  "Template for discuss prompts built from text regions.

The following placeholders are replaced using `format-spec':
  %f file path (or buffer name)
  %b start line
  %e end line
  %x excerpt text."
  :type 'string
  :group 'reader)

(defcustom my/agent-shell-discuss-pdf-template
  (string-join
   '("Let's discuss this PDF excerpt."
     ""
     "PDF: %f"
     "Page: %p"
     ""
     "```"
     "%x"
     "```\n\n")
   "\n")
  "Template for discuss prompts built from PDF selections.

The following placeholders are replaced using `format-spec':
  %f PDF file path (or buffer name)
  %p page number
  %x excerpt text."
  :type 'string
  :group 'reader)

(defun my/agent-shell--discuss-selection ()
  "Return active discuss selection metadata, or nil when unavailable."
  (cond
   ((and (derived-mode-p 'pdf-view-mode)
         (fboundp 'pdf-view-active-region-p)
         (fboundp 'pdf-view-active-region-text)
         (pdf-view-active-region-p))
    (list :kind 'pdf
          :file (when-let* ((file (buffer-file-name)))
                  (abbreviate-file-name file))
          :page (if (fboundp 'pdf-view-current-page)
                    (number-to-string (pdf-view-current-page))
                  "?")
          :excerpt (mapconcat #'identity (pdf-view-active-region-text) "\n\n")))
   ((use-region-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (line-start (line-number-at-pos beg))
           (line-end (line-number-at-pos (if (> end beg) (1- end) end))))
      (list :kind 'text
            :file (when-let* ((file (buffer-file-name)))
                    (abbreviate-file-name file))
            :line-start (number-to-string line-start)
            :line-end (number-to-string line-end)
            :excerpt (buffer-substring-no-properties beg end))))
   (t nil)))

(defun my/agent-shell--format-discuss-prompt (selection)
  "Build discuss prompt text for SELECTION metadata."
  (let* ((kind (plist-get selection :kind))
         (template (if (eq kind 'pdf)
                       my/agent-shell-discuss-pdf-template
                     my/agent-shell-discuss-text-template))
         (file (or (plist-get selection :file)
                   (buffer-name)))
         (line-start (or (plist-get selection :line-start) "?"))
         (line-end (or (plist-get selection :line-end) "?"))
         (page (or (plist-get selection :page) "?"))
         (excerpt (or (plist-get selection :excerpt) "")))
    (format-spec template
                 `((?f . ,file)
                   (?b . ,line-start)
                   (?e . ,line-end)
                   (?p . ,page)
                   (?x . ,excerpt)))))

(defun my/agent-shell--start-discuss-with-config (agent-config)
  "Start AGENT-CONFIG shell and prefill prompt from active selection."
  (let* ((selection (my/agent-shell--discuss-selection))
         (prompt (and selection
                      (my/agent-shell--format-discuss-prompt selection)))
         (shell-buffer (agent-shell-start :config agent-config)))
    (when prompt
      (let ((inserted (agent-shell-insert :text prompt :shell-buffer shell-buffer)))
        (when-let* ((target-buffer (map-elt inserted :buffer))
                    ((buffer-live-p target-buffer)))
          (with-current-buffer target-buffer
            (let* ((inhibit-read-only t)
                   (start (map-elt inserted :start))
                   (end (map-elt inserted :end)))
              (when (and (integerp start)
                         (integerp end))
                (save-excursion
                  (goto-char start)
                  (when (looking-at-p "\n\n")
                    (delete-char 2)
                    (setq end (- end 2))))
                (goto-char end)
                (when-let* ((window (get-buffer-window target-buffer)))
                  (set-window-point window end))))))))
    shell-buffer))

(defun my/agent-shell-codex-discuss ()
  "Start Codex and prefill a discuss prompt from active selection."
  (interactive)
  (require 'agent-shell-openai)
  (my/agent-shell--start-discuss-with-config
   (agent-shell-openai-make-codex-config)))

(defun my/agent-shell-claude-discuss ()
  "Start Claude Code and prefill a discuss prompt from active selection."
  (interactive)
  (require 'agent-shell-anthropic)
  (my/agent-shell--start-discuss-with-config
   (agent-shell-anthropic-make-claude-code-config)))

;;;;; package

(defvar my-agent-shell-transcripts-dir nil
  "Directory for agent-shell transcript files.")

(defun my/agent-shell-transcripts-dir ()
  "Return the configured transcript directory.

Signal an error when `my-agent-shell-transcripts-dir' is unset."
  (if-let ((dir-name (my-setting-string 'my-agent-shell-transcripts-dir)))
      (file-name-as-directory (expand-file-name dir-name))
    (user-error "`my-agent-shell-transcripts-dir' is not set")))

(defun my/agent-shell-transcript-file-path-function ()
  "Generate a file path for saving agent shell transcripts."
  (expand-file-name
   (format "%s--transcript%s.txt"
           (format-time-string "%Y%m%dT%H%M%S")
           (concat
            (when-let* ((proj (project-current))
                        (name (project-name proj)))
              (with-temp-buffer
                (insert name)
                (goto-char (point-min))
                (when (re-search-forward "[a-z0-9][a-z0-9-]*$")
                  (concat "-" (match-string 0)))))))
   (my/agent-shell-transcripts-dir)))

(defun my/agent-shell-mode-hook ()
  (my/set-TeX-master-preview)
  (setq-local preview-tailor-local-multiplier 0.8))

(defun my/agent-shell-insert-org-timestamp ()
  "Insert an Org timestamp with time at point."
  (interactive)
  (org-timestamp '(16)))

(defun my/agent-shell-yank-fenced-block ()
  "Insert yanked text in a fenced code block."
  (interactive)
  (unless (bolp) (newline))
  (insert "```\n")
  (yank)
  (insert "\n```\n"))

(use-package-full agent-shell
  :ensure (:host github :repo "xenodium/agent-shell"
                 :depth nil
                 :inherit nil)
  :bind
  (:map agent-shell-mode-map
        ("o" . my/agent-shell-ui-toggle-fragment-at-point-or-self-insert)
        ("C-c t" . my/agent-shell-insert-org-timestamp)
        ("C-c m" . my/agent-shell-yank-fenced-block))
  (:map project-prefix-map
        ("z x" . my/agent-shell-codex-discuss)
        ("z c" . my/agent-shell-claude-discuss))
  :init
  (add-to-list 'project-switch-commands
               '(my/agent-shell-codex-discuss "Codex"))
  (add-to-list 'project-switch-commands
               '(my/agent-shell-claude-discuss "Claude"))
  :commands (agent-shell-openai-start-codex
             agent-shell-anthropic-start-claude-code)
  :hook
  ((agent-shell-mode . abbrev-mode)
   (agent-shell-mode . my/agent-shell-mode-hook))
  :config
  (setopt agent-shell-session-strategy 'prompt)
  (setopt agent-shell-openai-authentication
          (agent-shell-openai-make-authentication :login t))
  (setopt agent-shell-anthropic-default-model-id "opus")

  (setopt agent-shell-file-completion-enabled nil)
  (setopt agent-shell-openai-codex-environment
          (agent-shell-make-environment-variables
           "GITHUB_MCP_PAT" (exec-path-from-shell-getenv "GITHUB_MCP_PAT")))
  (setopt agent-shell-mcp-servers
          (list
           ;; my/mcp-server-elisp-dev
           ;; my/mcp-server-life-mail
           ;; (my/mcp-server-github)
           ))
  (setopt agent-shell-transcript-file-path-function
          #'my/agent-shell-transcript-file-path-function))

(defun my/agent-shell--call-or-self-insert (command)
  "Run COMMAND unless we should insert at the prompt instead.

COMMAND is a function (typically an agent-shell motion) that is invoked
with `funcall' when the user is not typing at the last prompt.  When
the user is at the last prompt, not busy, and presses a character key,
insert the character instead of invoking COMMAND."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (if (and (not (shell-maker-busy))
           (shell-maker-point-at-last-prompt-p)
           (integerp last-command-event))
      (self-insert-command 1)
    (funcall command)))

(defun my/agent-shell-ui-toggle-fragment-at-point-or-self-insert ()
  "Toggle visibility of fragment body at point or insert at prompt.

Behaves like `agent-shell-ui-toggle-fragment-at-point', but if point is
at the input prompt and a character key was pressed, insert the
character instead of toggling."
  (interactive)
  (my/agent-shell--call-or-self-insert #'agent-shell-ui-toggle-fragment-at-point))

;; (use-package-full preview-auto
;;   :after preview
;;   :demand
;;   :hook (LaTeX-mode . preview-auto-setup)
;;   :config
;;   (setopt preview-LaTeX-command-replacements
;;           '(preview-LaTeX-disable-pdfoutput))
;;   (setq preview-protect-point t)
;;   (setq preview-locating-previews-message nil)
;;   (setq preview-leave-open-previews-visible t))

;;;;; attention

(use-package-full knockknock
  :defer t
  :ensure (:host github :repo "konrad1977/knockknock"
                 :depth nil
                 :inherit nil
                 :pin t))

(use-package-full agent-shell-attention
  :repo-scan
  :ensure (:host github :repo "ultronozm/agent-shell-attention.el"
                 :depth nil
                 :inherit nil :pin t)
  :after agent-shell
  :demand
  :bind (("C-z a" . agent-shell-attention-jump))
  :config
  (require 'knockknock)
  (setopt agent-shell-attention-notify-function
          (lambda (_buffer title body)
            (knockknock-notify
             :title title
             :message (mapconcat
                       #'identity
                       (seq-take (string-lines body) 3)
                       "\n")
             :icon "nf-cod-bot"
             :duration 5)))
  (setopt agent-shell-attention-render-function
          #'agent-shell-attention-render-active)
  (setopt agent-shell-attention-indicator-location 'global-mode-string)
  (agent-shell-attention-mode))


;;;; codex CLI wrapper

(defvar codex-exec-command-history nil
  "Minibuffer history for `codex-exec-run' prompts.")

(defun codex-exec-run (instruction)
  (interactive
   (list (read-string "Codex instruction: " nil 'codex-exec-command-history)))
  (let* ((buffer (generate-new-buffer "*codex-exec*"))
         proc)
    (with-current-buffer buffer
      (compilation-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "$ codex exec --sandbox workspace-write %S\n\n"
                        instruction))))
    (setq proc
          (apply #'start-process
                 "codex-exec" buffer
                 "codex" "exec"
                 "--sandbox" "workspace-write"
                 (list instruction)))
    (with-current-buffer buffer
      (set-marker (process-mark proc) (point)))
    (pop-to-buffer buffer)))

;;; erc

(use-package-full erc
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
(use-package-full erc-log
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

(use-package-full erc-desktop-notifications
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

(use-package-full cc-mode
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

(use-package-full cmake-build
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

(use-package-full czm-cpp
  :repo-scan
  :ensure (:host github :repo "ultronozm/czm-cpp.el" :files ("*.el" "template") :depth nil
                 :inherit nil :pin t)
  :defer t
  :custom
  (czm-cpp-scratch-directory my-scratch-cpp-dir))

(add-to-list 'auto-mode-alist '("\\.ixx\\'" . c++-mode))

(use-package-full c-ts-mode
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

(use-package-full cmake-ts-mode
  :ensure nil
  :defer t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :config
  (require 'treesit-auto))

;;; calc

(defmacro with-calc-language (lang &rest body)
  "Execute the forms in BODY with `calc-language` set to LANG.
The value of `calc-language` is restored after BODY has been processed."
  `(let ((old-lang calc-language))
     (unwind-protect
         (progn
           (calc-set-language ,lang)
           ,@body)
       (calc-set-language old-lang))))

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

;;; git

(use-package czm-vc
  :ensure (:host github :repo "ultronozm/czm-vc.el"
                 :depth nil
                 :inherit nil)
  :after vc
  :config
  (with-eval-after-load 'vc
    (keymap-set vc-prefix-map "K" #'czm-vc-root-shortlog-all)
    (keymap-set vc-prefix-map "C" #'czm-vc-diff-staged)
    (keymap-set vc-prefix-map "N" #'czm-vc-create-directory-with-git-repo)
    (keymap-set vc-prefix-map "z" #'czm-vc-git-sparse-map))
  (with-eval-after-load 'vc-git
    (keymap-global-set "C-x C-g" #'czm-vc-switch-to-git-status-file)
    (add-hook 'vc-git-log-edit-mode-hook
              #'czm-vc-git-log-edit-add-orig-head-to-future-history))
  (with-eval-after-load 'vc-dir
    (keymap-set vc-dir-mode-map "C-c d" #'czm-vc-dir-dired-marked))
  (with-eval-after-load 'log-view
    (keymap-set log-view-mode-map "E" #'czm-vc-git-fixup-staged)
    (keymap-set log-view-mode-map "w" #'czm-vc-log-view-copy-revision-or-range-as-kill)
    (keymap-set log-view-mode-map "o" #'log-view-toggle-entry-display))
  (with-eval-after-load 'project
    (keymap-set project-prefix-map "P" #'czm-vc-project-format-patch-last-commit))
  (with-eval-after-load 'diff-mode
    (keymap-set diff-mode-map "C-c d" #'czm-vc-diff-dired-changed-files))
  (with-eval-after-load 'embark
    (czm-vc-embark-setup)
    (my-embark--insert-target-finder-before
     #'czm-vc-embark-target-git-commit-at-point
     #'embark-target-identifier-at-point)))



(use-package transient
  :ensure t
  :demand t)

(use-package magit
  :defer t
  :hook
  (magit-status-mode . visual-line-mode)
  :init
  (add-to-list 'project-switch-commands '(magit-project-status "Magit"))
  :config
  (setopt magit-commit-diff-inhibit-same-window t)
  (add-to-list 'display-buffer-alist
               '("\\`\\*?magit-diff:.*\\*?\\'"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.45)
                 (window-parameters . ((no-delete-other-windows . t)))))
  :bind
  (:repeat-map
   magit-smerge-repeat-map
   ("RET" . magit-smerge-keep-current)
   ("b" . magit-smerge-keep-base)
   ("l" . magit-smerge-keep-lower)
   ("u" . magit-smerge-keep-upper))
  (:map
   vc-prefix-map
   ("b C" . magit-branch-configure)
   ("b d" . magit-branch-delete)
   ("b r" . magit-rebase-branch)
   ("b x h" . magit-reset-hard)
   ("b x m" . magit-reset-mixed))
  (:map project-prefix-map
        ("m" . magit-project-status)))

(setq auth-sources '("~/.authinfo.gpg"))

(use-package forge
  :ensure
  :after magit)

(defun czm-file-is-tex-or-bib (file)
  "Return t if FILE is a .tex or .bib file."
  (or (string-suffix-p ".tex" file)
      (string-suffix-p ".bib" file)))

(use-package-full publish
  :repo-scan
  :ensure (:host github :repo "ultronozm/publish.el" :depth nil)
  :defer t
  :custom
  (publish-repo-root my-publish-math-repo)
  (publish-disallowed-unstaged-file-predicate #'czm-file-is-tex-or-bib))

(use-package-full magit-fill-column
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

(use-package-full llm-vc-commit
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
         :chat-model "claude-sonnet-4-6")))

(defun my/diff-hl-mode-hook ()
  "Enable `diff-hl-margin-mode' in `lean4-mode' buffers."
  (when (eq major-mode 'lean4-mode)
    (diff-hl-margin-local-mode (if diff-hl-mode 1 -1))))

(use-package diff-hl
  :defer t
  :bind
  ("H-d" . diff-hl-mode)
  ("H-D" . diff-hl-set-reference-rev-in-project)
  :config
  (add-hook 'diff-hl-mode-hook #'my/diff-hl-mode-hook)
  (setopt diff-hl-goto-hunk-old-revisions t)
  (setopt diff-hl-show-staged-changes nil)
  (set-face-attribute 'diff-hl-insert nil
                      :background "#00ff00"
                      :foreground "#00ff00")
  (set-face-attribute 'diff-hl-delete nil
                      :background "#ff0000"
                      :foreground "#ff0000")
  (set-face-attribute 'diff-hl-change nil
                      :background "#ff00ff"
                      :foreground "#ff00ff")
  (set-face-attribute 'diff-hl-reference-insert nil
                      :background "#008800"
                      :foreground "#008800")
  (set-face-attribute 'diff-hl-reference-delete nil
                      :background "#880000"
                      :foreground "#880000")
  (set-face-attribute 'diff-hl-reference-change nil
                      :background "#880088"
                      :foreground "#880088"))

(use-package-full repo-scan
  :repo-scan
  :ensure (:host github :repo "ultronozm/repo-scan.el" :depth nil)
  :defer t)

;;; edit-indirect


(defun my/edit-indirect-setup ()
  (setq fill-column 999999)
  (setq-local dynexp-math-delimiters 'paren)
  (my/maybe-set-preview-master-local)
  (preview-auto-mode))

(defun my/maybe-edit-indirect-setup ()
  (when (memq major-mode '(LaTeX-mode latex-mode))
    (my/edit-indirect-setup)))

(defun my/edit-indirect-region-elisp (beg end)
  (interactive (progn
                 (unless (use-region-p)
                   (user-error "No active region"))
                 (list (region-beginning) (region-end))))
  (let ((edit-indirect-guess-mode-function
         (lambda (_parent _beg _end)
           (emacs-lisp-mode))))
    (edit-indirect-region beg end t)))

(defun my/edit-indirect-region-LaTeX (beg end)
  (interactive (progn
                 (unless (use-region-p)
                   (user-error "No active region"))
                 (list (region-beginning) (region-end))))
  (let ((edit-indirect-guess-mode-function
         (lambda (_parent _beg _end)
           (if (fboundp 'LaTeX-mode)
               (LaTeX-mode)
             (latex-mode)))))
    (edit-indirect-region beg end t)))

(use-package-full edit-indirect
  :ensure (:host github :repo "Fanael/edit-indirect"
                 :depth nil
                 :inherit nil
                 :pin t)
  :commands (edit-indirect-region
             edit-indirect-commit
             edit-indirect-abort edit-indirect-save)
  :demand
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*edit-indirect"
                 (display-buffer-reuse-window display-buffer-below-selected)))
  (setopt edit-indirect-empty-region-indicator "|")
  (add-hook 'edit-indirect-after-creation-hook #'my/maybe-edit-indirect-setup)
  (with-eval-after-load 'embark
    (keymap-set embark-region-map "L" #'my/edit-indirect-region-LaTeX)))

;;; latex

(defun my-LaTeX-mode-setup ()
  (turn-on-reftex)
  (apply #'LaTeX-add-environments
         (mapcar (lambda (env) (list env 'LaTeX-env-label))
                 '("lemma" "exercise" "example" "proposition"
                   "corollary" "remark" "definition" "theorem"
                   "proof")))
  (when my-latex-buffer-face
    (setq buffer-face-mode-face my-latex-buffer-face)
    (buffer-face-mode))
  (outline-minor-mode)
  (abbrev-mode)
  (visual-line-mode)
  (setq fill-column 999999)
  (czm-setup-and-activate-tex-fold)
  (local-set-key (kbd "C-c m") #'my-LaTeX-math-map)
  (setq-local preview-tailor-local-multiplier 1.2))

(defun my-TeX-view-master ()
  "View the entire TeX document, ignoring any active region.
Unlike `TeX-view', this command always views the master file,
even if you've recently performed operations on a region.
This is useful when you want to ensure you're viewing the
complete document rather than just a previewed region."
  (interactive)
  (let ((TeX-current-process-region-p nil))
    (call-interactively #'TeX-view)))

(use-package-full latex
  :ensure `(auctex
            :host nil
            :repo ,(if my-auctex-git-permissions
                       "git@git.savannah.gnu.org:/srv/git/auctex.git"
                     "https://git.savannah.gnu.org/git/auctex.git")
            :depth nil
            :inherit nil
            :pre-build (("sh" "-lc" "cd doc && makeinfo --no-split auctex.texi")
                        ("sh" "-lc" "cd doc && makeinfo --no-split preview-latex.texi"))
            :build (:not elpaca--compile-info)
            :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style"))
  ;; :demand                             ; otherwise, madness ensues.
  :config
  (setopt preview-image-type 'dvi*)
  (setq TeX-data-directory (expand-file-name "elpaca/builds/auctex" user-emacs-directory))
  (setq TeX-lisp-directory TeX-data-directory)
  (add-to-list 'TeX-file-extensions "tex#?\\.~[^~]+~")
  (with-eval-after-load 'org-src
    (push '("latex" . LaTeX) org-src-lang-modes))
  (put 'LaTeX-narrow-to-environment 'disabled nil)
  (TeX-source-correlate-mode)
  (let ((cmds '(other-window ace-window consult-register-load
                             next-line previous-line
                             beginning-of-buffer end-of-buffer)))
    (with-eval-after-load 'preview
      (dolist (cmd cmds)
        (add-to-list 'preview-auto-reveal-commands cmd)))
    (with-eval-after-load 'tex-fold
      (dolist (cmd cmds)
        (add-to-list 'TeX-fold-auto-reveal-commands cmd))))
  (advice-add 'text-scale-adjust :after (lambda (&rest _) (preview-clearout-buffer)))
  (advice-add 'global-text-scale-adjust :after (lambda (&rest _) (preview-clearout-buffer)))
  (defun czm-tex-font-fold-advice (&rest _)
    "Advice to fold macros after `TeX-font' is called."
    (when TeX-fold-mode
      (save-excursion
        (when (looking-at "}")
          (forward-char))
        (when (looking-back "[{}]" (- (point) 1))
          (backward-sexp))
        (let ((macro-start (point)))
          (forward-sexp)
          (font-lock-ensure macro-start (point))
          (goto-char macro-start)
          (TeX-fold-macro)))))
  (advice-add 'TeX-font :after #'czm-tex-font-fold-advice)
  :hook
  (LaTeX-mode . my-LaTeX-mode-setup)
  (TeX-mode . prettify-symbols-mode)
  (prog-mode . my/maybe-set-preview-master-local)
  :bind
  (:map LaTeX-mode-map
        ("C-c C-g" . czm-latex-calc-grab)
        ("C-c C-n" . nil) ; TeX-normal-mode
        ("C-c #" . nil)
        ("C-c i" . LaTeX-make-inline)
        ("C-c p e" . LaTeX-repeat-recent-math)
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
  ;; (TeX-fold-quotes-on-insert t)
  (TeX-fold-bib-files (list my-master-bib-file))
  (TeX-ignore-warnings "Package hyperref Warning: Token not allowed in a PDF string")
  (TeX-insert-macro-default-style 'mandatory-args-only)
  ;; (TeX-suppress-ignored-warnings t)
  (LaTeX-insert-into-comments nil)
  :custom-face
  (preview-face ((t (:background unspecified)))))

(defvar-keymap tex-error-repeat-map
  :doc "Keymap to repeat TeX error navigation commands.
Used in `repeat-mode'."
  :repeat t
  "`" #'TeX-next-error
  "~" #'TeX-previous-error)

(defun my-LaTeX-toggle-numbered ()
  "Convert math construct at point to \"equation*\".
If the math construct is already \"equation*\", then toggle with the
numbered variant \"equation\"."
  (interactive)
  (unless (texmathp) (user-error "Not inside math"))
  (let ((current (car texmathp-why)))
    (LaTeX-modify-math
     (pcase current
       ("equation*" "equation")
       ("equation" "equation*")
       (_ "equation*")))))

(defun my-LaTeX-toggle-align ()
  "Toggle math environment at point between \"equation\" and \"align\"."
  (interactive)
  (unless (texmathp) (user-error "Not inside math"))
  (let ((current (car texmathp-why)))
    (LaTeX-modify-math
     (pcase current
       ("align*" "equation*")
       ("equation*" "align*")
       ("align" "equation")
       ("equation" "align")
       (_ "align*")))))

(defun my-LaTeX-toggle-multline ()
  "Toggle math environment at point between \"equation\" and \"multline\"."
  (interactive)
  (unless (texmathp) (user-error "Not inside math"))
  (let ((current (car texmathp-why)))
    (LaTeX-modify-math
     (pcase current
       ("multline*" "equation*")
       ("equation*" "multline*")
       ("multline" "equation")
       ("equation" "multline")
       (_ "multline*")))))

(defvar-keymap my-LaTeX-math-map
  :doc "Math conversion/toggle commands for LaTeX."
  :prefix t
  "c" #'latex-math-from-calc
  "n" #'my-LaTeX-toggle-numbered
  "a" #'my-LaTeX-toggle-align
  "m" #'my-LaTeX-toggle-multline)

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

(use-package-full preview-tailor
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

(use-package-full czm-tex-util
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

(use-package-full czm-pythontex
  :ensure (:host github :repo "ultronozm/czm-pythontex.el"
                 :depth nil
                 :inherit nil)
  :after latex
  :bind (:map LaTeX-mode-map
              ("C-c '" . czm-tex-pythontex-edit-indirect))
  :config
  (czm-tex-pythontex-flush-left-begin-end-mode 1))

(defun czm-setup-and-activate-tex-fold ()
  (require 'czm-pythontex)
  (require 'czm-tex-jump)
  (require 'czm-tex-ref)
  (setq TeX-fold-macro-spec-list
        (seq-remove (lambda (item)
                      (and (numberp (car item))
                           (= (car item) 1)))
                    TeX-fold-macro-spec-list))
  (dolist (item `(("[{2}]||[href]" ("href"))
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
                  (1 ("emph" "textit" "textsl" "textmd" "textrm" "textsf"
                      "texttt" "textbf" "textsc" "textup"))
                  (,(lambda (text) (propertize text 'face '(underline)))
                   ("underline"))
                  (,(lambda (text) (propertize text 'face '(:strike-through t)))
                   ("sout"))))
    (add-to-list 'TeX-fold-macro-spec-list item))
  (dolist (item `((1 ("mathrm"))))
    (add-to-list 'TeX-fold-macro-spec-list item))
  (dolist (item `((("🌅" . "🌇") ("document"))
                  (("⚡" . "⚡") ("minted" "minted*"))
                  ((czm-tex-fold-format-pythontex-environment . "⚡")
                   ,czm-tex-pythontex-environments)
                  (("♣" . "♣") ("results" "results*"))
                  ((TeX-fold-format-theorem-environment . "◼")
                   ("idea" "solution" "quote"))))
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

(use-package-full czm-tex-jump
  :repo-scan
  :ensure (:host github :repo "https://github.com/ultronozm/czm-tex-jump.el.git" :depth nil
                 :inherit nil :pin t)
  ;; :after avy
  :after latex
  :bind
  (:map LaTeX-mode-map
        ("s-r" . czm-tex-jump))
  :hook (LaTeX-mode . czm-tex-jump-setup))

(use-package-full czm-tex-ref
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

(use-package-full dynexp
  :repo-scan
  :ensure (:host github :repo "ultronozm/dynexp.el" :depth nil)
  :after latex
  :hook (LaTeX-mode . dynexp-latex-setup)
  :bind (:map LaTeX-mode-map
              ("SPC" . dynexp-space)
              ("TAB" . dynexp-next)))

(use-package-full czm-tex-edit
  :repo-scan
  :ensure (:host github :repo "ultronozm/czm-tex-edit.el" :depth nil
                 :inherit nil :pin t)
  :after latex dynexp
  ;; :demand ; should come after latex and dynexp
  :bind
  (:map LaTeX-mode-map
        ;; ("C-c t a" . czm-tex-edit-alertify) ;; add to TeX-font?
        ;; ("C-c t l" . czm-tex-edit-underline)
        ;; ("C-c t u" . czm-tex-edit-unemphasize)
        ("C-c t e" . czm-tex-edit-external-document-link)
        ;; ("C-c p e" . czm-tex-edit-repeat-most-recent-equation)
        ;; ("C-c p d" . czm-tex-edit-repeat-line-contents)
        ;; ("C-c p r" . czm-tex-edit-repeat-region)
        ("C-c p s" . czm-tex-edit-substackify)
        ;; ("C-c p i" . czm-tex-edit-yank-interior-delete-delim)
        ("C-c p f" . czm-tex-edit-fractionify-region)
        ;; ("C-c p b" . czm-tex-edit-enlarge-parentheses)
        ("C-c p h" . czm-tex-edit-split-equation)
        ("C-<return>" . czm-tex-edit-return)
        ;; ("$" . czm-tex-edit-insert-dollar-or-wrap-region) ; not necessary w/ electric-pair-mode?
        ("\"" . czm-tex-edit-insert-quote-or-wrap-region))
  :config
  (czm-tex-edit-define-color-functions-and-bindings
   "C-c t c"
   (("red" . "r") ("green" . "g") ("blue" . "b") ("yellow" . "y") ("orange" . "o") ("purple" . "p") ("black" . "k") ("white" . "w") ("cyan" . "c") ("magenta" . "m") ("lime" . "l") ("teal" . "t") ("violet" . "v") ("pink" . "i") ("brown" . "n") ("gray" . "a") ("darkgreen" . "d") ("lightblue" . "h") ("lavender" . "e") ("maroon" . "u") ("beige" . "j") ("indigo" . "x") ("turquoise" . "q") ("gold" . "f") ("silver" . "s") ("bronze" . "z"))))

(use-package-full auctex-cont-latexmk
  :repo-scan
  :ensure (:host github :repo "ultronozm/auctex-cont-latexmk.el" :depth nil)
  :after latex
  :bind (:map LaTeX-mode-map ("C-c k" . auctex-cont-latexmk-toggle))
  :custom
  (auctex-cont-latexmk-command
   '("latexmk -pvc -shell-escape -pdf -view=none -e "
     ("$pdflatex=q/pdflatex %O -synctex=1 -file-line-error -interaction=nonstopmode %S/"))))

(defun my/preview-auto-mode-ensure-TeX-master (arg)
  (interactive "P")
  (if arg
      (progn
        (preview-auto-mode -1)
        (preview-clearout-buffer))
    (when (bound-and-true-p TeX-master)
      (my/maybe-set-preview-master-local))
    (preview-auto-mode (if preview-auto-mode -1 1))))

(use-package-full preview-auto
  :repo-scan
  :ensure (:host github :repo "ultronozm/preview-auto.el" :depth nil)
  :after latex
  :hook (LaTeX-mode . preview-auto-setup)
  :init
  (keymap-global-set "H-r" #'my/preview-auto-mode-ensure-TeX-master)
  :config
  (setopt preview-LaTeX-command-replacements
          '(preview-LaTeX-disable-pdfoutput))
  (setq preview-protect-point t)
  (setq preview-locating-previews-message nil)
  (setq preview-leave-open-previews-visible t)
  (defun czm-setup-tex-file--local-common-tex-file ()
    "Return local path to common.tex for current buffer, or nil."
    (let* ((dir (file-name-directory (or buffer-file-name default-directory)))
           (path (and dir (expand-file-name "common.tex" dir))))
      (when (and path (file-readable-p path))
        path)))
  (defun czm-setup-tex-file-experimental ()
    "Experimental command-based replacement for `czm-setup-tex-file'."
    (interactive)
    (insert "ltx")
    (call-interactively #'dynexp-space)
    (save-buffer)
    (call-interactively #'outline-previous-heading)
    (call-interactively #'foldout-zoom-subtree)
    (forward-line 2)
    (if (czm-setup-tex-file--local-common-tex-file)
        (progn
          (unless (bound-and-true-p preview-auto-mode)
            (preview-auto-mode 1))
          ;; Defer preamble caching so preview-auto/timers settle first.
          (let ((buf (current-buffer)))
            (run-with-timer
             0.05 nil
             (lambda (b)
               (when (buffer-live-p b)
                 (with-current-buffer b
                   (ignore-errors
                     (call-interactively #'preview-cache-preamble)))))
             buf)))
      (message
       "czm-setup-tex-file: missing `common.tex' in %s; skipping preview setup (M-x czm-copy-standard-tex-files)"
       (abbreviate-file-name
        (or (file-name-directory (or buffer-file-name default-directory))
            default-directory)))))
  (defalias 'czm-setup-tex-file
    (kmacro "l t x SPC s-s C-c o p z C-n C-n C-c C-p C-a C-c C-p C-f"))
  (add-to-list 'auto-insert-alist
               '("\\.tex\\'" . czm-setup-tex-file-experimental))
  (setopt preview-auto-chars-above 1200)
  (setopt preview-auto-chars-below 1800)
  (setopt preview-auto-interval 0.3))

(use-package-full buframe)

(use-package-full auctex-label-numbers
  :repo-scan
  :ensure (:host github :repo "ultronozm/auctex-label-numbers.el" :depth nil)
  :after latex)

(use-package-full library
  :repo-scan
  :after latex czm-tex-util
  :defer t
  :ensure (:host github :repo "ultronozm/library.el" :depth nil)
  :custom
  (library-pdf-directory my-pdf-folder)
  (library-bibtex-file my-master-bib-file)
  (library-download-directory my-downloads-folder)
  (library-org-capture-template-key "j"))

(use-package-full tex-parens
  :repo-scan
  :ensure (:host github :repo "ultronozm/tex-parens.el" :depth nil)
  :after latex
  :config
  (repeat-mode 1)
  (defun czm-tex-jump-back-with-breadcrumb ()
    (interactive)
    (save-excursion (insert "<++>"))
    (call-interactively #'tex-parens-backward-down-list))
  (add-to-list 'preview-auto-reveal-commands #'czm-tex-jump-back-with-breadcrumb)
  (bind-keys
   :map tex-parens-structural-edit-map
   ("n" . tex-parens-forward-list)
   ("p" . tex-parens-backward-list)
   ("u" . tex-parens-backward-up-list)
   ("M-u" . tex-parens-up-list)
   ("g" . tex-parens-down-list)
   ("M-g" . tex-parens-backward-down-list)
   ("f" . tex-parens-forward-sexp)
   ("b" . tex-parens-backward-sexp)
   ("a" . tex-parens-beginning-of-list)
   ("A" . tex-parens-kill-to-beginning-of-list)
   ("e" . tex-parens-end-of-list)
   ("E" . tex-parens-kill-to-end-of-list)
   ("i" . tex-parens-mark-inner)
   ("[" . beginning-of-defun)
   ("]" . end-of-defun)
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
   ("RET" . TeX-newline))
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
  :hook
  (LaTeX-mode . tex-parens-mode))

(use-package-full tex-item
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

;;; sage

(use-package-full sage
  :ensure (:host nil :repo "https://codeberg.org/ultronozm/sage-mode"
                 :remotes
                 (("upstream" :repo "https://codeberg.org/rahguzar/sage-mode"))
                 :depth nil
                 :main "sage.el"
                 :inherit nil)
  :defer t
  :config
  (defun my/setup-sage ()
    "Set up completion for Sage mode."
    (setq-local completion-styles '(basic))
    (setq-local corfu-sort-function 'nil)
    (corfu-mode)
    (setq-local orderless-component-separator (rx (or "_" ".")))
    (setq-local gud-pdb-command-name "sage -python -m pdb"))
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

(use-package-full mmm-mode
  :defer t
  :custom (mmm-global-mode nil)
  :config
  (face-spec-set 'mmm-default-submode-face
                 '((((background light)) (:background "#ddffff"))
                   (((background dark)) (:background "#004444")))
                 'face-defface-spec))

(use-package-full czm-tex-mint
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

(use-package-full symtex
  :repo-scan
  :ensure (:host github :repo "ultronozm/symtex.el" :depth nil)
  :after latex
  :bind
  (:map global-map ("C-c V" . symtex-process))
  (:map LaTeX-mode-map ("C-c v" . symtex-dwim)))

;;; lean

(defun czm-set-lean4-local-variables ()
  (setq-local preview-tailor-local-multiplier 0.7)
  (my/maybe-set-preview-master-local)
  ;; lean4-mode's non-plain info buffer uses a synchronous JSON-RPC
  ;; request (`$/lean/rpc/connect`), which can make Emacs feel stuck
  ;; over TRAMP.  Prefer the async plain-goal path for remote buffers.
  (when (file-remote-p default-directory)
    (setq-local lean4-info-plain t)
    (setq-local lean4-info-refresh-even-if-invisible nil)
    (setq-local lean4-idle-delay 0.2)))

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
  (lean4-info-plain t)
  (lean4-info-refresh-even-if-invisible nil)
  :bind (:map lean4-mode-map
              ("C-c C-k" . quail-show-key))
  :config
  (setopt lean4-idle-delay 0.2)
  (setopt lean4-auto-start-eglot nil)
  (add-to-list 'global-auto-revert-ignore-modes 'lean4-mode)
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
    (require 'flymake-overlays)
    (set-face-attribute 'flymake-overlays-face nil :background
                        "#fff59d" :foreground "black")
    (set-face-attribute 'czm-lean4-overlay-face nil :background
                        "#fff59d" :foreground "black"))
  (advice-add 'lean4-info-buffer-redisplay :around #'czm-lean4-info-buffer-redisplay)
  (with-eval-after-load 'copilot
    (when (boundp 'copilot-completion-map)
      (map-keymap
       (lambda (key cmd)
         (define-key lean4-mode-map (vector key) cmd))
       copilot-completion-map))))

(use-package flymake-overlays
  :repo-scan
  :ensure (:host github :repo "ultronozm/flymake-overlays.el" :depth nil)
  :after flymake
  :bind (:map flymake-mode-map
              ;; ("C-c t" . flymake-overlays-smart-toggle)
              )
  ;; :hook (flymake-mode . flymake-overlays-mode)
  )

(use-package lean4-indent
  :ensure (:host github
                 :repo "ultronozm/lean4-indent.el"
                 :depth nil)
  :demand t
  :after lean4-mode
  :config
  (add-hook 'lean4-mode-hook #'lean4-indent-setup-buffer))

(use-package lean4-imenu
  :ensure (:host github
                 :repo "ultronozm/lean4-imenu.el"
                 :depth nil)
  :demand t
  :after lean4-mode
  :config
  (add-hook 'lean4-mode-hook
            (lambda ()
              (setq-local eglot-stay-out-of (cons 'imenu eglot-stay-out-of))
              (setq-local imenu-create-index-function #'lean4-imenu-create-index))))

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

;;; misc

(let ((file (locate-user-emacs-file "init-personal.el")))
  (when (file-exists-p file)
    (load file)))

(run-with-idle-timer
 5 nil
 (lambda ()
   (require 'server)
   (unless (server-running-p)
     (server-start))))

(setopt python-indent-guess-indent-offset nil)

(use-package-full clipdiff
  :repo-scan
  :defer t
  :ensure (:host github :repo "ultronozm/clipdiff.el" :depth nil))

(use-package-full cython-mode
  :ensure t
  :defer t)

(use-package vundo
  :ensure t
  :defer t
  :init
  (defun my/vundo (arg)
    "With `C-u' prefix argument, call `vundo'.  Otherwise, call `undo'."
    (interactive "P")
    (if (equal arg '(4))
        (vundo)
      (undo arg)))
  :config
  (setopt vundo-use-region-undo t)
  :bind (([remap undo] . my/vundo)))

(with-eval-after-load 'mailcap
  (dolist (item
           '((".docx" .
              "application/vnd.openxmlformats-officedocument.wordprocessingml.document")))
    (add-to-list 'mailcap-mime-extensions item)))

(use-package-full osx-dictionary
  :bind (("C-z w" . osx-dictionary-search-word-at-point))
  :config
  (defun my-osx-dictionary--candidate ()
    "Return the best lookup candidate for `osx-dictionary'."
    (or (and (use-region-p)
             (buffer-substring-no-properties (region-beginning)
                                             (region-end)))
        (thing-at-point 'symbol t)
        (thing-at-point 'word t)
        (osx-dictionary--region-or-word)))

  (defun my-osx-dictionary-search (&optional word)
    "Look up WORD (a string) using `osx-dictionary'.
When used via Embark, WORD comes from the current target."
    (interactive (list (my-osx-dictionary--candidate)))
    (let* ((raw (or word (my-osx-dictionary--candidate)))
           (clean (and raw (string-trim (substring-no-properties raw)))))
      (unless (and clean (> (length clean) 0))
        (user-error "No word at point or region"))
      (osx-dictionary--view-result clean)))
  (with-eval-after-load 'embark
    (dolist (map '(embark-symbol-map embark-identifier-map embark-region-map))
      (when (boundp map)
        (keymap-set (symbol-value map) "C-d" #'my-osx-dictionary-search)))))

(with-eval-after-load 'image-mode
  (keymap-unset image-mode-map "W"))

(use-package-full diff-vc-patch
  :ensure (:host github :repo "ultronozm/diff-vc-patch.el"
                 :depth nil
                 :inherit nil)
  :after diff-mode
  :demand
  :hook (diff-mode . diff-vc-patch-mode))

(use-package-full speedread
  :ensure (:host github :repo "ultronozm/speedread.el"
                 :depth nil
                 :inherit nil)
  :defer t
  :config
  (setopt speedread-display-style 'window)
  (setopt iread-chars 20))

(use-package-full code-cells)

(use-package-full python-repl-eldoc
  :ensure (:host github :repo "ultronozm/python-repl-eldoc.el"
                 :depth nil
                 :inherit nil)
  :after python
  :demand
  :config
  (python-repl-eldoc-global-mode 1))

(use-package-full overleaf
  :defer 5
  :ensure (:host github
                 :repo "ultronozm/overleaf.el"
                 :remotes (("upstream" :repo "vale981/overleaf.el"))
                 :depth nil
                 :inherit nil
                 :pin t)
  :config
  (with-eval-after-load 'overleaf
    (add-hook 'overleaf-mode-hook
              (lambda ()
                (when (bound-and-true-p aggressive-indent-mode)
                  (aggressive-indent-mode -1)))))
  (let ((cookie-file (expand-file-name "~/.overleaf-cookies.gpg")))
    (setopt overleaf-save-cookies (overleaf-save-cookies-to-file cookie-file))
    (setopt overleaf-cookies
            (overleaf-read-cookies-from-firefox
             :firefox-folder (expand-file-name my-firefox-folder)
             :profile "default-release")))
  (with-eval-after-load 'latex
    (keymap-set LaTeX-mode-map "C-c O" overleaf-command-map))
  (with-eval-after-load 'bibtex
    (keymap-set bibtex-mode-map "C-c O" overleaf-command-map)))
