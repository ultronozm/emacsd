;; loaded from ~/gnu-emacs/lisp/site-start.el
(setq ns-command-modifier 'meta)
(setq ns-alternate-modifier 'super)
(setq w32-lwindow-modifier 'super)
(if (string-equal system-type "windows-nt" )
    (progn
      (w32-register-hot-key [s-])
      (w32-register-hot-key [s])))

(mapc (lambda (keybind)
	(let ((key (car keybind))
	      (cmd (cadr keybind)))
	  (global-set-key (kbd key) cmd)))
      '(
	("C-c g" goto-line)
	("C-x C-b" electric-buffer-list)
	("s-b" switch-to-buffer)
	("s-d" find-file)
	("s-0" delete-window)
	("s-1" delete-other-windows)
	("s-2" split-window-below)
	("s-3" split-window-right)
	("s-4" double-split-window-below-and-delete)
	("s-5" double-split-window-right-and-delete)
	;; (,(kbd "C-x C-f") counsel-find-file)
	("C-m" newline-and-indent)
	("M-n" next-error)
	("M-p" previous-error)
	("M-/" hippie-expand)
	("C-c m" compile)
	("C-c r" org-capture)
	("C-c o" org-open-at-point-global)
	("C-c l" org-store-link)
	("C-c L" org-insert-link-global)
	("C-c a" org-agenda)
	("C-c s" shell)
	("C-c f" toggle-frame-fullscreen)
	("C-h C-f" find-function)
	("C-h C-v" find-variable)
	("C-h C-l" find-library)
	("C-w" czm/kill-or-delete-region)
	("C-M-z" zap-up-to-char)
	("C-M-g" down-list)
	("s-<left>" previous-buffer)
	("s-<right>" next-buffer)
	("s-<up>" (lambda () (interactive) (enlarge-window 5)))
	("s-<down>" (lambda () (interactive) (shrink-window 5)))
	("s-o" other-window)
	("s-O" (lambda () (interactive) (other-window -1)))
        ("s-'" nil)
        ("s-n" nil)
        ("s-N" make-frame)
        ("s-n" outline-next-heading)
        ("s-p" outline-previous-heading)
	("s-k" kill-current-buffer)
	("s-s" save-buffer)
	("s-v" view-mode)
	("s-f" zap-to-char)
	("s-F" czm/zap-to-char-backwards)
	("s-t" zap-up-to-char)
	("s-T" czm/zap-up-to-char-backwards)
	("s-." repeat)
	("s-w" switch-to-buffer)
	("s-i" czm/find-lisp-file)
	)
      )

(defun czm/zap-to-char-backwards (char)
  "Zap to CHAR backwards."
  (interactive "cZap to char (backwards): ")
  (zap-to-char -1 char))

(defun czm/zap-up-to-char-backwards (char)
  "Zap up to CHAR backwards."
  (interactive "cZap up to char (backwards): ")
  (zap-up-to-char -1 char))

(defvar czm/find-lisp-file-completion-fn 'completing-read
  "The completion function to use for the `find-elisp-file-variable` function.
Possible values: completing-read, ivy-read.")

(defun double-split-window-below-and-delete ()
  "Split the window below twice, visit the previous window and then remove the current window"
  (interactive)
  (split-window-below)
  (other-window 1)
  (split-window-below)
  (delete-window)
  (other-window 1)
  (next-buffer))

(defun double-split-window-right-and-delete ()
  "Split the window to the right twice, visit the previous window and then remove the current window"
  (interactive)
  (split-window-right)
  (other-window 1)
  (split-window-right)
  (delete-window)
  (other-window 1)
  (next-buffer))

(defun czm/kill-or-delete-region (start end &optional arg)
  "Kill or delete region based on the presence of an optional argument.

If called with a prefix argument (C-u), delete the region between START and END.
Otherwise, kill the region between START and END."
  (interactive "r\nP")
  (if arg
      (delete-region start end)
    (kill-region start end)))



(defun czm/find-lisp-file ()
  "Opens an elisp file in the ~/.emacs.d or ~/.emacs.d/lisp directory."
  (interactive)
  (let* ((elisp-dir1 (expand-file-name user-emacs-directory))
         (elisp-files (append
                       (directory-files elisp-dir1 t "\\.el$")))
         (default-elisp-file (concat user-emacs-directory "init.el"))
         (completion-fn czm/find-lisp-file-completion-fn)
         (selected-elisp-file (funcall completion-fn "Select elisp file: " elisp-files
                                       nil t nil nil default-elisp-file)))
    (when selected-elisp-file (find-file selected-elisp-file))))

(dolist (key '(
	       "s-C"
					; "s-D" ; dired
	       "s-E"
	       "s-H"
	       "s-L"
					; "s-M" ; manual-entry
	       "s-S"
	       "s-c"
	       "s-g"
	       "s-h"
	       "s-m"
	       "s-p"
	       "s-u"
	       "s-q"
	       "s-x"
	       "s-y"
	       "s-z"
	       ))
  (global-unset-key (kbd key)))
