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
	         (global-set-key (kbd key)
                          cmd)))
      '(
	       ("C-c g" goto-line)
	       ;; ("C-x C-b" electric-buffer-list)
        ("C-x C-b" ibuffer)
	       ("s-b" switch-to-buffer)
	       ("s-d" find-file)
	       ("s-0" delete-window)
	       ("s-1" delete-other-windows)
	       ("s-2" split-window-below)
	       ("s-3" split-window-right)
	       ("C-m" newline-and-indent)
	       ("M-n" next-error)
	       ("M-p" previous-error)
	       ("M-/" hippie-expand)
        ("C-c d" czm/open-downloads-dired)
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
	       ("C-M-z" zap-up-to-char)
	       ("C-M-g" down-list)
	       ("C-M-t" transpose-sexps)
        ("M-+" raise-sexp)
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
	       ("s-k" kill-current-buffer)
	       ("s-s" save-buffer)
	       ("s-v" view-mode)
        ;; s-f and s-t are unoccupied!
	       ("s-y" pop-to-mark-command)
        ("s-." repeat)
	       ("s-w" switch-to-buffer)
	       ("s-i" czm/find-lisp-file)
        ("<" burp-left)
        (">" burp-right)
        ("/" burp-unwrap)
	       ("<up>" windmove-up)
	       ("<down>" windmove-down)
	       ("<right>" windmove-right)
	       ("<left>" windmove-left)
        ("s-SPC" cycle-spacing)
        ("s-6" (lambda () (interactive) (delete-indentation nil)))
        ("s-7" (lambda () (interactive) (delete-indentation t)))
        ("C-x C-M-t" transpose-regions)
	       )
      )

(defvar czm/find-lisp-file-completion-fn 'completing-read
  "The completion function to use for the `find-elisp-file-variable` function.
Possible values: completing-read, ivy-read.")

;; TODO: add "burp.el" functionality here, since it's very handy when
;; working with startup configs

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


(defun czm/open-downloads-dired ()
  "Open the '~/Downloads' directory in Dired mode."
  (interactive)
  (dired "~/Downloads"))

(defun burp--matching-delim (char)
  "Return the matching delimiter for CHAR, or nil if none."
  (cadr (assoc char '((?\( ?\)) (?\[ ?\]) (?\{ ?\}) (?\< ?\>) (?\" ?\") (?\' ?\') (?\` ?\')
                      (?\) ?\() (?\] ?\[) (?\} ?\{) (?\> ?\<) (?\" ?\") (?\' ?\') (?\' ?\`)))))

(defun burp--slurp-left ()
  "Slurp the next sexp into the current one, to the left."
  (let ((pos (point))
        (char (char-after)))
    (delete-char 1)
    (condition-case nil
        (progn
          (backward-sexp)
          (insert char)
          (backward-char))
      (error
       (goto-char pos)
       (insert char)
       (backward-char)))))

(defun burp--barf-left ()
  "Barf the next sexp out of the current one, to the right."
  (let ((char (char-before))
          (pos (point)))
      (backward-char 1)
      (condition-case nil
          (progn
            (backward-sexp)
            (backward-sexp)
            (forward-sexp))
        (error
         (search-backward (char-to-string (burp--matching-delim char)))
         (forward-char)))
      (insert char)
      (save-excursion
        (goto-char pos)
        (delete-char 1))
      ))


(defun burp-left ()
  "Slurp or barf to the right.
If the point is before a list, slurp the next sexp into the list.
If the point is after a list, barf the last sexp out of the list.
If the point is before a quote, slurp the next sexp into the quote.
If the point is after a quote, barf the last sexp out of the quote.
Otherwise, call `self-insert-command'."
  (interactive)
  (cond
   ((eq (char-after) ?\()
    (burp--slurp-left))
   ((eq (char-before) ?\))
    (burp--barf-left))
   ((and (eq (char-after) ?\")
         (not (eq (char-before) ?\")))
    (burp--slurp-left))
   ((eq (char-before) ?\")
    (unless (eq (char-before (1- (point))) ?\")
      (unless (eq (char-after) ?\")
        (burp--barf-left))))
   (t
    (call-interactively #'self-insert-command))))

(defun burp--barf-right ()
  "Barf the next sexp out of the current one, to the right."
  (let ((char (char-after))
          (pos (point)))
      (forward-char 1)
      (condition-case nil
          (progn
            (forward-sexp)
            (forward-sexp)
            (backward-sexp))
        (error
         (search-forward (char-to-string (burp--matching-delim char)))
         (backward-char)))
      (insert char)
      (save-excursion
        (goto-char pos)
        (delete-char 1))
      (backward-char)))

(defun burp--slurp-right ()
  "Slurp the next sexp into the current one, to the right."
  (backward-char)
  (let ((pos (point))
        (char (char-after)))
    (forward-char)
    (condition-case nil
        (progn
          (forward-sexp)
          (insert char)
          (save-excursion
            (goto-char pos)
            (delete-char 1)))
      (error nil))))


(defun burp-right ()
  "Slurp or barf to the right.
If the point is before a list, slurp the next sexp into the list.
If the point is after a list, barf the last sexp out of the list.
If the point is before a quote, slurp the next sexp into the quote.
If the point is after a quote, barf the last sexp out of the quote.
Otherwise, call `self-insert-command'."
  (interactive)
  (cond
   ; prev is )
   ((eq (char-before) ?\))
    (burp--slurp-right))
   ; next is (
   ((eq (char-after) ?\()
    (burp--barf-right))
   ((and (eq (char-before) ?\")
         (not (eq (char-after) ?\")))
    (burp--slurp-right))
   ((eq (char-after) ?\")
    (unless (eq (char-after (1+ (point))) ?\")
      (unless (eq (char-after (1- (point))) ?\")
        (burp--barf-right))))
   (t
    (call-interactively #'self-insert-command))))


;; (defun burp-unwrap ()
;;   "Remove the next sexp from its list."
;;   (interactive)
;;   (condition-case nil
;;       (delete-pair)
;;     (error
;;      (condition-case nil
;;          (save-excursion
;;            (backward-sexp)
;;            (delete-pair))
;;        (error (call-interactively #'self-insert-command))))))

(defun burp-unwrap ()
  "Remove the next sexp from its list."
  (interactive)
  (condition-case nil
      (delete-pair)
    (error
     (call-interactively #'self-insert-command))))


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
	              "s-u"
	              "s-q"
	              "s-x"
	              "s-z"
	              ))
  (global-unset-key (kbd key)))
