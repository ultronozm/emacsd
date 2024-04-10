;;; -*- lexical-binding: t; -*-

(setq ns-command-modifier 'meta)
(setq ns-alternate-modifier 'super)
(setq ns-function-modifier 'hyper)
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
        ("C-c d" czm-dired-downloads)
        ("s-d" czm-find-math-document)
	       ("C-c m" compile)
	       ("C-c r" org-capture)
	       ("C-c o" org-open-at-point-global)
	       ("C-c l" org-store-link)
	       ("C-c L" org-insert-link-global)
	       ("C-c a" org-agenda)
	       ("C-c s" shell)
	       ("C-c f" toggle-frame-fullscreen)
	       ("C-h C-f" find-function)
        ("H-b" abbrev-mode)
        ("H-e" toggle-debug-on-error)
        ("H-f" follow-mode)
        ("H-i" overwrite-mode)
        ("H-l" flymake-mode)
        ("H-k" flycheck-mode)
        ("H-m" mmm-mode)
        ("H-v" visual-line-mode)
	       ("C-h C-v" find-variable)
	       ("C-h C-l" find-library)
	       ("C-M-z" zap-up-to-char)
	       ("C-M-g" down-list)
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
        ("s-q" bury-buffer)
	       ("s-k" kill-current-buffer)
	       ("s-s" save-buffer)
	       ("s-v" view-mode)
        ("s-." repeat)
	       ("s-i" czm/find-lisp-file)
        ;; ("<" burp-left)
        ;; (">" burp-right)
        ;; ("/" burp-unwrap)
        ("M-_" delete-pair)
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


(defun czm-dired-downloads ()
  "Open the downloads directory in Dired mode."
  (interactive)
  (dired my-downloads-folder))

(defun czm-find-math-document ()
  "Find a file in the math documents folder."
  (interactive)
  (project-find-file-in nil (list my-math-folder) `(local . ,my-math-folder)))

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
               ;; s-w
	              ;; "s-y"
	              "s-z"
	              ))
  (global-unset-key (kbd key)))

;; H-ACFNHMD -- macOS annoyance
