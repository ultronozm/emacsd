;;; burp.el --- Minimal implementation of slurp/barf for lists  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 1.0
;; URL: https://github.com/ultronozm/burp.el
;; Package-Requires: ((emacs "24.1"))
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a minimalist alternative to packages like
;; `paredit', `lispy', etc., intended for my own experimentation.  It
;; implements slurp and barf functions for lists and quotes.  When
;; positioned just before or just after a list or quote block, these
;; work much like `lispy-slurp-or-barf-left' and
;; `lispy-slurp-or-barf-right'.  Otherwise, they call
;; `self-insert-char'.  The further command `burp-unwrap' removes the
;; outermost list or quote block.
;;
;; Sample use-package declaration:
;; 
;; (use-package burp
;;   :ensure
;;   :bind
;;   (:map emacs-lisp-mode-map
;;         ("<" . burp-left)
;;         (">" . burp-right)
;;         ("/" . burp-unwrap)))
;;         
;; (use-package emacs
;;   :custom
;;   (delete-pair-blink-delay 0)
;;   :bind
;;   (:map emacs-lisp-mode-map
;;         ("M-_" . delete-pair)
;;         ("M-+" . kill-backward-up-list)))



;;; Code:


(defun burp--matching-delim (char)
  "Return the matching delimiter for CHAR, or nil if none."
  (cadr (assoc char insert-pair-alist)))

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
  (backward-char)
  (let ((char (char-after)))
    (delete-char 1)
    (condition-case nil
        (progn
          (backward-sexp)
          (backward-sexp)
          (forward-sexp))
      (error
       (search-backward (char-to-string (burp--matching-delim char)))
       (forward-char)))
    (insert char)))

;;;###autoload
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

;;;###autoload
(defun burp-right ()
  "Slurp or barf to the right.
If the point is before a list, slurp the next sexp into the list.
If the point is after a list, barf the last sexp out of the list.
If the point is before a quote, slurp the next sexp into the quote.
If the point is after a quote, barf the last sexp out of the quote.
Otherwise, call `self-insert-command'."
  (interactive)
  (cond
   ((eq (char-after) ?\()
    (burp--barf-right))
   ((eq (char-before) ?\))
    (burp--slurp-right))
   ((and (eq (char-before) ?\")
         (not (eq (char-after) ?\")))
    (burp--slurp-right))
   ((eq (char-after) ?\")
    (unless (eq (char-after (1+ (point))) ?\")
      (unless (eq (char-after (1- (point))) ?\")
        (burp--barf-right))))
   (t
    (call-interactively #'self-insert-command))))

;;;###autoload
(defun burp-unwrap ()
  "Remove the next sexp from its list."
  (interactive)
  (condition-case nil
      (delete-pair)
    (error
     (condition-case nil
         (save-excursion
           (backward-sexp)
           (delete-pair))
       (error (call-interactively #'self-insert-command))))))

(provide 'burp)
;;; burp.el ends here
