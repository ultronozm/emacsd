;;; -*- lexical-binding: t; -*-

;;; won't be necessary in Emacs 31+
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
