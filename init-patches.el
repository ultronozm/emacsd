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
  (require 'foldout)
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
