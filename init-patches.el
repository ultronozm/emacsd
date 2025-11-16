;;; -*- lexical-binding: t; -*-

;;; Emacs patches

(when nil
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
  )
(when nil
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
      (widen))))

(when nil ;; patch pending
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

  (require 'use-package-bind-key)

  (defun use-package-normalize-binder (name keyword args)
    (let ((arg args)
          args*)
      (while arg
        (let ((x (car arg)))
          (cond
           ;; (KEY . COMMAND)
           ((and (consp x)
                 (or (stringp (car x))
                     (vectorp (car x)))
                 (or (use-package-recognize-function (cdr x) t #'stringp)))
            (setq args* (nconc args* (list x)))
            (setq arg (cdr arg)))
           ;; KEYWORD
           ;;   :map KEYMAP
           ;;   :prefix-docstring STRING
           ;;   :prefix-map SYMBOL
           ;;   :prefix STRING
	          ;;   :repeat-docstring STRING
           ;;   :repeat-map SYMBOL
           ;;   :filter SEXP
           ;;   :menu-name STRING
           ;;   :package SYMBOL
	          ;;   :continue(-only) and :exit are used within :repeat-map
           ((or (and (eq x :map) (symbolp (cadr arg)))
                (and (eq x :prefix) (stringp (cadr arg)))
                (and (eq x :prefix-map) (symbolp (cadr arg)))
                (and (eq x :prefix-docstring) (stringp (cadr arg)))
	               (and (eq x :repeat-map) (symbolp (cadr arg)))
	               (memq x '(:continue :continue-only :exit))
                (and (eq x :repeat-docstring) (stringp (cadr arg)))
                (eq x :filter)
                (and (eq x :menu-name) (stringp (cadr arg)))
                (and (eq x :package) (symbolp (cadr arg))))
            (setq args* (nconc args* (list x (cadr arg))))
            (setq arg (cddr arg)))
           ((listp x)
            (setq args*
                  (nconc args* (use-package-normalize-binder name keyword x)))
            (setq arg (cdr arg)))
           (t
            ;; Error!
            (use-package-error
             (concat (symbol-name name)
                     " wants arguments acceptable to the `bind-keys' macro,"
                     " or a list of such values"))))))
      args*)))

(when nil
  (defun czm-foldout-exit-fold-override (&optional num-folds)
    "Return to the ARG'th enclosing fold view.  With ARG = 0 exit all folds.

Normally causes exited folds to be hidden, but with ARG < 0, -ARG folds are
exited and text is left visible."
    (interactive "p")
    (let ((hide-fold t) start-marker end-marker
          beginning-of-heading end-of-subtree
          (original-point (point)))

      ;; check there are some folds to leave
      (if (null foldout-fold-list)
	         (error "Not in a fold!"))

      (cond
       ;; catch a request to leave all folds
       ((zerop num-folds)
        (setq num-folds (length foldout-fold-list)))

       ;; have we been told not to hide the fold?
       ((< num-folds 0)
        (setq hide-fold nil
              num-folds (- num-folds))))

      ;; limit the number of folds if we've been told to exit too many
      (setq num-folds (min num-folds (length foldout-fold-list)))

      ;; exit the folds
      (widen)
      (while (not (zerop num-folds))
        ;; get the fold at the top of the stack
        (setq start-marker (car (car foldout-fold-list))
	             end-marker (cdr (car foldout-fold-list))
	             foldout-fold-list (cdr foldout-fold-list)
	             num-folds (1- num-folds))

        ;; Make sure there is a newline at the end of this fold,
        ;; otherwise the following heading will get joined to the body
        ;; text.
        (if end-marker
	           (progn
	             (goto-char end-marker)
	             (forward-char -1)
	             (or (memq (preceding-char) '(?\n ?\^M))
		                (insert ?\n))))

        ;; If this is the last fold to exit, hide the text unless we've
        ;; been told not to.  Note that at the moment point is at the
        ;; beginning of the following heading if there is one.

        ;; Also, make sure that the newline before the following heading
        ;; is \n otherwise it will be hidden.  If there is a newline
        ;; before this one, make it visible too so we do the same as
        ;; outline.el and leave a blank line before the heading.
        (when (zerop num-folds)
	         (if end-marker
	             (setq beginning-of-heading (point)
		                  end-of-subtree (progn (forward-char -1)
					                                     (if (memq (preceding-char)
						                                              '(?\n ?\^M))
					                                         (forward-char -1))
					                                     (point))))
	         ;; hide the subtree
	         (when hide-fold
	           (goto-char start-marker)
	           (outline-hide-subtree))

	         ;; make sure the next heading is exposed
	         (if end-marker
              (outline-flag-region end-of-subtree beginning-of-heading nil)))

        ;; zap the markers so they don't slow down editing
        (set-marker start-marker nil)
        (if end-marker (set-marker end-marker nil)))

      ;; narrow to the enclosing fold if there is one
      (if foldout-fold-list
	         (progn
	           (setq start-marker (car (car foldout-fold-list))
		                end-marker (cdr (car foldout-fold-list)))
	           (narrow-to-region start-marker
			                           (if end-marker
				                              (1- (marker-position end-marker))
			                             (point-max)))))

      ;; if we do not hide the region, then keep the point where it was
      (unless hide-fold
        (goto-char original-point))

      (recenter)

      ;; update the mode line
      (foldout-update-mode-line)))

  (advice-add 'foldout-exit-fold :override #'czm-foldout-exit-fold-override))
