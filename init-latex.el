;;; -*- lexical-binding: t; -*-

(defun czm-tex-buffer-face ()
  (interactive)
  (setq buffer-face-mode-face
        '(:height 260
                  :width normal
                  :family
                  ;; "Monaco"
                  "Andale Mono"
                  ;; "Lucida Grande"
                  ;; "Papyrus"
                  ;; "Hoefler Text"
                  )
        ;; '(:height 260
        ;;           :width normal
        ;;           :family
        ;;           "Lucida Grande"
        ;;           )
        ;; '(:height 260 :width normal
        ;;                                 :family
        ;;                                 ;; "Didot"
        ;;                                 "Andale Mono"
        ;;                                 ;; "Lucida Grande"
        ;;                                 ;; "Papyrus"
        ;;                                 ;; "PT Mono"
        ;;                                 ;; "Baskerville"
        ;;                                 ;; "Hoefler Text"
        ;;                                 ;; "Monaco"
        ;;                                 ;; "Verdana"
        ;;                                 ;; "Menlo"
        ;;                                 ;; "DejaVu Mono-12"
        ;;                                 ;; "Lucida Typewriter"
        ;;                                 ;; "Courier New"
        ;;                                 )
        )
  ;; (unless buffer-face-mode
  ;;   (buffer-face-mode 0))
  (buffer-face-mode))

(defun czm-tex-setup-environments-and-outline-regexp ()
  (LaTeX-add-environments
   '("lemma" LaTeX-env-label)
   '("exercise" LaTeX-env-label)
   '("example" LaTeX-env-label)
   '("proposition" LaTeX-env-label)
   '("corollary" LaTeX-env-label)
   '("remark" LaTeX-env-label)
   '("definition" LaTeX-env-label)
   '("theorem" LaTeX-env-label))
  (setq-local outline-regexp
	             (concat "\\\\"
		                    (regexp-opt (append latex-metasection-list
					                                     (mapcar #'car latex-section-alist)
					                                     '("bibliography"))
				                              t))))

(defun czm-widen-first (orig-fun &rest args)
  (save-restriction
    (widen)
    (apply orig-fun args)))

;; https://karthinks.com/software/latex-input-for-impatient-scholars/
(defun latex-math-from-calc ()
  "Evaluate `calc' on the contents of line at point."
  (interactive)
  (cond ((region-active-p)
         (let* ((beg (region-beginning))
                (end (region-end))
                (string (buffer-substring-no-properties beg end)))
           (kill-region beg end)
           (insert (calc-eval `(,string calc-language latex
                                        calc-prefer-frac t
                                        calc-angle-mode rad)))))
        (t (let ((l (thing-at-point 'line)))
             (end-of-line 1) (kill-line 0)
             (insert (calc-eval `(,l
                                  calc-language latex
                                  calc-prefer-frac t
                                  calc-angle-mode rad)))))))

(defun czm-latex-calc-grab (beg end)
  (interactive "r")
  ;; (let ((old-lang calc-language))
  ;;   (unwind-protect
  ;;       (progn
  ;;         (save-excursion
  ;;           (calc-create-buffer))
  ;;         (calc-set-language 'latex)
  ;;         (calc-grab-region beg end '(4)))
  ;;     (when old-lang
  ;;       (calc-set-language old-lang))))
  ;; (calc-grab-region beg end '(4))
  (symtex-with-calc-language 'latex
                             (calc-grab-region beg end '(4))))

(defun czm-TeX-next-error-wrapper (&optional arg)
  (interactive "P")
  (if
      (or (null (TeX-active-buffer))
          (eq 'compilation-mode (with-current-buffer TeX-command-buffer
                                  major-mode)))
      (TeX-next-error arg reparse)
    (next-error arg)))

(defun czm-TeX-previous-error-wrapper (&optional arg)
  (interactive "P")
  (if
      (or (null (TeX-active-buffer))
          (eq 'compilation-mode (with-current-buffer TeX-command-buffer
                                  major-mode)))
      (TeX-previous-error arg reparse)
    (previous-error arg)))

(defun czm-unwrap-mark-sexp ()
  (interactive)
  (let ((results (sp-unwrap-sexp)))
    ;; this returns something like (:beg 12501 :end 12618 :op "\\left[" :cl "\\right]" :prefix "" :suffix ".").  let's bind those fields to variables using plist-get:
    (let ((end (plist-get results :end))
          (op (plist-get results :op))
          (cl (plist-get results :cl)))
      (let ((new-end
             (- end
                (+ (length op) (length cl)))))
        ;; highlight region between beg and end
        (push-mark end)
        (activate-mark)))))

(use-package latex
  :elpaca (auctex
           :files
           ("*.el" "*.info" "dir" "doc" "etc" "images" "latex" "style")
           :pre-build
           (("./autogen.sh")
            ("./configure"
             "--with-texmf-dir=$(dirname $(kpsexpand '$TEXMFHOME'))"
             "--with-lispdir=.")
            ("make")
            ("make" "install")))

  :demand                               ; otherwise, madness ensues.

  :config
  (setq TeX-data-directory (expand-file-name "elpaca/builds/auctex" user-emacs-directory))
  (setq TeX-lisp-directory TeX-data-directory)

  :hook
  (LaTeX-mode . TeX-fold-mode)
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . czm-tex-setup-environments-and-outline-regexp)
  (LaTeX-mode . czm-tex-buffer-face)
  (LaTeX-mode . outline-minor-mode)
  (LaTeX-mode . abbrev-mode)
  ;; (LaTeX-mode . toggle-word-wrap)
  (LaTeX-mode . visual-line-mode)
  (LaTeX-mode . (lambda ()
                  (setq fill-column 999999)))
  (LaTeX-mode . smartparens-mode)

  :bind
  (:map LaTeX-mode-map
        ("s-a" . abbrev-mode)
        ("s-c" . preview-clearout-at-point)
        ("s-q" . LaTeX-fill-buffer)

        ("C-c C-l" . latex-math-from-calc)
        ("C-c C-g" . czm-latex-calc-grab)
        ("C-c C-n" . nil)
                                        ; TeX-normal-mode
        ("C-c #" . nil)
        ;; ("M-n" . czm-TeX-next-error-wrapper)
        ;; ("M-p" . czm-TeX-previous-error-wrapper)
        ([remap next-error])
        ([remap previous-error])
        ("M-n" . next-error)
        ("M-p" . previous-error)

        ("M-u" . sp-up-sexp)
        ("M-U" . sp-unwrap-sexp)
        ("M-S" . czm-unwrap-mark-sexp)
        )

  :config
  (put 'LaTeX-narrow-to-environment 'disabled nil)
  (TeX-source-correlate-mode)
  (advice-add 'TeX-view :around #'czm-widen-first) ; fixes bug in TeX-view

  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (preview-auto-cache-preamble t)
  (preview-default-option-list
   '("displaymath" "floats" "graphics" "textmath" "sections" "footnotes" "showlabels"))
                                        ;  (preview-gs-command "/usr/local/bin/gs")  ; compare with rungs?
                                        ;  (preview-image-type 'pnm) ; compare with png?



  (reftex-derive-label-parameters
   '(15 50 t 1 "-"
        ("the" "on" "in" "off" "a" "for" "by" "of" "and" "is" "to")
        t)))

;;  don't want foldout to include "bibliography"
(defun czm-LaTeX-outline-level-advice (orig-fun &rest args)
  (if (looking-at "\\\\bibliography") 1 (apply orig-fun args)))

(defun my-preview-tailor-factor-function ()
  "ez"
  (if (string-suffix-p ".lean" (buffer-file-name)) 0.6 1.0))

(use-package preview-tailor
  :elpaca (:host github :repo "ultronozm/preview-tailor.el"
                 :depth nil)
  :after latex
  :config
  (preview-tailor-init)
  :custom
  (preview-tailor-additional-factor-function #'my-preview-tailor-factor-function))

(use-package foldout
  :elpaca nil
  :config
  (advice-add 'LaTeX-outline-level :around #'czm-LaTeX-outline-level-advice))

(use-package smartparens
  :bind
  (:map LaTeX-mode-map
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp))


  :config
  (spw/remap-mark-command 'sp-mark-sexp LaTeX-mode-map)

  (defun sp-latex-insert-spaces-inside-pair (_id action _context)
    "ID, ACTION, CONTEXT."
    (when (eq action 'insert)
      (insert "  ")
      (backward-char 1))
    (when (and (eq action 'wrap)
               (save-excursion
                 (goto-char (sp-get sp-last-wrapped-region :beg-in))
                 (not (sp--looking-back-p "[[{(]"))))
      (save-excursion
        (goto-char (sp-get sp-last-wrapped-region :end-in))
        (insert " ")
        (goto-char (sp-get sp-last-wrapped-region :beg-in))
        (insert " "))))

  (defun sp-latex-skip-match-apostrophe (ms _mb me)
    "MS, MB, ME."
    (when (equal ms "'")
      (save-excursion
        (goto-char me)
        (looking-at-p "\\sw"))))

  (defun sp-latex-skip-double-quote (_id action _context)
    "ID, ACTION, CONTEXT."
    (when (eq action 'insert)
      (when (looking-at-p "''''")
        (delete-char -2)
        (delete-char 2)
        (forward-char 2))))

  (defun sp-latex-point-after-backslash (id action _context)
    "Return t if point follows a backslash, nil otherwise.
This predicate is only tested on \"insert\" action.
ID, ACTION, CONTEXT."
    (when (eq action 'insert)
      (let ((trigger (sp-get-pair id :trigger)))
        (looking-back (concat "\\\\" (regexp-quote (if trigger trigger id))) nil))))

  (add-to-list 'sp-navigate-skip-match
               '((tex-mode plain-tex-mode latex-mode) . sp--backslash-skip-match))

  (sp-with-modes '(
                   tex-mode
                   plain-tex-mode
                   latex-mode
                   LaTeX-mode
                   )
    (sp-local-pair "`" "'"
                   :actions '(:rem autoskip)
                   :skip-match 'sp-latex-skip-match-apostrophe
                   :unless '(sp-latex-point-after-backslash sp-in-math-p))
    ;; math modes, yay.  The :actions are provided automatically if
    ;; these pairs do not have global definitions.
    (sp-local-pair "$" "$")
    (sp-local-pair "\\[" "\\]"
                   :unless '(sp-latex-point-after-backslash))

    ;; disable useless pairs.
    (sp-local-pair "\\\\(" nil :actions nil)
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "\\\"" nil :actions nil)

    ;; quote should insert ``'' instead of double quotes.  If we ever
    ;; need to insert ", C-q is our friend.
    (sp-local-pair "``" "''"
                   :trigger "\""
                   :unless '(sp-latex-point-after-backslash sp-in-math-p)
                   :post-handlers '(sp-latex-skip-double-quote))

    ;; add the prefix function sticking to {} pair
    (sp-local-pair "{" nil :prefix "\\\\\\(\\sw\\|\\s_\\)*")

    ;; do not add more space when slurping
    (sp-local-pair "{" "}")
    (sp-local-pair "(" ")")
    (sp-local-pair "[" "]")

    ;; pairs for big brackets.  Needs more research on what pairs are
    ;; useful to add here.  Post suggestions if you know some.
    (sp-local-pair "\\left(" "\\right)"
                   :trigger "\\l("
                   :when '(sp-in-math-p)
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\left[" "\\right]"
                   :trigger "\\l["
                   :when '(sp-in-math-p)
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\left\\{" "\\right\\}"
                   :trigger "\\l{"
                   :when '(sp-in-math-p)
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\left|" "\\right|"
                   :trigger "\\l|"
                   :when '(sp-in-math-p)
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\bigl(" "\\bigr)"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\biggl(" "\\biggr)"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\Bigl(" "\\Bigr)"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\Biggl(" "\\Biggr)"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\bigl[" "\\bigr]"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\biggl[" "\\biggr]"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\Bigl[" "\\Bigr]"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\Biggl[" "\\Biggr]"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\bigl\\{" "\\bigr\\}"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\biggl\\{" "\\biggr\\}"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\Bigl\\{" "\\Bigr\\}"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\Biggl\\{" "\\Biggr\\}"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\lfloor" "\\rfloor"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\lceil" "\\rceil"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\langle" "\\rangle"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair  "\\lVert" "\\rVert"
                    :when '(sp-in-math-p)
                    :trigger "\\lVert"
                    :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair  "\\lvert" "\\rvert"
                    :when '(sp-in-math-p)
                    :trigger "\\lvert"
                    :post-handlers '(sp-latex-insert-spaces-inside-pair))
    ;; (sp-local-pair  "\\left\\lvert" "\\right\\rvert"
    ;;                 :when '(sp-in-math-p)
    ;;                 :trigger "\\left\\lvert"
    ;;                 :post-handlers '(sp-latex-insert-spaces-inside-pair))
    ;; (sp-local-pair  "\\left\\lVert" "\\right\\rVert"
    ;;                 :when '(sp-in-math-p)
    ;;                 :trigger "\\left\\lVert"
    ;;                 :post-handlers '(sp-latex-insert-spaces-inside-pair))

    ;; some common wrappings
    (sp-local-tag "\"" "``" "''" :actions '(wrap))
    (sp-local-tag "\\b" "\\begin{_}" "\\end{_}")
    (sp-local-tag "bi" "\\begin{itemize}" "\\end{itemize}")
    (sp-local-tag "be" "\\begin{enumerate}" "\\end{enumerate}")))

(use-package spout
  :elpaca (:host github :repo "ultronozm/spout.el"
                 :depth nil)
  :after latex
  :hook
  (LaTeX-mode . spout-mode)
  :custom
  (spout-keys
   '(("n" outline-next-visible-heading)
     ("p" outline-previous-visible-heading)
     ("u" outline-up-heading)
     ("f" outline-forward-same-level)
     ("b" outline-backward-same-level)
     ("M-<left>" outline-promote left-word)
     ("M-<right>" outline-demote right-word)
     ("<" beginning-of-buffer)
     (">" end-of-buffer)
     ("<up>" outline-move-subtree-up windmove-up)
     ("<down>" outline-move-subtree-down windmove-down)
     ("s" outline-show-subtree)
     ("d" outline-hide-subtree)
     ("a" outline-show-all)
     ("q" outline-hide-sublevels)
     ("t" outline-hide-body)
     ("k" outline-show-branches)
     ("l" outline-hide-leaves)
     ("i" outline-insert-heading)
     ("o" outline-hide-other)
     ("@" outline-mark-subtree)
     ("z" foldout-zoom-subtree)
     ("x" foldout-exit-fold)))
  :config
  (require 'texmathp)
  (defun LaTeX-skip-verbatim (orig-fun &rest args)
    (if (eq major-mode 'latex-mode)
        (let ((n 100))
          (apply orig-fun args)
          (while (and (LaTeX-verbatim-p) (> n 0))
            (setq n (- n 1))
            (apply orig-fun args)))
      (apply orig-fun args)))
  (dolist (f '(outline-next-heading
               outline-previous-heading
               outline-up-heading
               outline-forward-same-level
               outline-backward-same-level))
    (advice-add f :around #'LaTeX-skip-verbatim)))

(defun latex/kill-environment (arg)
  "Kill forward to end of environment.
With ARG N, kill forward to Nth end of environment;
negative ARG -N means kill backward to Nth start of environment."
  (interactive "p")
  (kill-region (point) (progn (latex/forward-environment arg) (point))))

(defun latex/backward-kill-environment (arg)
  "Kill back to start of environment.
With ARG N, kill back to Nth start of environment;
negative ARG -N means kill forward to Nth end of environment."
  (interactive "p")
  (kill-region (point) (progn (latex/backward-environment arg) (point))))

(defun latex/kill-sexp-or-environment (arg)
  (interactive "p")
  (if (looking-at "\\\\begin")
      (latex/kill-environment arg)
    (sp-kill-sexp arg)))

(defun latex/backward-kill-sexp-or-environment (arg)
  (interactive "p")
  (sp-backward-kill-sexp arg))



(defun latex/backward-up-list-or-beginning-of-environment (arg)
  (interactive "p")
  (condition-case nil
      (sp-backward-up-sexp arg)
    (scan-error (latex/beginning-of-environment arg))))

(defun latex/down-list-or-enter-environment (arg)
  (interactive "p")
  (if (looking-at "\\\\begin")
      (progn
        (forward-line)
        (back-to-indentation)
        )
    (sp-down-sexp arg)))

(global-set-key (kbd "M-u") 'up-list)



(defun latex/mark-sexp-or-environment (arg)
  (interactive "p")
  (if (looking-at "\\\\begin")
      (progn (push-mark
              (save-excursion
                (latex/forward-environment arg)
                (point)))
             (activate-mark))
    (spw/sp-mark-sexp)))

;; use smartparens w/ latex

(use-package latex-extra
  :after latex
  :bind
  (:map latex-extra-mode-map
        ("TAB" . nil)
        ("C-M-SPC" . latex/mark-sexp-or-environment)
        ("C-M-u" . latex/backward-up-list-or-beginning-of-environment)
        ("C-M-g" . latex/down-list-or-enter-environment)
        ("C-M-e" . latex/forward-environment)
        ("C-M-a" . latex/backward-environment)
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-k" . latex/kill-sexp-or-environment)
        ("C-M-<backspace>" . latex/backward-kill-sexp-or-environment)
        ("C-s-n" . latex/forward-environment)
        ("C-s-p" . latex/backward-environment)
        ("C-s-e" . latex/forward-environment)
        ("C-s-a" . latex/backward-environment)
        ("C-s-k" . latex/kill-environment)
        ("C-s-<backspace>" . latex/backward-kill-environment)
        )
  :custom
  (latex/override-preview-map nil)
  (latex/override-font-map nil)
  (latex/override-fill-map nil)
  :hook
  (LaTeX-mode . latex-extra-mode))

(use-package czm-tex-util
  :elpaca (:host github :repo "ultronozm/czm-tex-util.el"
                 :depth nil)
  :after latex)

(use-package czm-tex-fold
  :elpaca (:host github :repo "ultronozm/czm-tex-fold.el"
                 :depth nil)
  :demand ; otherwise, this doesn't work until the second time you
                                        ; open a .tex file.  but it needs to be loaded after auctex.
  :bind
  (:map TeX-fold-mode-map
        ("C-c C-o C-s" . czm-tex-fold-fold-section)
        ("C-c C-o s" . czm-tex-fold-clearout-section))
  :config
  (czm-tex-fold-set-defaults)
  (czm-tex-fold-install)
  :custom
  (czm-tex-fold-bib-file my-master-bib-file)
  :hook
  (LaTeX-mode . tex-fold-mode))

;; the following should perhaps be part of czm-tex-fold:

(defun czm-tex-fold-macro-previous-word ()
  (interactive)
  (if TeX-fold-mode
      (save-excursion
	       (backward-word)
	       (TeX-fold-item 'macro))))

(advice-add 'LaTeX-insert-item :after #'czm-tex-fold-macro-previous-word)


(defun my-yank-after-advice (&rest _)
  "Fold any yanked ref or eqref."
  (when (and (eq major-mode 'latex-mode)
             TeX-fold-mode
             (string-match "\\\\\\(ref\\|eqref\\){\\([^}]+\\)}"
                           (current-kill 0)))
    (czm-tex-fold-macro-previous-word)))

(advice-add 'yank :after #'my-yank-after-advice)

;;

(use-package czm-tex-jump
  :elpaca (:host github :repo "https://github.com/ultronozm/czm-tex-jump.el.git"
                 :depth nil)
  ;; :after avy
  :bind
  (:map LaTeX-mode-map
        ("s-r" . czm-tex-jump)))

(use-package czm-tex-ref
  :elpaca (:host github :repo "ultronozm/czm-tex-ref.el"
                 :depth nil)
  :custom
  (czm-tex-ref-master-bib-file my-master-bib-file)
  (czm-tex-ref-rearrange-bib-entries t)
  :bind
  (:map global-map
	       ("C-c 0" . czm-tex-ref-bib))
  (:map LaTeX-mode-map
	       ("C-c 9" . czm-tex-ref-label)
	       ("C-c 0" . czm-tex-ref-bib)))

;; (defun czm-attrap-LaTeX-fixer (msg pos end)
;;   (cond
;;    ((s-matches? (rx "Use either `` or '' as an alternative to `\"'.")msg)
;;     (list (attrap-option 'fix-open-dquote
;;             (delete-region pos (1+ pos))
;;             (insert "``"))
;;           (attrap-option 'fix-close-dquote
;;             (delete-region pos (1+ pos))
;;             (insert "''"))))
;;    ((s-matches? (rx "Non-breaking space (`~') should have been used.") msg)
;;     (attrap-one-option 'non-breaking-space
;;       (if (looking-at (rx space))
;;           (delete-region pos (1+ pos))
;;         (delete-region (save-excursion (skip-chars-backward "\n\t ") (point)) (point)))
;;       (insert "~")))
;;    ((s-matches? (rx "Interword spacing (`\\ ') should perhaps be used.") msg)
;;     (attrap-one-option 'use-interword-spacing
;;       (delete-region (1-  (point))
;;                      (point))
;;       (insert "\\ ")))
;;    ((s-matches? (rx "Delete this space to maintain correct pagereferences.") msg)
;;     (attrap-one-option 'fix-space-pageref
;;       (if (looking-back (rx bol (* space)))
;;           (progn (skip-chars-backward "\n\t ")
;;                  (insert "%"))
;;         (delete-region (point) (save-excursion (skip-chars-forward " \t") (point)))
;; 	)))
;;    ((s-matches? (rx "You should enclose the previous parenthesis with `{}'.") msg)
;;     (attrap-one-option 'enclose-with-braces
;;       (insert "}")
;;       (save-excursion
;; 	(backward-char)
;; 	(backward-sexp)
;; 	(re-search-backward "[^[:alnum:]\\_\\/]")
;; 	(forward-char)
;; 	(insert "{")
;; 	)))
;;    ((s-matches? (rx "You should not use punctuation in front of quotes.") msg)
;;     (attrap-one-option 'swap-punctuation-with-quotes
;;       (progn
;; 	(delete-char 2)
;; 	(backward-char)
;; 	(insert "''"))
;;       ))))

(defun czm-attrap-LaTeX-fixer-flymake (msg pos end)
  (cond
   ((s-matches? (rx "Use either `` or '' as an alternative to `\"'.")
                msg)
    (list (attrap-option 'fix-open-dquote
            (delete-region pos (1+ pos))
            (insert "``"))
          (attrap-option 'fix-close-dquote
            (delete-region pos (1+ pos))
            (insert "''"))))
   ((s-matches? (rx "Non-breaking space (`~') should have been used.")
                msg)
    (attrap-one-option 'non-breaking-space
      (if (looking-at (rx space))
          (delete-region pos (1+ pos))
        (delete-region (save-excursion (skip-chars-backward "\n\t ")
                                       (point))
                       (point)))
      (insert "~")))
   ((s-matches? (rx "Interword spacing (`\\ ') should perhaps be used.")
                msg)
    (attrap-one-option 'use-interword-spacing
      (delete-region (point)
                     (1+ (point)))
      (insert "\\ ")))
   ((s-matches? (rx "Delete this space to maintain correct pagereferences.")
                msg)
    ;; not yet fixed
    (attrap-one-option 'fix-space-pageref
      (if (looking-back (rx bol (* space)))
          (progn (skip-chars-backward "\n\t ")
                 (insert "%"))
        (delete-region (point)
                       (save-excursion (skip-chars-forward " \t")
                                       (point)))
	       )))
   ((s-matches? (rx "You should enclose the previous parenthesis with `{}'.")
                msg)
    (attrap-one-option 'enclose-with-braces
      (forward-char)
      (insert "}")
      (save-excursion
	       (backward-char)
	       (backward-sexp)
	       (re-search-backward "[^[:alnum:]\\_\\/]")
	       (forward-char)
	       (insert "{")
	       )))
   ((s-matches? (rx "You should not use punctuation in front of quotes.")
                msg)
    (attrap-one-option 'swap-punctuation-with-quotes
      (progn
	       (forward-char)
        (delete-char 2)
	       (backward-char)
	       (insert "''"))
      ))))

(use-package emacs
  :elpaca nil
  :after flycheck attrap
  :config
  (add-to-list 'attrap-flycheck-checkers-alist '(tex-chktex . czm-attrap-LaTeX-fixer)))

(use-package latex-flymake
  :elpaca nil
  :after latex)

(with-eval-after-load 'attrap
  (setcdr (assoc 'LaTeX-flymake attrap-flymake-backends-alist)
          #'czm-attrap-LaTeX-fixer-flymake))

(defun czm/latex-tmp-new ()
  "Create new temporary LaTeX buffer."
  (interactive)
  (let ((dir (file-name-as-directory my-tmp-tex-dir))
	       (filename (format-time-string "tmp-%Y%m%dT%H%M%S.tex")))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (let ((filepath (expand-file-name filename dir)))
      (find-file filepath)
      (save-buffer)
      ;; (czm-preview-timer-toggle)
      )))

(use-package dynexp
  :elpaca (:host github :repo "ultronozm/dynexp.el"
                 :depth nil)
  :demand ; but after auctex
  :bind
  (:map LaTeX-mode-map
        ("SPC" . dynexp-space)
        ("TAB" . dynexp-next))
  :config
  (with-eval-after-load 'latex
    (quietly-read-abbrev-file "~/.emacs.d/elpaca/repos/dynexp/lisp/dynexp-abbrev.el")))

(use-package czm-tex-edit
  :elpaca (:host github :repo "ultronozm/czm-tex-edit.el"
                 :depth nil)
  :after latex dynexp
  :demand ; should come after latex and dynexp
  :bind
  (:map LaTeX-mode-map
        ("C-c t i" . czm-tex-edit-emphasize)
        ("C-c t a" . czm-tex-edit-alertify)
        ("C-c t b" . czm-tex-edit-bold)
        ("C-c t l" . czm-tex-edit-underline)
        ("C-c t u" . czm-tex-edit-unemphasize)
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
        ("s-<return>" . czm-tex-edit-return)
        ("$" . czm-tex-edit-insert-dollar-or-wrap-region))
  :config
  (czm-tex-edit-define-color-functions-and-bindings
   "C-c t c"
   (("red" . "r") ("green" . "g") ("blue" . "b") ("yellow" . "y") ("orange" . "o") ("purple" . "p") ("black" . "k") ("white" . "w") ("cyan" . "c") ("magenta" . "m") ("lime" . "l") ("teal" . "t") ("violet" . "v") ("pink" . "i") ("brown" . "n") ("gray" . "a") ("darkgreen" . "d") ("lightblue" . "h") ("lavender" . "e") ("maroon" . "u") ("beige" . "j") ("indigo" . "x") ("turquoise" . "q") ("gold" . "f") ("silver" . "s") ("bronze" . "z"))))

(use-package czm-tex-compile
  :elpaca (:host github :repo "ultronozm/czm-tex-compile.el"
                 :depth nil)
  :bind
  ("C-c k" . czm-tex-compile-toggle))

(use-package czm-preview
  :elpaca (:host github :repo "ultronozm/czm-preview.el"
                 :depth nil)
  :after latex
  :mode ("\\.tex\\'" . latex-mode)
  :bind
  (:map LaTeX-mode-map
	       ("s-u" . czm-preview-mode)
	       ("C-c p m" . czm-preview-toggle-master))
  :custom
  (czm-preview-TeX-master my-preview-master)
  (czm-preview-regions-not-to-preview '("<++>" "<+++>"))
  (czm-preview-allowed-files
   '("\\.tex\\(<\\([^>]+\\)>\\)*$"
     "\\[ latex \\]\\*\\(<\\([^>]+\\)>\\)*$"
     "\\.lean$"
     "\\.org$"
     ))
  (czm-preview-predicate #'my-czm-preview-predicate)
  :hook
  (LaTeX-mode . czm-preview-mode-conditionally-enable)

  :config
  (setq-default TeX-PDF-mode nil)
  ;; because texlive 2023 seems super slow
  (with-eval-after-load 'preview
    (let ((tex-dir (when (equal (system-name)
                                "Pauls-MBP-3")
                     "/usr/local/texlive/2020/bin/x86_64-darwin/")))
      (setq preview-LaTeX-command
	           `(
	             ,(concat
	               "%`"
	               tex-dir
	               "%l \"\\nonstopmode\\nofiles\\PassOptionsToPackage{")
	             ("," . preview-required-option-list)
	             "}{preview}\\AtBeginDocument{\\ifx\\ifPreview\\undefined" preview-default-preamble "\\fi}\"%' \"\\detokenize{\" %(t-filename-only) \"}\""))))

  ;; (setq czm-preview-latex-prefix-directory "/usr/local/texlive/2023/bin/universal-darwin/")
  ;; /usr/local/texlive/2023/bin/universal-darwin/

  )

(defun current-mmm-mode ()
  "Return current mmm-mode at point."
  (let ((overlays (overlays-at (point)))
        result)
    (while (and overlays (not result))
      (let* ((overlay (car overlays))
             (properties (overlay-properties overlay))
             (mmm-mode (plist-get properties 'mmm-mode)))
        (setq result mmm-mode)
        (setq overlays (cdr overlays))))
    result))

(defun my-czm-preview-predicate ()
  "Predicate for determining whether to preview.

If major-mode is latex-mode, then return t.

If major-mode is lean4-mode, and if mmm-mode is activated, then
return t precisely when the current mmm-mode is latex-mode.

Otherwise, return nil."
  (cond
   ;; check whether mmm-mode is bound as a symbol first
   (
    (and (boundp 'mmm-mode) mmm-mode)
    (or (eq mmm-primary-mode 'latex-mode)
        (eq (current-mmm-mode) 'latex-mode)))
   ((eq major-mode 'latex-mode) t)
   ;;((eq-major-mode 'org-mode))
   ))

(use-package library
  :after latex czm-tex-util
  :elpaca (:host github :repo "ultronozm/library.el"
                 :depth nil)
  :custom
  (library-pdf-directory "~/Dropbox/math documents/unsorted/")
  (library-bibtex-file my-master-bib-file)
  (library-download-directory "~/Downloads/")
  (library-org-capture-template-key "j")
  :bind
  ;; ("C-c n" . library-clipboard-to-refs)
  )


;; ;; testing this out for a bit, to make sure it works as you hoped
;; (defun LaTeX-env-beginning-pos-col ()
;;   "Return a cons: (POINT . COLUMN) for current environment's beginning."
;;   (save-excursion
;;     (LaTeX-find-matching-begin)
;;     (LaTeX-back-to-indentation)
;;     (cons (point) (current-column))))



(defun preview--skip-preamble-region (region-text region-offset)
  "Skip preamble for the sake of predumped formats.
Helper function of `TeX-region-create'.

If REGION-TEXT doesn't contain preamble, it returns nil.
Otherwise, it returns cons (ALTERED-TEXT . ALTERED-OFFSET) where
ALTERED-TEXT is REGION-TEXT without the preamble part and
ALTERED-OFFSET is REGION-OFFSET increased by the number of lines
of the preamble part of REGION-TEXT."
  (if (and TeX-header-end (string-match TeX-header-end region-text))
      (cons (substring region-text (match-end 0))
            (with-temp-buffer
              (insert (substring region-text 0 (match-end 0)))
              (+ region-offset (TeX-current-offset))))))


(defun czm-copy-standard-tex-files ()
  "Copy standard TeX files to the current directory."
  (interactive)
  ;; ask the user if he really wants to copy files into the current directory
  (if (y-or-n-p (format "Copy standard TeX files to %s? " default-directory))
      (let ((files '(my-common-tex-file my-master-bib-file)))
        (dolist (file files)
          (let ((source (expand-file-name file))
                (dest (expand-file-name (file-name-nondirectory file) default-directory)))
            (copy-file source dest t))))
    (message "Aborted.")))


(when nil
  (defun modify-syntax-table-4-latex ()
    (modify-syntax-entry ?\{ "(}")
    (modify-syntax-entry ?\} "){"))



  (autoload #'latex-forward-sexp "tex-mode" nil t)
  (modify-syntax-entry ?\\ "/" LaTeX-mode-syntax-table)
  (defun fix-LaTeX-sexp ()
    (setq-local forward-sexp-function #'latex-forward-sexp))
  (add-hook 'LaTeX-mode-hook #'fix-LaTeX-sexp)
  )