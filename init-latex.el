;;; -*- lexical-binding: t; -*-

(defun czm-tex-buffer-face ()
  (interactive)
  (setq buffer-face-mode-face
        '(:height 216
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
                                          '("bibliography"
                                            "begin{thebibliography"))
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
                             (let ((symtex--calc-allow-functions nil))
                               (calc-grab-region beg end '(4)))))

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

(package-install 'auctex)

(use-package emacs
  :after latex

  ;; :vc (:url "https://git.savannah.gnu.org/git/auctex.git")

  ;; :vc t
  ;; :load-path "~/.emacs.d/elpaca/repos/auctex"

  ;; (auctex
  ;;  :files
  ;;  ("*.el" "*.info" "dir" "doc" "etc" "images" "latex" "style")
  ;;  :pre-build
  ;;  (("./autogen.sh")
  ;;   ("./configure"
  ;;    "--with-texmf-dir=$(dirname $(kpsexpand '$TEXMFHOME'))"
  ;;    "--with-lispdir=.")
  ;;   ("make")
  ;;   ("make" "install")))

  ;; :demand                               ; otherwise, madness ensues.

  :config
  (add-to-list 'TeX-file-extensions "tex\\.~[0-9a-f]+~")

  (with-eval-after-load 'org-src
    (push '("latex" . LaTeX) org-src-lang-modes))

  :mode ("\\.tex\\'" . LaTeX-mode)

  :hook
  (latex-mode . LaTeX-mode) ;; absurd that this needs to be added
  ;; (LaTeX-mode . TeX-fold-mode)
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . czm-tex-setup-environments-and-outline-regexp)
  (LaTeX-mode . czm-tex-buffer-face)
  (LaTeX-mode . outline-minor-mode)
  (LaTeX-mode . abbrev-mode)
  ;; (LaTeX-mode . toggle-word-wrap)
  (LaTeX-mode . visual-line-mode)
  (LaTeX-mode . (lambda ()
                  (setq fill-column 999999)))

  :bind
  (:map LaTeX-mode-map
        ("s-c" . preview-clearout-at-point)
        ("s-q" . LaTeX-fill-buffer)

        ("C-c m" . latex-math-from-calc)
        ("C-c C-g" . czm-latex-calc-grab)
        ("C-c C-n" . nil)
                                        ; TeX-normal-mode
        ("C-c #" . nil)
        ;; ("M-n" . czm-TeX-next-error-wrapper)
        ;; ("M-p" . czm-TeX-previous-error-wrapper)
        ([remap next-error])
        ([remap previous-error])
        ("M-n" . next-error)
        ("M-p" . previous-error))

  :config
  (put 'LaTeX-narrow-to-environment 'disabled nil)
  (TeX-source-correlate-mode)
  (advice-add 'TeX-view :around #'czm-widen-first) ; fixes bug in TeX-view

  :config
  ;; (require 'texmathp)
  (defun LaTeX-skip-verbatim (orig-fun &rest args)
    (if (or (eq major-mode 'latex-mode)
            (eq major-mode 'LaTeX-mode))
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
    (advice-add f :around #'LaTeX-skip-verbatim))

  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (preview-auto-cache-preamble t)
  (preview-image-type 'dvipng)
  ;; (preview-default-option-list
  ;;  '("displaymath" "floats" "graphics" "textmath" "sections" "footnotes" "showlabels"))
                                        ;  (preview-gs-command "/usr/local/bin/gs")  ; compare with rungs?
                                        ;  (preview-image-type 'pnm) ; compare with png?



  (reftex-derive-label-parameters
   '(15 50 t 1 "-"
        ("the" "on" "in" "off" "a" "for" "by" "of" "and" "is" "to")
        t))

  :custom-face (preview-face ((t (:background nil)))))

;;  don't want foldout to include "bibliography"
(defun czm-LaTeX-outline-level-advice (orig-fun &rest args)
  (if (looking-at "\\\\bibliography\\|\\\\begin{thebibliography}") 1 (apply orig-fun args)))

(defun my-preview-tailor-factor-function ()
  "ez"
  (if (string-suffix-p ".lean" (buffer-file-name)) 0.6 0.833))

(unless (package-installed-p 'preview-tailor)
  (package-vc-install "https://github.com/ultronozm/preview-tailor.el"))
(use-package preview-tailor
  ;; :vc (:url "https://github.com/ultronozm/preview-tailor.el")
  :after preview
  :demand
  :config
  (preview-tailor-init)
  :hook
  (kill-emacs . preview-tailor-save)
  :custom
  (preview-tailor-additional-factor-function #'my-preview-tailor-factor-function))

(use-package foldout
  :ensure nil
  :config
  (advice-add 'LaTeX-outline-level :around #'czm-LaTeX-outline-level-advice))

;; (spw/remap-mark-command 'sp-mark-sexp LaTeX-mode-map)

(unless (package-installed-p 'czm-tex-util)
  (package-vc-install "https://github.com/ultronozm/czm-tex-util.el"))
(use-package czm-tex-util
  ;; :vc (:url "https://github.com/ultronozm/czm-tex-util.el")
  :after latex)

(defun czm-tex-quote-advice (&rest _)
  (when (and TeX-fold-mode
             (looking-back "``\\(.*?\\)''"))
    (czm-tex-fold-quotes (match-beginning 0) (match-end 0))))

(defun czm-tex-fold-macro-previous-word ()
  (interactive)
  (if TeX-fold-mode
      (save-excursion
	(backward-word)
	(TeX-fold-item 'macro))))

(defun my-yank-after-advice (&rest _)
  "Fold any yanked ref or eqref."
  (when (and (or (eq major-mode 'latex-mode)
                 (eq major-mode 'LaTeX-mode))
             TeX-fold-mode
             (string-match "\\\\\\(ref\\|eqref\\){\\([^}]+\\)}"
                           (current-kill 0)))
    (czm-tex-fold-macro-previous-word)))

(defun czm-setup-and-activate-tex-fold ()
  (require 'czm-tex-fold)
  (require 'czm-tex-jump)
  (require 'czm-tex-ref)
  (czm-tex-fold-set-defaults)
  (czm-tex-fold-install)
  (TeX-fold-mode 1)
  (auctex-label-numbers-mode 1)
  (advice-add 'TeX-insert-quote :after #'czm-tex-quote-advice)
  (advice-add 'LaTeX-insert-item :after #'czm-tex-fold-macro-previous-word)
  (advice-add 'yank :after #'my-yank-after-advice)
  (remove-hook 'LaTeX-mode-hook #'czm-setup-and-activate-tex-fold)
  (add-hook 'LaTeX-mode-hook #'TeX-fold-mode))

(defun czm-abbreviate-latex-mode-name ()
  (setq TeX-base-mode-name "L"))

(add-hook 'LaTeX-mode-hook #'czm-abbreviate-latex-mode-name)

(unless (package-installed-p 'czm-tex-fold)
  (package-vc-install "https://github.com/ultronozm/czm-tex-fold.el"))
(use-package czm-tex-fold
  ;; :vc (:url "https://github.com/ultronozm/czm-tex-fold.el")
  :after latex
  :custom
  (czm-tex-fold-bib-file my-master-bib-file)
  :hook
  (LaTeX-mode . czm-setup-and-activate-tex-fold))

(unless (package-installed-p 'czm-tex-jump)
  (package-vc-install "https://github.com/ultronozm/czm-tex-jump.el"))
(use-package czm-tex-jump
  ;; :vc (:url "https://github.com/ultronozm/czm-tex-jump.el")
  ;; :after avy
  :after latex
  :bind
  (:map LaTeX-mode-map
        ("s-r" . czm-tex-jump-avy))
  :hook (LaTeX-mode . czm-tex-jump-setup))

(unless (package-installed-p 'czm-tex-ref)
  (package-vc-install "https://github.com/ultronozm/czm-tex-ref.el"))
(use-package czm-tex-ref
  ;; :vc (:url "https://github.com/ultronozm/czm-tex-ref.el")
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
   ((s-matches? (rx "Intersentence spacing (`\\@') should perhaps be used.")
                msg)
    (attrap-one-option 'use-intersentence-spacing
      (insert "\\@")))
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
        (insert "''"))))))

(use-package emacs
  :ensure nil
  :after flycheck attrap
  :config
  (add-to-list 'attrap-flycheck-checkers-alist '(tex-chktex . czm-attrap-LaTeX-fixer)))

(use-package latex-flymake
  :ensure nil
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

(unless (package-installed-p 'dynexp)
  (package-vc-install "https://github.com/ultronozm/dynexp.el"))
(use-package dynexp
  ;; :vc (:url "https://github.com/ultronozm/dynexp.el")
  :after latex
  :bind
  (:map LaTeX-mode-map
        ("SPC" . dynexp-space)
        ("TAB" . dynexp-next))
  :config
  (with-eval-after-load 'latex
    (quietly-read-abbrev-file "~/.emacs.d/elpa/dynexp/lisp/dynexp-abbrev.el")))

(unless (package-installed-p 'czm-tex-edit)
  (package-vc-install "https://github.com/ultronozm/czm-tex-edit.el"))
(use-package czm-tex-edit
  ;; :vc (:url "https://github.com/ultronozm/czm-tex-edit.el")
  :after latex dynexp
  ;; :demand ; should come after latex and dynexp
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
        ("$" . czm-tex-edit-insert-dollar-or-wrap-region)
        ("\"" . czm-tex-edit-insert-quote-or-wrap-region))
  :config
  (czm-tex-edit-define-color-functions-and-bindings
   "C-c t c"
   (("red" . "r") ("green" . "g") ("blue" . "b") ("yellow" . "y") ("orange" . "o") ("purple" . "p") ("black" . "k") ("white" . "w") ("cyan" . "c") ("magenta" . "m") ("lime" . "l") ("teal" . "t") ("violet" . "v") ("pink" . "i") ("brown" . "n") ("gray" . "a") ("darkgreen" . "d") ("lightblue" . "h") ("lavender" . "e") ("maroon" . "u") ("beige" . "j") ("indigo" . "x") ("turquoise" . "q") ("gold" . "f") ("silver" . "s") ("bronze" . "z"))))

(unless (package-installed-p 'auctex-cont-latexmk)
  (package-vc-install "https://github.com/ultronozm/auctex-cont-latexmk.el"))
(use-package auctex-cont-latexmk
  :after latex
  :bind
  (:map LaTeX-mode-map
        ("C-c k" . auctex-cont-latexmk-toggle))
  :custom
  (auctex-cont-latexmk-command
   '("latexmk -pvc -shell-escape -pdf -view=none -e " ("$pdflatex=q/pdflatex %O -synctex=1 -interaction=nonstopmode %S/"))))

(setq TeX-ignore-warnings "Package hyperref Warning: Token not allowed in a PDF string")
;; (setq TeX-suppress-ignored-warnings t)


(defun my/set-TeX-master ()
  (setq-local TeX-master
              "~/doit/preview-master.tex"))

(add-hook 'prog-mode-hook #'my/set-TeX-master)

(unless (package-installed-p 'preview-auto)
  (package-vc-install "https://github.com/ultronozm/preview-auto.el"))
(use-package preview-auto
  :after latex
  :config
  (setq preview-protect-point t)
  (setq preview-locating-previews-message nil)
  (setq preview-leave-open-previews-visible t)
  :custom
  (preview-auto-interval 0.1)
  (preview-LaTeX-command-replacements
   '(preview-LaTeX-disable-pdfoutput)))

(unless (package-installed-p 'auctex-label-numbers)
  (package-vc-install "https://github.com/ultronozm/auctex-label-numbers.el"))
(use-package auctex-label-numbers
  :after latex czm-tex-fold)

(unless (package-installed-p 'library)
  (package-vc-install "https://github.com/ultronozm/library.el"))
(use-package library
  ;; :vc (:url "https://github.com/ultronozm/library.el")
  :after latex czm-tex-util
  :custom
  (library-pdf-directory my-pdf-folder)
  (library-bibtex-file my-master-bib-file)
  (library-download-directory my-downloads-folder)
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
      (let ((files (list my-common-tex-file my-master-bib-file)))
        (dolist (file files)
          (let ((source (expand-file-name file))
                (dest (expand-file-name (file-name-nondirectory file) default-directory)))
            (copy-file source dest t))))
    (message "Aborted.")))

(defvar czm-tex-avy-jump-regexp
  "\\(. \\$\\|..\n[[:space:]]*\\\\begin{\\(eq\\|ali\\)\\)")

(defun czm-tex-avy-jump ()
  (interactive)
  (avy-jump czm-tex-avy-jump-regexp
            :action (lambda (pos)
                      (goto-char pos)
                      (forward-char 2)
                      (let ((this-command #'tex-parens-down-list))
                        (tex-parens-down-list)))))

(defun czm-tex-avy-copy ()
  (interactive)
  (avy-jump czm-tex-avy-jump-regexp
            :action (lambda (pos)
                      (save-excursion
                        (copy-region-as-kill
                         (+ pos 2)
                         (save-excursion
                           (goto-char (+ pos 2))
                           (tex-parens-forward-list)
                           (point))))
                      (yank))))

;; needs a bit of work -- should kill the line when it's empty, take a numeric arg, etc
(defun czm-tex-soft-kill ()
  (interactive)
  (let* ((eol (save-excursion (end-of-visual-line) (point)))
         (last-point (point))
         (soft-eol
          (save-excursion
            (tex-parens-forward-sexp)
            (while (and (< (point) eol)
                        (> (point) last-point))
              (setq last-point (point))
              (tex-parens-forward-sexp))
            (min (point) eol))))
    (kill-region (point) soft-eol)))

(defun czm-tex-jump-back-with-breadcrumb ()
  (interactive)
  (save-excursion
    (insert "<++>"))
  (let ((this-command #'tex-parens-down-list))
    (tex-parens-backward-down-list)))

(defun czm-tex-mark-inner ()
  (interactive)
  (tex-parens-backward-up-list)
  (tex-parens-down-list)
  (set-mark (point))
  (tex-parens-up-list)
  (tex-parens-backward-down-list))

(defun czm-tex-end-of-list ()
  (interactive)
  (let ((last (point)))
    (tex-parens-forward-sexp)
    (while (> (point) last)
      (setq last (point))
      (tex-parens-forward-sexp))))

(defun czm-tex-beginning-of-list ()
  (interactive)
  (let ((last (point)))
    (tex-parens-backward-sexp)
    (while (< (point) last)
      (setq last (point))
      (tex-parens-backward-sexp))))

(defun czm-tex-forward-sentence-or-end-of-list ()
  (interactive)
  (if (texmathp)
      (let ((last (point)))
        (tex-parens-forward-sexp)
        (while (> (point) last)
          (setq last (point))
          (tex-parens-forward-sexp)))
    (forward-sentence)))

(defun czm-tex-backward-sentence-or-beginning-of-list ()
  (interactive)
  (if (texmathp)
      (let ((last (point)))
        (tex-parens-backward-sexp)
        (while (< (point) last)
          (setq last (point))
          (tex-parens-backward-sexp)))
    (backward-sentence)))

(unless (package-installed-p 'tex-parens)
  (package-vc-install "https://github.com/ultronozm/tex-parens.el"))
(use-package tex-parens
  ;; :vc (:url "https://github.com/ultronozm/tex-parens.el")
  :bind
  (:map LaTeX-mode-map
        ("M-i" . tex-parens-mark-inner)
        ("s-j" . czm-tex-avy-jump)
        ("s-c" . czm-tex-avy-copy)
        ("s-e" . tex-parens-end-of-list)
        ("s-a" . tex-parens-beginning-of-list))

  :hook
  (tex-mode . tex-parens-mode)
  (LaTeX-mode . tex-parens-mode)

  :config
  (defun czm-tex-parens-expand-abbrev-advice (orig-fun &rest args)
    (require 'tex-parens)
    (unless (tex-parens--comment)
      (apply orig-fun args)))

  (advice-add 'expand-abbrev :around #'czm-tex-parens-expand-abbrev-advice)

  (define-repeat-map tex-parens-structural-edit
    ("n" tex-parens-forward-list
     "p" tex-parens-backward-list
     "u" tex-parens-backward-up-list
     "M-u" tex-parens-up-list
     "g" tex-parens-down-list
     "M-g" tex-parens-backward-down-list)
    (:continue
     "f" tex-parens-forward-sexp
     "b" tex-parens-backward-sexp
     "a" beginning-of-defun
     "e" end-of-defun
     "d" czm-deactivate-mark-interactively
     "k" kill-sexp
     ">" tex-parens-burp-right
     "<" tex-parens-burp-left
     "C-/" undo
     "r" tex-parens-raise-sexp
     "/" tex-parens-delete-pair
     "t" transpose-sexps
     "w" kill-region
     "M-w" kill-ring-save
     "y" yank
     "c" lispy-clone
     ;; "C-M-SPC" spw/tex-parens-mark-sexp
     "RET" TeX-newline))
  (repeat-mode 1))

(unless (package-installed-p 'tex-item)
  (package-vc-install "https://github.com/ultronozm/tex-item.el"))
(use-package tex-item
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

(defalias 'czm-setup-tex-file
  (kmacro "l t x SPC s-s s-p z C-n C-n C-c C-p C-a C-c C-p C-f"))
