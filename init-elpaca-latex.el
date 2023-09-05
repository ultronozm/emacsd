(defun czm-tex-buffer-face ()
  (interactive)
  (setq buffer-face-mode-face
        '(:height 260
                  :width normal
                  :family
                  "Andale Mono"
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
  (unless buffer-face-mode
    (buffer-face-mode 0))
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


(defun frame-on-primary-monitor-p (frame)
  (let* ((monitors (display-monitor-attributes-list))
	 (primary-monitor (car monitors))
	 (frame-monitor (frame-monitor-attributes frame)))
    (equal primary-monitor frame-monitor)))

(defun my-preview-scale-function ()
  (* 1.5 (funcall (preview-scale-from-face))))

(defun my-preview-scale-function ()
  (*
   (expt text-scale-mode-step text-scale-mode-amount)
   (funcall (preview-scale-from-face))
   (if (frame-on-primary-monitor-p (selected-frame))
       1.5
     1.8
     ;; 2.7
     )))

(use-package latex
  :elpaca  (auctex
            :files
            ("*.el" "*.info" "dir" "doc" "etc" "images" "latex" "style")
            :pre-build
            (("./autogen.sh")
             ("./configure"
              "--with-texmf-dir=$(dirname $(kpsexpand '$TEXMFHOME'))"
              "--with-lispdir=.")
             ("make")
             ("make" "install")
             ))
  
  :demand ; otherwise, madness ensues.

  :config
  (setq TeX-data-directory (expand-file-name  "~/.emacs.d/elpaca/builds/auctex"))
  (setq TeX-lisp-directory TeX-data-directory)
  
  :hook
  (LaTeX-mode . TeX-fold-mode)
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . czm-tex-setup-environments-and-outline-regexp)
  (LaTeX-mode . czm-tex-buffer-face)
  (LaTeX-mode . outline-minor-mode)
  (LaTeX-mode . abbrev-mode)
  (LaTeX-mode . (lambda () (setq fill-column 999999)))

  :bind
  (:map LaTeX-mode-map
        ("s-a" . abbrev-mode)
	("s-c" . preview-clearout-at-point)
        ("s-q" . LaTeX-fill-buffer)
        ("C-c C-n" . nil)               ; TeX-normal-mode
        ("C-c #" . nil))
  
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


  
  (preview-scale-function #'my-preview-scale-function)
  ;; (preview-scale-function 1.5) ; maybe enable, or see what else you were doing?
  (reftex-derive-label-parameters
   '(15 50 t 1 "-"
	("the" "on" "in" "off" "a" "for" "by" "of" "and" "is" "to")
	t)))

;;  don't want foldout to include "bibliography"
(defun czm-LaTeX-outline-level-advice (orig-fun &rest args)
  (if (looking-at "\\\\bibliography") 1 (apply orig-fun args)))

(use-package foldout
  :elpaca nil
  :config
  (advice-add 'LaTeX-outline-level :around #'czm-LaTeX-outline-level-advice))


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
     ("<left>" outline-promote windmove-left)
     ("<right>" outline-demote windmove-right)
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

(use-package latex-extra
  :after latex
  :bind
  (:map latex-extra-mode-map
        ("TAB" . nil)
        ("C-M-a" . latex/beginning-of-environment)
        ("C-M-e" . latex/end-of-environment))
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
  (czm-tex-fold-bib-file "~/doit/refs.bib")
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
  (czm-tex-ref-master-bib-file "~/doit/refs.bib")
  (czm-tex-ref-rearrange-bib-entries t)
  :bind
  (:map global-map
	("C-c 0" . czm-tex-ref-bib))
  (:map LaTeX-mode-map
	("C-c 9" . czm-tex-ref-label)
	("C-c 0" . czm-tex-ref-bib)))

(defun czm-attrap-LaTeX-fixer (msg pos end)
  (cond
   ((s-matches? (rx "Use either `` or '' as an alternative to `\"'.")msg)
    (list (attrap-option 'fix-open-dquote
            (delete-region pos (1+ pos))
            (insert "``"))
          (attrap-option 'fix-close-dquote
            (delete-region pos (1+ pos))
            (insert "''"))))
   ((s-matches? (rx "Non-breaking space (`~') should have been used.") msg)
    (attrap-one-option 'non-breaking-space
      (if (looking-at (rx space))
          (delete-region pos (1+ pos))
        (delete-region (save-excursion (skip-chars-backward "\n\t ") (point)) (point)))
      (insert "~")))
   ((s-matches? (rx "Interword spacing (`\\ ') should perhaps be used.") msg)
    (attrap-one-option 'use-interword-spacing
      (delete-region pos (1+ pos))
      (insert "\\ ")))
   ((s-matches? (rx "Delete this space to maintain correct pagereferences.") msg)
    (attrap-one-option 'fix-space-pageref
      (if (looking-back (rx bol (* space)))
          (progn (skip-chars-backward "\n\t ")
                 (insert "%"))
        (delete-region (point) (save-excursion (skip-chars-forward " \t") (point)))
	)))
   ((s-matches? (rx "You should enclose the previous parenthesis with `{}'.") msg)
    (attrap-one-option 'enclose-with-braces
      (insert "}")
      (save-excursion
	(backward-char)
	(backward-sexp)
	(re-search-backward "[^[:alnum:]\\_\\/]")
	(forward-char)
	(insert "{")
	)))
   ((s-matches? (rx "You should not use punctuation in front of quotes.") msg)
    (attrap-one-option 'swap-punctuation-with-quotes
      (progn
	(delete-char 2)
	(backward-char)
	(insert "''"))
      ))))

(use-package emacs
  :elpaca nil
  :after flycheck attrap
  :config
  (add-to-list 'attrap-flycheck-checkers-alist '(tex-chktex . czm-attrap-LaTeX-fixer)))

(defun czm/latex-tmp-new ()
  "Create new temporary LaTeX buffer."
  (interactive)
  (let ((dir "~/doit/")
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
        ("TAB" . dynexp-next)))

(use-package czm-tex-edit
  :elpaca (:host github :repo "ultronozm/czm-tex-edit.el"
                 :depth nil)
  :after latex dynexp
  :demand ; should come after latex and dynexp
  :bind
  (:map LaTeX-mode-map
        ("C-c t i" . czm-tex-edit-emphasize)
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
        ("s-<return>" . czm-tex-edit-return))
  :config
  (czm-tex-edit-define-color-functions-and-bindings
   "C-c t c"
   (("red" . "r") ("green" . "g") ("blue" . "b") ("yellow" . "y") ("orange" . "o") ("purple" . "p") ("black" . "k") ("white" . "w") ("cyan" . "c") ("magenta" . "m") ("lime" . "l") ("teal" . "t") ("violet" . "v") ("pink" . "i") ("brown" . "n") ("gray" . "a") ("darkgreen" . "d") ("lightblue" . "h") ("lavender" . "e") ("maroon" . "u") ("beige" . "j") ("indigo" . "x") ("turquoise" . "q") ("gold" . "f") ("silver" . "s") ("bronze" . "z"))))

(use-package czm-tex-compile
  :elpaca (:host github :repo "ultronozm/czm-tex-compile.el"
                 :depth nil)
  :bind
  ("C-c k" . czm-tex-compile)
  ("s-]" . czm-tex-compile-next-error)
  ("s-[" . czm-tex-compile-previous-error))

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
  (czm-preview-TeX-master "~/doit/preview-master.tex")
  (czm-preview-regions-not-to-preview '("<++>" "<+++>"))
  :hook
  (LaTeX-mode . czm-preview-mode)

  :config
  (setq-default TeX-PDF-mode nil)
  ;; because texlive 2023 seems super slow
  (with-eval-after-load 'preview
    (let ((tex-dir (when (equal (system-name) "Pauls-MBP-3")
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


(use-package library
  :after latex czm-tex-util
  :elpaca (:host github :repo "ultronozm/library.el"
                 :depth nil)
  :bind
  ("C-c n" . library-clipboard-to-refs))


;; ;; testing this out for a bit, to make sure it works as you hoped
;; (defun LaTeX-env-beginning-pos-col ()
;;   "Return a cons: (POINT . COLUMN) for current environment's beginning."
;;   (save-excursion
;;     (LaTeX-find-matching-begin)
;;     (LaTeX-back-to-indentation)
;;     (cons (point) (current-column))))
