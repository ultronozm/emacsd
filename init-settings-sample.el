;; -*- lexical-binding: t; -*-

;; rename this file to "init-settings.el" and edit the following.
;; Relocate everything by editing the three path roots below; the rest derive.

(setq my-first-name "Paul"
      ;; --- path roots (edit these to relocate) ---
      my-doit-dir    "~/doit"      ; work monorepo (kept top-level)
      my-work-dir    "~/work"      ; catch-all parent for working repos
      my-scratch-dir "~/scratch"   ; disposable capture space (not migrated)
      ;; --- doit files (derived) ---
      my-master-bib-file (expand-file-name "refs.bib" my-doit-dir)
      my-common-tex-file (expand-file-name "common.tex" my-doit-dir)
      my-todo-file       (expand-file-name "todo.org" my-doit-dir)
      my-projects-file   (expand-file-name "projects.org" my-doit-dir)
      my-log-file        (expand-file-name "log.org" my-doit-dir)
      my-old-log-file    (expand-file-name "log-old.org" my-doit-dir)
      my-preview-master  (expand-file-name "preview-master.tex" my-doit-dir)
      ;; --- mail (mboxes live in Dropbox, not git) ---
      my-mail-folder "~/Dropbox/mail"
      my-mail-host "gmail.com"
      my-mail-host-imap "imap.gmail.com"
      my-mail-port "993"
      my-mail-user "ultrono@gmail.com"
      my-mail-inbox "imaps://ultrono%40gmail.com@imap.gmail.com:993/n"
      ;; --- other folders ---
      my-downloads-folder "~/Downloads"
      my-pdf-folder "~/Dropbox/math documents/unsorted/"
      my-math-folder "~/Dropbox/math documents/"
      my-publish-math-repo (expand-file-name "math" my-work-dir)
      ;; --- scratch subdirs (derived) ---
      my-scratch-sage-dir     (expand-file-name "sage" my-scratch-dir)
      my-sage-exe "/usr/local/bin/sage"
      my-scratch-tex-dir      (expand-file-name "tex" my-scratch-dir)
      my-scratch-org-dir      (expand-file-name "org" my-scratch-dir)
      my-scratch-cpp-dir      (expand-file-name "cpp" my-scratch-dir)
      my-scratch-gpt-dir      (expand-file-name "gpt" my-scratch-dir)
      my-scratch-markdown-dir (expand-file-name "markdown" my-scratch-dir)
      ;; --- ui / misc ---
      my-face-heights '((default . 150)
                        (mode-line . 120)
                        (mode-line-inactive . 120)
                        (tab-bar . 120)
                        (mode-line-active . 120))
      my-auctex-git-permissions nil
      my-latex-buffer-face '(:height 216 :width normal
                                     :family "Andale Mono")
      my-agent-shell-transcripts-dir "~/Dropbox/agent-transcripts/excerpts"
      my-emacs-source-dir "~/work/emacs/"
      my-firefox-folder "~/Library/Application Support/Firefox")

(customize-set-variable 'user-full-name "Paul D. Nelson")
(customize-set-variable 'user-mail-address "ultrono@gmail.com")

;; Machine-local safe local variables.
(setopt safe-local-variable-directories nil)
(setopt safe-local-variable-values nil)

(setq epa-file-encrypt-to '("CA22DA3EDC6B89A12CFFE1643DA49C122A95D133"))
