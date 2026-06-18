;; -*- lexical-binding: t; -*-

;; rename this file to "init-settings.el" and edit the following: 

(setq my-first-name "Paul"
      my-master-bib-file "~/doit/refs.bib"
      my-common-tex-file "~/doit/common.tex"
      my-todo-file "~/doit/todo.org"
      my-projects-file "~/doit/projects.org"
      my-log-file "~/doit/log.org"
      my-mail-folder "~/mail"
      my-mail-host "gmail.com"
      my-mail-host-imap "imap.gmail.com"
      my-mail-port "993"
      my-mail-user "ultrono@gmail.com"
      my-mail-inbox "imaps://ultrono%40gmail.com@imap.gmail.com:993/n"
      my-old-log-file "~/doit/log-old.org"
      my-preview-master "~/doit/preview-master.tex"
      my-downloads-folder "~/Downloads"
      my-pdf-folder "~/Dropbox/math documents/unsorted/"
      my-math-folder "~/Dropbox/math documents/"
      my-publish-math-repo "~/math"
      my-scratch-sage-dir "~/scratch/sage"
      my-sage-exe "/usr/local/bin/sage"
      my-scratch-tex-dir "~/scratch/tex"
      my-scratch-org-dir "~/scratch/org"
      my-scratch-cpp-dir "~/scratch/cpp"
      my-scratch-gpt-dir "~/scratch/gpt"
      my-scratch-markdown-dir "~/scratch/markdown"
      my-face-heights '((default . 150)
                        (mode-line . 120)
                        (mode-line-inactive . 120)
                        (tab-bar . 120)
                        (mode-line-active . 120))
      my-auctex-git-permissions nil
      my-latex-buffer-face '(:height 216 :width normal
                                     :family "Andale Mono")
      my-agent-shell-transcripts-dir "~/Dropbox/agent-transcripts/excerpts"
      my-emacs-source-dir "~/gnu-emacs/"
      my-firefox-folder "~/Library/Application Support/Firefox")

(customize-set-variable 'user-full-name "Paul D. Nelson")
(customize-set-variable 'user-mail-address "ultrono@gmail.com")

(setopt safe-local-variable-directories
        (mapcar #'expand-file-name
                '("~/repos/nla-main/"
                  "~/repos/nla-prep/"
                  "~/ua/"
                  "~/OneDrive - Aarhus universitet/Paul's OneDrive folder/DNRF/application/"
                  "/Users/Shared/workspace/noobajit/")))

(setq my-extra-safe-local-variable-values
      '((eval add-to-list 'LaTeX-indent-begin-exceptions-list "ifs")
        (czm-preview-TeX-master
         . "~/doit/preview-master-principal_cg.tex")))

(setq epa-file-encrypt-to '("CA22DA3EDC6B89A12CFFE1643DA49C122A95D133"))
