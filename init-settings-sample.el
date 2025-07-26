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
      my-face-heights '((default . 120)
                        (mode-line . 96)
                        (mode-line-inactive . 96)
                        (tab-bar . 96))
      my-auctex-git-permissions nil)

(customize-set-variable 'user-full-name "Paul D. Nelson")
(customize-set-variable 'user-mail-address "ultrono@gmail.com")

(setenv "LIBRARY_PATH"
        (mapconcat
         #'identity
         '("/opt/homebrew/opt/gcc/lib/gcc/14"
           "/opt/homebrew/opt/libgccjit/lib/gcc/14"
           "/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin23/14")
         ":"))
