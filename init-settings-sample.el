;; rename this file to "init-settings.el" and edit the following:

(setq my-first-name "Paul"
      my-master-bib-file "~/doit/refs.bib"
      my-common-tex-file "~/doit/common.tex"
      my-todo-file "~/doit/todo.org"
      my-log-file "~/doit/log.org"
      my-old-log-file "~/doit/log-old.org"
      my-preview-master "~/doit/preview-master.tex"
      my-downloads-folder "~/Downloads"
      my-pdf-folder "~/Dropbox/math documents/unsorted/"
      my-math-folder "~/Dropbox/math documents/"
      my-scratch-sage-dir "~/scratch/sage"
      my-sage-exe "/usr/local/bin/sage"
      my-scratch-tex-dir "~/scratch/tex"
      my-scratch-org-dir "~/scratch/org"
      my-scratch-cpp-dir "~/scratch/cpp"
      my-scratch-gpt-dir "~/scratch/gpt")

(customize-set-variable 'user-full-name "Paul D. Nelson")
(customize-set-variable 'user-mail-address "nelson.paul.david@gmail.com")

(setenv "LIBRARY_PATH"
        (mapconcat
         #'identity
         '("/opt/homebrew/opt/gcc/lib/gcc/14"
           "/opt/homebrew/opt/libgccjit/lib/gcc/14"
           "/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin23/14")
         ":"))
