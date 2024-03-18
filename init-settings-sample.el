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
      my-tmp-sage-dir "~/scratch/sage"
      my-sage-root "~/sage/sage-9.8"
      my-tmp-tex-dir "~/scratch/tex"
      my-tmp-org-dir "~/scratch/org"
      my-tmp-cpp-dir "~/scratch/cpp"
      my-tmp-gpt-dir "~/scratch/gpt")

(customize-set-variable 'user-full-name "Paul D. Nelson")
(customize-set-variable 'user-mail-address "nelson.paul.david@gmail.com")

(setenv "LIBRARY_PATH"
        (mapconcat
         #'identity
         '("/opt/homebrew/opt/gcc/lib/gcc/13"
           "/opt/homebrew/opt/libgccjit/lib/gcc/13"
           "/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin22/13")
         ":"))
