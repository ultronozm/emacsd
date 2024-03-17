;; rename this file to "init-settings.el" and edit the following:

(setq my-first-name "Paul"
      my-master-bib-file "~/doit/refs.bib"
      my-common-tex-file "~/doit/common.tex"
      my-todo-file "~/doit/todo.org"
      my-log-file "~/doit/log.org"
      my-old-log-file "~/doit/log-old.org"
      my-preview-master "~/doit/preview-master.tex"
      my-tmp-sage-dir "~/doit/sage"
      my-sage-root "~/sage/sage-9.8"
      my-tmp-tex-dir "~/doit"
      my-tmp-org-dir "~/doit")

(customize-set-variable 'user-full-name "Paul D. Nelson")
(customize-set-variable 'user-mail-address "nelson.paul.david@gmail.com")

(setenv "LIBRARY_PATH"
        (mapconcat
         #'identity
         '("/opt/homebrew/opt/gcc/lib/gcc/13"
           "/opt/homebrew/opt/libgccjit/lib/gcc/13"
           "/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin22/13")
         ":"))