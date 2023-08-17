(setq package-enable-at-startup nil)
(cond
 ((equal (system-name) "d51735")
  (setenv "LIBRARY_PATH"
          (mapconcat
           #'identity
           ("/opt/homebrew/opt/gcc/lib/gcc/13"
            "/opt/homebrew/opt/libgccjit/lib/gcc/13"
            "/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin22/13")
           ":")))
 ((equal (system-name) "Pauls-MBP-3")
   (setenv "LIBRARY_PATH"
           (mapconcat
            #'identity
            '("/usr/local/var/homebrew/linked/gcc/lib/gcc/13/"
              "/usr/local/var/homebrew/linked/libgccjit/lib/gcc/13/"
              "/usr/local/var/homebrew/linked/gcc/lib/gcc/13/gcc/x86_64-apple-darwin21/13/")
            ":"))))

