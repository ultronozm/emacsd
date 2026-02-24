;; -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)
(with-eval-after-load 'use-package-core
  (eval-after-load 'lisp-mode
    `(let ((symbol-regexp
            (or (bound-and-true-p lisp-mode-symbol-regexp)
                "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")))
       (add-to-list 'lisp-imenu-generic-expression
                    (list "Packages" ,use-package-form-regexp-eval 2))
       (add-to-list 'lisp-imenu-generic-expression
                    (list "Packages"
                          (concat "^\\s-*(use-package-full\\s-+\\("
                                  symbol-regexp
                                  "\\)")
                          1)))))
(setq use-package-enable-imenu-support t)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq initial-frame-alist '((fullscreen . fullboth)
                            (vertical-scroll-bars . nil)))
(setq default-frame-alist '((vertical-scroll-bars . nil)
                            ;; (width . 80) ;; default
                            ;; (height . 36) ;; default
                            (width . 120)
                            (height . 54)))
(unless (display-graphic-p)
  (setq frame-background-mode 'light))
