;; -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)
(with-eval-after-load 'use-package-core
  (eval-after-load 'lisp-mode
    `(add-to-list 'lisp-imenu-generic-expression
                  (list nil ,use-package-form-regexp-eval 2))))
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
