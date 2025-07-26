;; -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)
(eval-after-load 'lisp-mode
  `(add-to-list 'lisp-imenu-generic-expression
                    (list nil ,use-package-form-regexp-eval 2)))
(setq use-package-enable-imenu-support t)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq-default
 default-frame-alist
 '((fullscreen . fullboth)
   (vertical-scroll-bars . nil)))
