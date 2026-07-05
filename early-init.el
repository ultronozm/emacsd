;; -*- lexical-binding: t; -*-

;; Disable GC and file-name handlers during startup; restored below.
;; The handler restore merges rather than assigns, in case anything
;; registered a handler during init.  The timer is a fallback in case
;; an error in an earlier `emacs-startup-hook' entry skips the rest of
;; the hook.
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)
(defun my/restore-startup-defaults ()
  ;; Must stay idempotent: it runs from both the hook and the timer.
  ;; `append' (never `nconc') -- with shared list structure, nconc
  ;; would build a circular list and delete-dups would hang.
  (setq gc-cons-threshold (* 32 1024 1024))
  (setq file-name-handler-alist
        (delete-dups (append file-name-handler-alist
                             my/file-name-handler-alist))))
(add-hook 'emacs-startup-hook #'my/restore-startup-defaults 90)
(run-with-timer 10 nil #'my/restore-startup-defaults)

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
(setq initial-frame-alist '(
                            ;; (fullscreen . fullboth)
                            (vertical-scroll-bars . nil)))
(setq default-frame-alist '((vertical-scroll-bars . nil)
                            ;; (width . 80) ;; default
                            ;; (height . 36) ;; default
                            (width . 120)
                            (height . 54)))
(unless (display-graphic-p)
  (setq frame-background-mode 'light))
