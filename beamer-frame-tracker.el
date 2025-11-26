;;; beamer-frame-tracker.el --- Auto-compile and view the current Beamer frame -*- lexical-binding: t; -*-

;; Author: Your Name Here <you@example.com>
;; Version: 4.0
;; Package-Requires: ((emacs "26.1") (auctex "13.1"))
;; Keywords: tex, beamer, convenience, tools
;; URL: https://github.com/you/beamer-frame-tracker

;;; Commentary:
;;
;; `beamer-frame-tracker-mode' provides a live-preview experience for
;; editing individual frames in a Beamer presentation.
;;
;; This mode uses a simple, robust architecture that decouples
;; compilation and viewing for maximum reliability.
;;
;; How it Works:
;; 1.  Each frame is assigned a unique, persistent temporary filename.
;; 2.  When you modify a frame and pause, the mode starts a LaTeX
;;     compilation for that frame IN THE BACKGROUND. It does not block
;;     Emacs and does not try to automatically open the viewer.
;; 3.  When you move the point into any tracked frame, the mode checks if a
;;     corresponding PDF exists. If it does, it displays it non-interactively.
;;
;; This design is simple, predictable, and avoids the interactive
;; prompts and race conditions of more complex asynchronous approaches.
;;
;; To use, enable it in a TeX buffer with `M-x beamer-frame-tracker-mode`.
;; To enable it automatically for all Beamer documents, add this to your
;; init file:
;;
;;   (add-hook 'LaTeX-mode-hook #'beamer-frame-tracker-auto-on)

;;; Code:

(require 'tex)
(require 'latex)
(eval-when-compile (require 'cl-lib))

;;;; Customization & Constants -----------------------------------------------

(defgroup beamer-frame-tracker nil
  "Automatically compile the current Beamer frame after edits."
  :group 'TeX :prefix "bft-")

(defcustom bft-idle-delay 0.75
  "Seconds of idle time to wait after modification before compiling."
  :type 'number
  :group 'beamer-frame-tracker)

(defcustom bft-environment-name "frame"
  "The name of the LaTeX environment to track."
  :type 'string
  :group 'beamer-frame-tracker)

;;;; Buffer-Local State ------------------------------------------------------

(defvar-local bft--tracked-overlays nil
  "A list of all tracked frame overlays in this buffer.")
(defvar-local bft--current-overlay nil
  "The tracked overlay that the point is currently inside.")
(defvar-local bft--file-prefix nil
  "A unique, stable prefix for temp files for this buffer.")

;;;; Minor Mode Definition ---------------------------------------------------

;;;###autoload
(define-minor-mode beamer-frame-tracker-mode
  "Compile Beamer frames on edit, view on entry."
  :lighter " BFT"
  :group 'beamer-frame-tracker
  (if beamer-frame-tracker-mode
      (bft--enable)
    (bft--disable)))

;;;###autoload
(defun beamer-frame-tracker-auto-on ()
  "Enable `beamer-frame-tracker-mode' in documents using the Beamer class."
  (when (and (eq major-mode 'latex-mode)
             (stringp buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (format "\\\\documentclass\\b[^}]*\\{%s\\}"
                                       (regexp-quote "beamer"))
                               nil t)
        (beamer-frame-tracker-mode 1)))))

;;;; Mode Activation and Hooks ------------------------------------------------

(defun bft--enable ()
  "Activate hooks and initialize state for the minor mode."
  (setq bft--file-prefix (format "_bft_%s" (substring (secure-hash 'md5 (buffer-file-name)) 0 8)))
  (add-hook 'post-command-hook #'bft--post-command-hook nil 'local)
  (add-hook 'after-revert-hook #'bft--rescan-after-revert nil 'local)
  (bft--post-command-hook))

(defun bft--disable ()
  "Deactivate hooks and clean up all state."
  (remove-hook 'post-command-hook #'bft--post-command-hook 'local)
  (remove-hook 'after-revert-hook #'bft--rescan-after-revert 'local)
  (bft--cleanup))

(defun bft--cleanup ()
  "Cancel all timers, delete overlays, and remove temporary files."
  (dolist (ov bft--tracked-overlays)
    (when-let ((timer (overlay-get ov :bft-timer)))
      (cancel-timer timer))
    (delete-overlay ov))
  (when bft--file-prefix
    (let* ((pattern (expand-file-name (concat bft--file-prefix ".*")
                                      (TeX-master-directory)))
           (files (file-expand-wildcards pattern)))
      (dolist (file files)
        (ignore-errors (delete-file file)))))
  (setq bft--tracked-overlays nil
        bft--current-overlay nil))

(defun bft--rescan-after-revert ()
  "After reverting buffer, clear overlays and re-scan from point."
  (dolist (ov bft--tracked-overlays) (delete-overlay ov))
  (setq bft--tracked-overlays nil)
  (bft--post-command-hook))

;;;; Core Logic ----------------------------------------------------------------

(defun bft--post-command-hook ()
  "On frame entry, discover frame or display existing PDF."
  (let ((ov-at-point (bft--get-overlay-at-point)))
    (if ov-at-point
        (unless (eq ov-at-point bft--current-overlay)
          (setq bft--current-overlay ov-at-point)
          (message "BFT: Entered frame at %d." (overlay-start ov-at-point))
          (bft--display-frame-pdf ov-at-point))
      (setq bft--current-overlay nil)
      (when (string= (LaTeX-current-environment) bft-environment-name)
        (let* ((beg (save-excursion (when (LaTeX-find-matching-begin) (point))))
               (end (save-excursion (when (LaTeX-find-matching-end) (point)))))
          (when (and (integer-or-marker-p beg) (integer-or-marker-p end))
            (bft--track-new-frame (cons beg end))))))))

(defun bft--track-new-frame (bounds)
  "Create and track a new overlay for a frame at BOUNDS."
  (let* ((beg (car bounds)) (end (cdr bounds))
         (existing (cl-find-if (lambda (ov) (and (= (overlay-start ov) beg) (= (overlay-end ov) end)))
                               bft--tracked-overlays)))
    (unless existing
      (let ((ov (make-overlay beg end)))
        (overlay-put ov :bft-dirty-p t)
        (overlay-put ov 'after-change-function #'bft--after-change)
        (push ov bft--tracked-overlays)
        (setq bft--current-overlay ov)
        (message "BFT: Now tracking new frame at %d." beg)
        (bft--schedule-compile ov)))))

(defun bft--get-overlay-at-point ()
  "Return our overlay at point, if any."
  (car (cl-remove-if-not (lambda (ov) (overlay-get ov 'after-change-function))
                         (overlays-at (point)))))

(defun bft--get-frame-basename (overlay)
  "Return the unique, stable basename for an overlay."
  (format "%s_pos%d" bft--file-prefix (overlay-start overlay)))

;;;; Modification and Compilation ---------------------------------------------

(defun bft--after-change (overlay _beg _end _len)
  "Hook run when an overlay's text changes."
  (unless (overlay-get overlay :bft-dirty-p)
    (overlay-put overlay :bft-dirty-p t)
    (message "BFT: Frame at %d marked dirty." (overlay-start overlay)))
  (bft--schedule-compile overlay))

(defun bft--schedule-compile (overlay)
  "Schedule or reschedule compilation for a given OVERLAY."
  (when-let ((timer (overlay-get overlay :bft-timer)))
    (cancel-timer timer))
  (let ((timer (run-with-idle-timer
                bft-idle-delay nil #'bft--compile-if-dirty overlay)))
    (overlay-put overlay :bft-timer timer)))

(defun bft--compile-if-dirty (overlay)
  "Compile OVERLAY in the background if it is marked as dirty."
  (when (and (overlayp overlay) (overlay-get overlay :bft-dirty-p))
    (message "BFT: Compiling frame at %d..." (overlay-start overlay))
    ;; Mark as clean immediately. If compile fails, the next edit will
    ;; re-mark as dirty. This is a simple and robust strategy.
    (overlay-put overlay :bft-dirty-p nil)
    (bft--compile-frame-async overlay)))

(defun bft--compile-frame-async (overlay)
  "Compile OVERLAY asynchronously in the background."
  (let ((TeX-save-query nil)
        (TeX-show-compilation-output nil)
        ;; Forcefully disable AUCTeX's automatic command chaining.
        (TeX-command-next nil)
        (TeX-region (bft--get-frame-basename overlay)))
    (save-excursion
      (TeX-pin-region (overlay-start overlay) (overlay-end overlay))
      (TeX-region-update)
      (TeX-command (TeX-command-default (TeX-region-file))
                   #'TeX-region-file))))

;;;; Viewing -------------------------------------------------------------------

(defun bft--display-frame-pdf (overlay)
  "Display the PDF for OVERLAY non-interactively, if it exists."
  (with-current-buffer (overlay-buffer overlay)
    (let ((TeX-region (bft--get-frame-basename overlay)))
      (let ((output-file (funcall #'TeX-region-file (TeX-output-extension))))
        (when (file-exists-p output-file)
          (message "BFT: Displaying PDF for frame at %d." (overlay-start overlay))
          ;; Use the non-interactive backend for viewing.
          (TeX-command "View" #'TeX-region-file 0))))))

(provide 'beamer-frame-tracker)
;;; beamer-frame-tracker.el ends here
