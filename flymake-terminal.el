;;; flymake-margin.el --- Sets flymake to work with margin instead of fringes -*- lexical-binding: t; -*-

;; Author: Rahul M. Juliato
;; Created: March 25, 2024
;; Version: 0.1.0
;; Keywords: flymake, margin, tui
;; URL: https://github.com/LionyxML/flymake-margin
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; `flymake-margin` is a minor mode that aims to get flymake out of
;; the fringe and into the margin.
;;
;; To enable it, install the package and add it to your load path:
;;
;;     (require 'flymake-margin)
;;     (flymake-margin-mode t)
;;
;; There are several customizable options you can find:
;;
;;     M-x customize-group flymake-margin
;;

;;; Code:
(require 'flymake)

(defgroup flymake-margin nil
  "Customize Flymake on Margin."
  :group 'flymake
  :prefix "flymake-margin-")

(defcustom flymake-margin-error-symbol "E"
  "Symbol to represent terminal errors in Flymake."
  :type 'string
  :group 'flymake-margin)

(defcustom flymake-margin-warning-symbol "W"
  "Symbol to represent terminal warnings in Flymake."
  :type 'string
  :group 'flymake-margin)

(defcustom flymake-margin-note-symbol "N"
  "Symbol to represent terminal notes in Flymake."
  :type 'string
  :group 'flymake-margin)

(defcustom flymake-margin-side 'left
  "Side of the buffer to display Flymake margin symbols.
Possible values are 'left' and 'right'."
  :type '(choice (const :tag "Left" left) (const :tag "Right" right))
  :group 'flymake-margin
  :set (lambda (symbol value)
         (set symbol value)
         (when (eq value 'left)
           (setq-default left-margin-width 1)
           (modify-all-frames-parameters '((left-fringe . left-margin))))
         (when (eq value 'right)
           (setq-default right-margin-width 1)
           (modify-all-frames-parameters '((left-fringe . right-margin))))))

(defun flymake-margin-fringe-overlay-spec-advice (bitmap &optional recursed)
"Advice function to customize Flymake fringe overlay."
(if (and (symbolp bitmap)
		 (boundp bitmap)
		 (not recursed))
	(flymake--fringe-overlay-spec
	 (symbol-value bitmap) t)
  (let ((margin-side (if (eq flymake-margin-side 'left) 'left-margin 'right-margin)))
	(and flymake-fringe-indicator-position
		 bitmap
		 (propertize "!" 'display
					 `((margin ,margin-side)
					   ,bitmap))))))

(defun flymake-margin-setup-symbols ()
  "Setup Flymake margin symbols."
  (put 'flymake-error 'flymake-bitmap
       (propertize (symbol-value 'flymake-margin-error-symbol)
                   'face `(:inherit (error default) :underline nil)))
  (put 'flymake-warning 'flymake-bitmap
       (propertize (symbol-value 'flymake-margin-warning-symbol)
                   'face `(:inherit (warning default) :underline nil)))
  (put 'flymake-note 'flymake-bitmap
       (propertize (symbol-value 'flymake-margin-note-symbol)
                   'face `(:inherit (success default) :underline nil))))

(defcustom flymake-margin-get-margin-symbol-default " "
  "Blank symbol to use when `flymake-get-margin-symbol returns nothing."
  :type 'string
  :group 'flymake-margin)

(defun flymake-get-margin-symbol (&optional beg end)
  "Return margin symbol based on the severity of Flymake diagnostics for the specified line range.
If BEG or END is not provided, default to the beginning or end position of the current line.
Ideally this would be used by some sort of git-gutter package (hl-diff maybe) and other
extensions modifying the margin in order to copy the contents."
  (interactive)
  (setq beg (or beg (line-beginning-position)))
  (setq end (or end (line-end-position)))
  (let ((diagnostics (flymake-diagnostics beg end)))
    (when diagnostics
      (let* ((severity (flymake--diag-type (car diagnostics))))
        (pcase severity
          (':error (symbol-value 'flymake-margin-error-symbol))
		  ('eglot-error (symbol-value 'flymake-margin-error-symbol))
          (':warning (symbol-value 'flymake-margin-warning-symbol))
		  ('eglot-warning (symbol-value 'flymake-margin-warning-symbol))
          (':note (symbol-value 'flymake-margin-note-symbol))
		  ('eglot-note (symbol-value 'flymake-margin-note-symbol))
          (_ (symbol-value 'flymake-margin-get-margin-symbol-default)))))))

;;;###autoload
(define-minor-mode flymake-margin-mode
  "Toggle Flymake margin mode on or off."
  :group 'flymake-margin
  :global t
  :lighter " FM"
  (if flymake-margin-mode
      (progn
        (flymake-margin-setup-symbols)
        (advice-add #'flymake--fringe-overlay-spec :override #'flymake-margin-fringe-overlay-spec-advice))
    (progn
      ;; TODO: Unload symbols loaded by `flymake-margin-setup-symbols`
      (advice-remove #'flymake--fringe-overlay-spec #'flymake-margin-fringe-overlay-spec-advice))))

(provide 'flymake-margin)

;;; flymake-margin.el ends here
