;;; ar-emacs-scala-mode-setup-tests.el --- Provide needed forms -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2024  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>

;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; (require 'ar-subr)
;; (require 'ar-emacs-scala-mode)
;; (require 'ar-emacs-scala-navigate)
;; (require 'ar-navigate)
;; (require 'ar-navigate-forward-forms)
;; (require 'ar-navigate-backward-forms)

(defun ar-toggle-debug-p ()
  "Toggle `ar-debug-p'. "
  (interactive)
  (setq ar-debug-p (not ar-debug-p))
  (message "ar-debug-p: %s"  ar-debug-p))

(defmacro ar-test (contents mode verbose &rest body)
  "Create temp buffer inserting CONTENTS.

BODY is code to be executed within the temp buffer "
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (funcall ,mode)
       (when ,verbose
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min) (point-max)))
       ,@body)))

(defmacro ar-test-point-min (contents mode verbose &rest body)
  "Create temp buffer in `scala-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (funcall ,mode)
       (insert ,contents)
       (goto-char (point-min))
       (when ,verbose
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min) (point-max)))
       ,@body)))

(defmacro ar-test-with-scala-buffer-point-min (contents &rest body)
  "Create temp buffer in `scala-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode py--imenu-create-index-p)
       (insert ,contents)
       (scala-mode)
       (goto-char (point-min))
       (when ar-debug-p (switch-to-buffer (current-buffer))
	     (font-lock-fontify-region (point-min) (point-max)))
       ,@body)
     ))

(defmacro ar-test-with-scala-buffer (contents &rest body)
  "Create temp buffer in `scala-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode py--imenu-create-index-p)
       (insert ,contents)
       (scala-mode)
       (when ar-debug-p (switch-to-buffer (current-buffer))
	     (font-lock-fontify-region (point-min) (point-max)))
       ;; (message "ERT %s" (point))
       ,@body)
     ))

(defmacro ar-test-with-shell-script-buffer (contents &rest body)
  "Create temp buffer in `emacs-lisp-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (shell-script-mode)
       (insert ,contents)
       (when ar-debug-p
	 (switch-to-buffer (current-buffer)))
       (font-lock-fontify-region (point-min) (point-max))
       ,@body)))

(defmacro ar-test-with-shell-script-buffer-point-min (contents &rest body)
  "Create temp buffer inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (shell-script-mode)
       (goto-char (point-min))
       (when ar-debug-p
	 (switch-to-buffer (current-buffer)))
       (font-lock-fontify-region (point-min) (point-max))
       ,@body)))

(provide 'ar-emacs-scala-mode-setup-tests)
;; ar-emacs-scala-mode-setup-tests.el ends here
