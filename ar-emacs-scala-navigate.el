;; ar-emacs-scala-navigate.el --- simple electric operator  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>
;; Keywords: convenience

;; Version: 0.0.1
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary: This is a still naive prototype

;;

;;; Code:

;; avoid: Warning reference to free variable ‘comint-last-prompt’
(require 'comint)
(require 'ar-subr)
(require 'ar-navigate)
(require 'ar-navigate-backward-forms)
(require 'ar-navigate-forward-forms)

(defun ar-scala-backward-def()
  (interactive)
  (ar-navigate-update-vars 'scala-mode)
  (ar-backward-def))

(defun ar-scala-backward-class()
  (interactive)
  (ar-navigate-update-vars 'scala-mode)
  (ar-backward-class))

(defun ar-scala-backward-def-or-class()
  (interactive)
  (ar-navigate-update-vars 'scala-mode)
  (ar-backward-def-or-class))

(defun ar-scala-forward-def()
  (interactive)
  (ar-navigate-update-vars 'scala-mode)
  (ar-forward-def))

(defun ar-scala-forward-class()
  (interactive)
  (ar-navigate-update-vars 'scala-mode)
  (ar-forward-class))

(defun ar-scala-forward-def-or-class()
  (interactive)
  (ar-navigate-update-vars 'scala-mode)
  (ar-forward-def-or-class))

(provide 'ar-emacs-scala-navigate)
;;; ar-emacs-scala-navigate.el ends here
