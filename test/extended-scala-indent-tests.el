;;; extended-scala-indent-tests.el --- Test scala-mode navigation  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2025  Andreas Röhler

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

(require 'ar-setup-ert-tests)

(ert-deftest extended-scala-indent-test-NE01iv ()
  (ar-test
"def removeKIntern[A](k: Int, xs: Seq[A], res: Seq[A] = Nil): Seq[A] = {
  if (xs.isEmpty) res.reverse
  else
    if (res.isEmpty) removeKIntern(k, xs.tail, xs.head +: res)
    else
      if (res.filter(_ == xs.head).length < k) removeKIntern(k, xs.tail, xs.head +: res)
      else
        removeKIntern(k, xs.tail, res)
}"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (search-backward "else" nil t 3)
    (should (eq 2 (ar-compute-indentation)))))

(ert-deftest extended-scala-indent-test-f905o5 ()
  (ar-test
"def removeKIntern[A](k: Int, xs: Seq[A], res: Seq[A] = Nil): Seq[A] = {
  if (xs.isEmpty) res.reverse
  else
    if (res.isEmpty) removeKIntern(k, xs.tail, xs.head +: res)
    else
      if (res.filter(_ == xs.head).length < k) removeKIntern(k, xs.tail, xs.head +: res)
      else
        removeKIntern(k, xs.tail, res)
}"
    'scala-mode
    (extended-scala-mode) 
    'ar-verbose-p
    (goto-char (point-max))
    (search-backward "if" nil t 2)
    (should (eq 4 (ar-compute-indentation)))))



(provide 'extended-scala-indent-tests)
;;; extended-scala-indent-tests.el ends here
