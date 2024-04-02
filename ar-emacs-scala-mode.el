;; ar-emacs-scala-mode.el --- simple electric operator  -*- lexical-binding: t; -*-

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
(require 'ar-beg-end)
(require 'ar-thingatpt-basic-definitions)
(require 'ar-thingatpt-utils-core)
(require 'ar-thingatpt-utils)
(require 'ar-sexp)
(require 'ar-navigate)
(require 'ar-navigate-backward-forms)
(require 'ar-navigate-forward-forms)
(require 'ar-emacs-scala-navigate)

;; Constants
(defconst ar-scala-block-closing-keywords-re
  "[ \t]*\\_<\\(return\\|raise\\|break\\|continue\\|pass\\)\\_>[ \n\t]"
  "Matches the beginning of a class, method or compound statement.")

(setq ar-scala-block-closing-keywords-re
  "[ \t]*\\_<\\(return\\|raise\\|break\\|continue\\|pass\\)\\_>[ \n\t]")

(defconst ar-scala-finally-re
  "[ \t]*\\_<finally:"
  "Regular expression matching keyword which closes a try-block.")

(defconst ar-scala-except-re "[ \t]*\\_<except\\_>"
  "Matches the beginning of a ‘except’ block.")

;; (defconst ar-scala-except-re
;;   "[ \t]*\\_<except\\_>[:( \n\t]*"
;;   "Regular expression matching keyword which composes a try-block.")

(defconst ar-scala-return-re
  ".*:?[ \t]*\\_<\\(return\\)\\_>[ \n\t]*"
  "Regular expression matching keyword which typically closes a function.")

(defconst ar-scala-decorator-re
  "[ \t]*@[^ ]+\\_>[ \n\t]*"
  "Regular expression matching keyword which typically closes a function.")

(defcustom ar-emacs-scala-indent-offset 2
  "Amount of offset per level of indentation."
  :type 'integer
  :tag "ar-emacs-scala-indent-offset")
(make-variable-buffer-local 'ar-emacs-scala-indent-offset)

(defcustom ar-scala-outdent-re-raw
  (list
   "case"
   "else"
   "except"
   "finally"
   )
  "Used by ‘ar-scala-outdent-re’."
  :type '(repeat string)
  :tag "ar-scala-outdent-re-raw"
  :group 'ar-scala-mode
  )

(defconst ar-scala-outdent-re
  (concat
   "[ \t]*"
   (regexp-opt ar-scala-outdent-re-raw 'symbols)
   "[)\t]*")
  "Regular expression matching statements to be dedented one level.")

(defcustom ar-scala-mark-decorators nil
  "If decorators should be marked too.

Default is nil.

Also used by navigation"
  :type 'boolean
  :tag "ar-scala-mark-decorators"
  :group 'scala-mode)

(defcustom ar-scala-no-outdent-re-raw
  (list
   "break"
   "continue"
   "import"
   "pass"
   "raise"
   "return")
  "Uused by ‘ar-scala-no-outdent-re’."
  :type '(repeat string)
  :tag "ar-scala-no-outdent-re-raw"
  :group 'ar-scala-mode)

(defconst ar-scala-no-outdent-re
  (concat
   "[ \t]*"
   (regexp-opt ar-scala-no-outdent-re-raw 'symbols)
   "[)\t]*$")
"Regular expression matching lines not to augment indent after.

See ‘ar-scala-no-outdent-re-raw’ for better readable content")

(defconst ar-scala-assignment-re "\\(\\_<\\w+\\_>[[:alnum:]:, \t]*[ \t]*\\)\\(=\\|+=\\|*=\\|%=\\|&=\\|^=\\|<<=\\|-=\\|/=\\|**=\\||=\\|>>=\\|//=\\)\\(.*\\)"
  "If looking at the beginning of an assignment.")

;; 'name':
(defconst ar-scala-map-re "'\\_<\\w+\\_>':")

(defcustom ar-scala-block-re-raw
  (list
   "class"
   "def"
   "for"
   "if"
   "match"
   "trait"
   "try"
   "while"
   "with"
   )
  "Matches the beginning of a compound statement but not it's clause."
  :type '(repeat string)
  :tag "ar-scala-block-re-raw"
  :group 'ar-scala-mode)


(defconst ar-scala-block-re (concat
		       ;; "[ \t]*"
		       (regexp-opt ar-scala-block-re-raw 'symbols)
		       "[:( \n\t]"
		       )
  "Matches the beginning of a compound statement.")

(defconst ar-block-re ar-scala-block-re
  "Populate ar-navigate.el.")

(defconst ar-scala-minor-block-re-raw (list
                                      "case"
				      "except"
				      "for"
				      "if"
                                      "match"
				      "try"
				      "with"
				      )
  "Matches the beginning of an case ‘for’, ‘if’, ‘try’, ‘except’ or ‘with’ block.")

(defconst ar-scala-minor-block-re
  (concat
   "[ \t]*"
   (regexp-opt ar-scala-minor-block-re-raw 'symbols)
   "[:( \n\t]")

  "Regular expression matching lines not to augment indent after.

See ‘ar-scala-minor-block-re-raw’ for better readable content")

(defconst ar-minor-block-re ar-scala-minor-block-re
  "Populate ar-navigate.el.")

(defconst ar-scala-try-re "[ \t]*\\_<try\\_>[: \n\t]"
  "Matches the beginning of a ‘try’ block.")

(defconst ar-try-re ar-scala-try-re
  "Populate ar-navigate.el.")

(defconst ar-scala-case-re "[ \t]*\\_<case\\_>[: \t][^:]*:"
  "Matches a ‘case’ clause.")

(defconst ar-case-re ar-scala-case-re
  "Populate ar-navigate.el.")

(defconst ar-scala-match-case-re "[ \t]*\\_<match\\_>[: \t][^:]*:"
  "Matches a ‘case’ clause.")

(defconst ar-match-case-re ar-scala-match-case-re
  "Populate ar-navigate.el.")

(defconst ar-scala-for-re "[ \t]*\\_<\\(for\\)\\_> +[[:alpha:]_][[:alnum:]_]* +in +[[:alpha:]_][[:alnum:]_()]* *[: \n\t]"
  "Matches the beginning of a ‘try’ block.")

(defconst ar-for-re ar-scala-for-re
  "Populate ar-navigate.el.")

(defconst ar-scala-if-re "[ \t]*\\_<if\\_> +[^\n\r\f]+ *[: \n\t]"
  "Matches the beginning of an ‘if’ block.")

(defconst ar-if-re ar-scala-if-re
  "Populate ar-navigate.el.")

(defconst ar-scala-else-re "[ \t]*\\_<else:[ \n\t]"
  "Matches the beginning of an ‘else’ block.")

(defconst ar-else-re ar-scala-else-re
  "Populate ar-navigate.el.")

(defconst ar-scala-class-re "[ \t]*\\_<\\(class\\|object\\|trait\\)\\_>[ \n\t]"
  "Matches the beginning of a class definition.")

(defconst ar-class-re ar-scala-class-re
  "Populate ar-navigate.el.")

(defconst ar-scala-def-or-class-re "[ \t]*\\_<\\(case class\\|class\\|def\\|@tailrec def\\|object\\|trait\\)\\_>[ \n\t]+\\([[:alnum:]_]*\\)"
  "Matches the beginning of a class- or functions definition.

Second group grabs the name")

(defvar ar-def-or-class-re ar-scala-def-or-class-re
  "Populate ar-navigate.el.")

(defconst ar-scala-def-re "[ \t]*\\_<\\(case class\\|def\\|@tailrec def\\)\\_>[ \n\t]"
  "Matches the beginning of a functions definition.")

(defconst ar-def-re ar-scala-def-re
 "Populate ar-navigate.el.")

(defcustom ar-scala-block-or-clause-re-raw
  (list
   "class"
   "def"
   "else"
   "except"
   "finally"
   "for"
   "if"
   "trait"
   "try"
   "while"
   "with"
   "match"
   "case"
   )
  "Matches the beginning of a compound statement or it's clause."
  :type '(repeat string)
  :tag "ar-scala-block-or-clause-re-raw"
  :group 'ar-scala-mode)

(defvar ar-scala-block-or-clause-re
  (concat
   "[ \t]*"
   (regexp-opt  ar-scala-block-or-clause-re-raw 'symbols)
   "[( \t]*.*:?")
  "See ‘ar-scala-block-or-clause-re-raw’, which it reads.")

(defconst ar-block-or-clause-re ar-scala-block-or-clause-re
  "Populate ar-navigate.el.")

(defcustom ar-scala-extended-block-or-clause-re-raw
  (list
      "abstract"
      "case"
      "catch"
      "class"
      "def"
      "do"
      "else"
      "enum"
      "export"
      "extends"
      "final"
      "finally"
      "for"
      "forSome"
      "given"
      "if"
      "implicit"
      "import"
      "lazy"
      "match"
      "new"
      "object"
      "override"
      "package"
      "private"
      "protected"
      "return"
      "sealed"
      "then"
      "throw"
      "trait"
      "try"
      "type"
      "val"
      "var"
      "while"
      "with"
      "yield"

)
    "Matches the beginning of a compound statement or it's clause."
    :type '(repeat string)
    :tag "ar-scala-extended-block-or-clause-re-raw"
    :group 'ar-scala-mode)

(defconst ar-scala-extended-block-or-clause-re
  (concat
   "[ \t]*"
   (regexp-opt  ar-scala-extended-block-or-clause-re-raw 'symbols)
   "[( \t:]+")
  "See ‘ar-scala-block-or-clause-re-raw’, which it reads.")

(defconst ar-extended-block-or-clause-re ar-scala-extended-block-or-clause-re
  "Populate ar-navigate.el.")

(defvar beginning-of-defun-command #'ar-backward-def-or-class)

(defun ar-navigate-update-vars (mode)
  (pcase mode
    (`python-mode
     (setq-local
      ar-def-re py-def-re
      ar-class-re py-class-re
      ar-def-or-class-re py-def-or-class-re))
    (`scala-mode
     (setq-local
      ar-def-re ar-scala-def-re
      ar-block-re ar-scala-block-re
      ar-minor-block-re ar-scala-minor-block-re
      ar-block-or-clause-re ar-scala-block-or-clause-re
      ar-def-re ar-scala-def-re
      ar-class-re ar-scala-class-re
      ar-def-or-class-re ar-scala-def-or-class-re
      ar-indent-offset ar-emacs-scala-indent-offset
      ))))

(require 'ar-navigate)
(require 'ar-emacs-scala-navigate)
(require 'ar-navigate-backward-forms)
(require 'ar-navigate-forward-forms)

(provide 'ar-emacs-scala-mode)
;;; ar-emacs-scala-mode.el ends here
