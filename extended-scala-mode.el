;; extended-scala-mode.el --- simple electric operator  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Andreas Röhler

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
(require 'scala-mode)

(add-to-list 'load-path "..")
(require 'ar-scala-vars)
(require 'ar-mode)

(defcustom extended-scala-indent-offset 2
  "Amount of offset per level of indentation."
  :type 'integer
  :tag "extended-scala-indent-offset")
(make-variable-buffer-local 'extended-scala-indent-offset)

;; (setq extended-scala-indent-offset 2)

;; (defcustom ar-scala-outdent-re-raw
;;   (list
;;    "case"
;;    "else"
;;    "except"
;;    "finally"
;;    )
;;   "Used by ‘ar-scala-outdent-re’."
;;   :type '(repeat string)
;;   :tag "ar-scala-outdent-re-raw"
;;   :group 'ar-scala-mode
;;   )

;; (defconst ar-scala-outdent-re
;;   (concat
;;    "[ \t]*"
;;    (regexp-opt ar-scala-outdent-re-raw 'symbols)
;;    "[)\t]*")
;;   "Regular expression matching statements to be dedented one level.")

;; (defcustom ar-scala-mark-decorators nil
;;   "If decorators should be marked too.

;; Default is nil.

;; Also used by navigation"
;;   :type 'boolean
;;   :tag "ar-scala-mark-decorators"
;;   :group 'scala-mode)

;; (defcustom ar-scala-no-outdent-re-raw
;;   (list
;;    "break"
;;    "continue"
;;    "import"
;;    "pass"
;;    "raise"
;;    "return")
;;   "Uused by ‘ar-scala-no-outdent-re’."
;;   :type '(repeat string)
;;   :tag "ar-scala-no-outdent-re-raw"
;;   :group 'ar-scala-mode)

;; (defconst ar-scala-no-outdent-re
;;   (concat
;;    "[ \t]*"
;;    (regexp-opt ar-scala-no-outdent-re-raw 'symbols)
;;    "[)\t]*$")
;; "Regular expression matching lines not to augment indent after.

;; See ‘ar-scala-no-outdent-re-raw’ for better readable content")

;; (defconst ar-scala-assignment-re "\\(\\_<\\w+\\_>[[:alnum:]:, \t]*[ \t]*\\)\\(=\\|+=\\|*=\\|%=\\|&=\\|^=\\|<<=\\|-=\\|/=\\|**=\\||=\\|>>=\\|//=\\)\\(.*\\)"
;;   "If looking at the beginning of an assignment.")

;; ;; 'name':
;; (defconst ar-scala-map-re "'\\_<\\w+\\_>':")

;; (defcustom ar-scala-block-re-raw
;;   (list
;;    "class"
;;    "def"
;;    "for"
;;    "if"
;;    "match"
;;    "trait"
;;    "try"
;;    "while"
;;    "with"
;;    )
;;   "Matches the beginning of a compound statement but not it's clause."
;;   :type '(repeat string)
;;   :tag "ar-scala-block-re-raw"
;;   :group 'ar-scala-mode)

;; (defconst ar-scala-block-re (concat
;; 		       ;; "[ \t]*"
;; 		       (regexp-opt ar-scala-block-re-raw 'symbols)
;; 		       "[:( \n\t]"
;; 		       )
;;   "Matches the beginning of a compound statement.")

;; (defconst ar-block-re ar-scala-block-re
;;   "Populate ar-navigate.el.")

;; (defconst ar-scala-minor-block-re-raw (list
;;                                       "case"
;; 				      "except"
;; 				      "for"
;; 				      "if"
;;                                       "match"
;; 				      "try"
;; 				      "with"
;; 				      )
;;   "Matches the beginning of an case ‘for’, ‘if’, ‘try’, ‘except’ or ‘with’ block.")

;; (defconst ar-scala-minor-block-re
;;   (concat
;;    "[ \t]*"
;;    (regexp-opt ar-scala-minor-block-re-raw 'symbols)
;;    "[:( \n\t]")

;;   "Regular expression matching lines not to augment indent after.

;; See ‘ar-scala-minor-block-re-raw’ for better readable content")

;; (defconst ar-minor-block-re ar-scala-minor-block-re
;;   "Populate ar-navigate.el.")

;; (defconst ar-scala-try-re "[ \t]*\\_<try\\_>[: \n\t]"
;;   "Matches the beginning of a ‘try’ block.")

;; (defconst ar-try-re ar-scala-try-re
;;   "Populate ar-navigate.el.")

;; (defconst ar-scala-case-re "[ \t]*\\_<case\\_>[: \t][^:]*:"
;;   "Matches a ‘case’ clause.")

;; (defconst ar-case-re ar-scala-case-re
;;   "Populate ar-navigate.el.")

;; (defconst ar-scala-match-case-re "[ \t]*\\_<match\\_>[: \t][^:]*:"
;;   "Matches a ‘case’ clause.")

;; (defconst ar-match-case-re ar-scala-match-case-re
;;   "Populate ar-navigate.el.")

;; (defconst ar-scala-for-re "[ \t]*\\_<\\(for\\)\\_> +[[:alpha:]_][[:alnum:]_]* +in +[[:alpha:]_][[:alnum:]_()]* *[: \n\t]"
;;   "Matches the beginning of a ‘try’ block.")

;; (defconst ar-for-re ar-scala-for-re
;;   "Populate ar-navigate.el.")

;; (defconst ar-scala-if-re "[ \t]*\\_<if\\_> +[^\n\r\f]+ *[: \n\t]"
;;   "Matches the beginning of an ‘if’ block.")

;; (defconst ar-if-re ar-scala-if-re
;;   "Populate ar-navigate.el.")

;; (defconst ar-scala-else-re "[ \t]*\\_<else:[ \n\t]"
;;   "Matches the beginning of an ‘else’ block.")

;; (defconst ar-else-re ar-scala-else-re
;;   "Populate ar-navigate.el.")

;; (defconst ar-scala-class-re "[ \t]*\\_<\\(class\\|object\\|trait\\)\\_>[ \n\t]"
;;   "Matches the beginning of a class definition.")

;; (defconst ar-class-re ar-scala-class-re
;;   "Populate ar-navigate.el.")

;; (defconst ar-scala-def-or-class-re "[ \t]*\\_<\\(case class\\|class\\|def\\|@tailrec def\\|object\\|trait\\)\\_>[ \n\t]+\\([[:alnum:]_]*\\)"
;;   "Matches the beginning of a class- or functions definition.

;; Second group grabs the name")

;; ;; (defvar ar-def-or-class-re ar-scala-def-or-class-re
;; ;;   "Populate ar-navigate.el.")

;; (defconst ar-scala-def-re "[ \t]*\\_<\\(case class\\|def\\|@tailrec def\\)\\_>[ \n\t]"
;;    "Matches the beginning of a functions definition.")

;; ;; (defconst ar-def-re ar-scala-def-re
;; ;;  "Populate ar-navigate.el.")

;; (defcustom ar-scala-block-or-clause-re-raw
;;   (list
;;    "class"
;;    "def"
;;    "else"
;;    "except"
;;    "finally"
;;    "for"
;;    "if"
;;    "trait"
;;    "try"
;;    "while"
;;    "with"
;;    "match"
;;    "case"
;;    )
;;   "Matches the beginning of a compound statement or it's clause."
;;   :type '(repeat string)
;;   :tag "ar-scala-block-or-clause-re-raw"
;;   :group 'ar-scala-mode)

;; (defvar ar-scala-block-or-clause-re
;;   (concat
;;    "[ \t]*"
;;    (regexp-opt  ar-scala-block-or-clause-re-raw 'symbols)
;;    "[( \t]*.*:?")
;;   "See ‘ar-scala-block-or-clause-re-raw’, which it reads.")

;; (defconst ar-block-or-clause-re ar-scala-block-or-clause-re
;;   "Populate ar-navigate.el.")

;; (defcustom ar-scala-extended-block-or-clause-re-raw
;;   (list
;;       "abstract"
;;       "case"
;;       "catch"
;;       "class"
;;       "def"
;;       "do"
;;       "else"
;;       "enum"
;;       "export"
;;       "extends"
;;       "final"
;;       "finally"
;;       "for"
;;       "forSome"
;;       "given"
;;       "if"
;;       "implicit"
;;       "import"
;;       "lazy"
;;       "match"
;;       "new"
;;       "object"
;;       "override"
;;       "package"
;;       "private"
;;       "protected"
;;       "return"
;;       "sealed"
;;       "then"
;;       "throw"
;;       "trait"
;;       "try"
;;       "type"
;;       "val"
;;       "var"
;;       "while"
;;       "with"
;;       "yield"

;; )
;;     "Matches the beginning of a compound statement or it's clause."
;;     :type '(repeat string)
;;     :tag "ar-scala-extended-block-or-clause-re-raw"
;;     :group 'ar-scala-mode)

;; (defconst ar-scala-extended-block-or-clause-re
;;   (concat
;;    "[ \t]*"
;;    (regexp-opt  ar-scala-extended-block-or-clause-re-raw 'symbols)
;;    "[( \t:]+")
;;   "See ‘ar-scala-block-or-clause-re-raw’, which it reads.")

;; (defconst ar-extended-block-or-clause-re ar-scala-extended-block-or-clause-re
;;   "Populate ar-navigate.el.")

;; (defvar beginning-of-defun-command #'ar-backward-def-or-class)

;; (ar-update-vars 'scala-mode)


(add-to-list 'auto-mode-alist
             '("\\.\\(scala\\|sbt\\|worksheet\\.sc\\)\\'" . scala-mode))

;; (defun all-mode-setting ()
;;   (set (make-local-variable 'indent-tabs-mode) ar-indent-tabs-mode)
;;   )

(setq
 comment-start "// "
 comment-start-skip "\\(//+\\|/\\*+\\)[ \t]*"
 ar-comment-start-re scala-syntax:comment-start-re
 ar-comment-start-skip-re "/[/*] +"
 ar--docbeg ar--docbeg
 ar--edit-register ar--edit-register
 ar--editbeg ar--editbeg
 ar--editend ar--editend
 ar--execute-use-temp-file-p ar--execute-use-temp-file-p
 ar--imenu-create-index-function ar--imenu-create-index-function
 ar--imenu-create-index-p ar--imenu-create-index-p
 ar--match-paren-forward-p ar--match-paren-forward-p
 ar--oldbuf ar--oldbuf
 ar--warn-tmp-files-left-p ar--warn-tmp-files-left-p
 ar--windows-config-register ar--windows-config-register
 ar-ask-about-save ar-ask-about-save
 ar-auto-complete-p ar-auto-complete-p
 ar-auto-completion-buffer ar-auto-completion-buffer
 ar-auto-completion-mode-p ar-auto-completion-mode-p
 ar-auto-fill-mode ar-auto-fill-mode
 ar-autofill-timer ar-autofill-timer
 ar-beep-if-tab-change ar-beep-if-tab-change
 ar-block-closing-keywords-re ar-block-closing-keywords-re
 ar-block-comment-prefix ar-block-comment-prefix
 ar-block-keywords scala-indent:control-keywords-cond-re
 ar-block-or-clause-re scala-indent:control-keywords-other-re
 ar-block-re ar-block-re scala-indent:control-keywords-re
 ar-buffer-name ar-buffer-name
 ar-builtins-face ar-builtins-face
 ar-chars-after ar-chars-after
 ar-chars-before ar-chars-before
 ar-check-command ar-check-command
 ar-class-name-face ar-class-name-face
 ar-class-re "\\<\\(class\\|object\\|trait\\)\\>"
 ar-clause-re scala-indent:control-keywords-other-re
 ar-close-provides-newline ar-close-provides-newline
 ar-closing-list-keeps-space ar-closing-list-keeps-space
 ar-closing-list-space ar-closing-list-space
 ar-coding-re ar-coding-re
 ar-comment-fill-column ar-comment-fill-column
 ar-company-pycomplete-p ar-company-pycomplete-p
 ar-compilation-regexp-alist ar-compilation-regexp-alist
 ar-complete-ac-sources ar-complete-ac-sources
 ar-complete-function ar-complete-function
 ar-complete-last-modified ar-complete-last-modified
 ar-continuation-offset ar-continuation-offset
 ar-current-defun-delay ar-current-defun-delay
 ar-current-defun-show ar-current-defun-show
 ar-custom-temp-directory ar-custom-temp-directory
 ar-debug-p ar-debug-p
 ar-decorators-face ar-decorators-face
 ar-dedent-keep-relative-column ar-dedent-keep-relative-column
 ar-dedicated-process-p ar-dedicated-process-p
 ar-def-class-face ar-def-class-face
 ar-def-face ar-def-face
 ar-def-or-class-re "\\<\\(class\\|def\\|object\\|trait\\)\\>"
 ar-def-re "\\<def\\>"
 ar-default-working-directory ar-default-working-directory
 ar-defun-use-top-level-p ar-defun-use-top-level-p
 ar-delete-function ar-delete-function
 ar-do-completion-p ar-do-completion-p
 ar-docstring-fill-column ar-docstring-fill-column
 ar-docstring-style ar-docstring-style
 ar-dotted-expression-syntax-table ar-dotted-expression-syntax-table
 ar-edit-buffer ar-edit-buffer
 ar-edit-only-p ar-edit-only-p
 ar-electric-close-active-p ar-electric-close-active-p
 ar-electric-colon-active-p ar-electric-colon-active-p
 ar-electric-colon-bobl-only ar-electric-colon-bobl-only
 ar-electric-colon-greedy-p ar-electric-colon-greedy-p
 ar-electric-colon-newline-and-indent-p ar-electric-colon-newline-and-indent-p
 ar-electric-comment-add-space-p ar-electric-comment-add-space-p
 ar-electric-comment-p ar-electric-comment-p
 ar-electric-yank-active-p ar-electric-yank-active-p
 ar-elif-re ar-elif-re
 ar-else-re ar-else-re
 ar-empty-comment-line-separates-paragraph-p ar-empty-comment-line-separates-paragraph-p
 ar-empty-line-closes-p ar-empty-line-closes-p
 ar-empty-line-p-chars ar-empty-line-p-chars
 ar-error ar-error
 ar-ert-test-default-executables ar-ert-test-default-executables
 ar-exception-buffer ar-exception-buffer
 ar-exception-name-face ar-exception-name-face
 ar-execute-directory ar-execute-directory
 ar-execute-no-temp-p ar-execute-no-temp-p
 ar-expression-re ar-expression-re
 ar-expression-skip-chars ar-expression-skip-chars
 ar-extended-block-or-clause-re-raw ar-extended-block-or-clause-re-raw
 ar-extensions ar-extensions
 ar-fast-completion-delay ar-fast-completion-delay
 ar-fast-filter-re ar-fast-filter-re
 ar-fast-output-buffer ar-fast-output-buffer
 ar-fast-process-p ar-fast-process-p
 ar-file-queue ar-file-queue
 ar-fileless-buffer-use-default-directory-p ar-fileless-buffer-use-default-directory-p
 ar-fill-column-orig ar-fill-column-orig
 ar-finally-re ar-finally-re
 ar-flake8-command ar-flake8-command
 ar-flake8-history ar-flake8-history
 ar-font-lock-defaults-p ar-font-lock-defaults-p
 ar-fontify-shell-buffer-p ar-fontify-shell-buffer-p
 ar-force-default-output-buffer-p ar-force-default-output-buffer-p
 ar-hide-comments-when-hiding-all ar-hide-comments-when-hiding-all
 ar-hide-show-keywords ar-hide-show-keywords
 ar-hide-show-minor-mode-p ar-hide-show-minor-mode-p
 ar-highlight-error-source-p ar-highlight-error-source-p
 ar-history-filter-regexp ar-history-filter-regexp
 ar-honor-PYTHONHISTORY-p ar-honor-PYTHONHISTORY-p
 ar-if-name-main-permission-p ar-if-name-main-permission-p
 ar-imenu-max-items ar-imenu-max-items
 ar-imenu-show-method-args-p ar-imenu-show-method-args-p
 ar-import-check-point-max ar-import-check-point-max
 ar-import-from-face ar-import-from-face
 ar-indent-comments ar-indent-comments
 ar-indent-honors-inline-comment ar-indent-honors-inline-comment
 ar-indent-list-style ar-indent-list-style
 ar-indent-no-completion-p ar-indent-no-completion-p
 ar-indent-offset extended-scala-indent-offset
 ar-indent-re ar-indent-re
 ar-input-filter-re ar-input-filter-re
 ar-install-directory ar-install-directory
 ar-ipython-command ar-ipython-command
 ar-ipython-completion-command-string ar-ipython-completion-command-string
 ar-ipython-completions ar-ipython-completions
 ar-ipython-execute-delay ar-ipython-execute-delay
 ar-ipython-history ar-ipython-history
 ar-ipython-modeline-display ar-ipython-modeline-display
 ar-ipython-module-completion-code ar-ipython-module-completion-code
 ar-ipython-send-delay ar-ipython-send-delay
 ar-ipython0.10-completion-command-string ar-ipython0.10-completion-command-string
 ar-jump-on-exception ar-jump-on-exception
 ar-jython-command ar-jython-command
 ar-jython-modeline-display ar-jython-modeline-display
 ar-keep-shell-dir-when-execute-p ar-keep-shell-dir-when-execute-p
 ar-keep-windows-configuration ar-keep-windows-configuration
 ar-kill-empty-line ar-kill-empty-line
 ar-known-shells-extended-commands ar-known-shells-extended-commands
 ar-labelled-re ar-labelled-re
 ar-last-exeption-buffer ar-last-exeption-buffer
 ar-last-position ar-last-position
 ar-last-window-configuration ar-last-window-configuration
 ar-lhs-inbound-indent ar-lhs-inbound-indent
 ar-line-number-offset ar-line-number-offset
 ar-line-re ar-line-re
 ar-load-pymacs-p ar-load-pymacs-p
 ar-load-skeletons-p ar-load-skeletons-p
 ar-master-file ar-master-file
 ar-match-paren-key ar-match-paren-key
 ar-match-paren-mode ar-match-paren-mode
 ar-match-paren-no-use-syntax-pps ar-match-paren-no-use-syntax-pps
 ar-max-help-buffer-p ar-max-help-buffer-p
 ar-max-specpdl-size ar-max-specpdl-size
 ar-message-executing-temporary-file ar-message-executing-temporary-file
 ar-minor-block-re ar-minor-block-re
 ar-minor-clause-re-raw ar-minor-clause-re-raw
 ar-mode-output-map ar-mode-output-map
 ar-modeline-acronym-display-home-p ar-modeline-acronym-display-home-p
 ar-modeline-display ar-modeline-display
 ar-new-session-p ar-new-session-p
 ar-new-shell-delay ar-new-shell-delay
 ar-newline-delete-trailing-whitespace-p ar-newline-delete-trailing-whitespace-p
 ar-no-outdent-re-raw ar-no-outdent-re-raw
 ar-not-expression-chars ar-not-expression-chars
 ar-not-expression-regexp ar-not-expression-regexp
 ar-number-face ar-number-face
 ar-object-reference-face ar-object-reference-face
 ar-operator-re ar-operator-re
 ar-org-cycle-p ar-org-cycle-p
 ar-orig-buffer-or-file ar-orig-buffer-or-file
 ar-outdent-re-raw ar-outdent-re-raw
 ar-outline-minor-mode-p ar-outline-minor-mode-p
 ar-outline-mode-keywords ar-outline-mode-keywords
 ar-output-buffer ar-output-buffer
 ar-paragraph-re ar-paragraph-re
 ar-partial-expression-re ar-partial-expression-re
 ar-pdb-executable ar-pdb-executable
 ar-pdb-path ar-pdb-path
 ar-pdbtrack-buffers-to-kill ar-pdbtrack-buffers-to-kill
 ar-pdbtrack-do-tracking-p ar-pdbtrack-do-tracking-p
 ar-pdbtrack-input-prompt ar-pdbtrack-input-prompt
 ar-pdbtrack-is-tracking-p ar-pdbtrack-is-tracking-p
 ar-pdbtrack-marker-regexp-funcname-group ar-pdbtrack-marker-regexp-funcname-group
 ar-pdbtrack-marker-regexp-line-group ar-pdbtrack-marker-regexp-line-group
 ar-pdbtrack-minor-mode-string ar-pdbtrack-minor-mode-string
 ar-pdbtrack-stack-entry-regexp ar-pdbtrack-stack-entry-regexp
 ar-pdbtrack-stacktrace-info-regexp ar-pdbtrack-stacktrace-info-regexp
 ar-pdbtrack-track-range ar-pdbtrack-track-range
 ar-pep8-command ar-pep8-command
 ar-pep8-history ar-pep8-history
 ar-prompt-on-changed-p ar-prompt-on-changed-p
 ar-pseudo-keyword-face ar-pseudo-keyword-face
 ar-pychecker-command-args ar-pychecker-command-args
 ar-pychecker-history ar-pychecker-history
 ar-pyflakes3-command ar-pyflakes3-command
 ar-pyflakes3-history ar-pyflakes3-history
 ar-pyflakespep8-command ar-pyflakespep8-command
 ar-pyflakespep8-history ar-pyflakespep8-history
 ar-pylint-command ar-pylint-command
 ar-pylint-history ar-pylint-history
 ar-python-command-args ar-python-command-args
 ar-python-completions ar-python-completions
 ar-python-edit-version ar-python-edit-version
 ar-python-ffap-setup-code ar-python-ffap-setup-code
 ar-python-history ar-python-history
 ar-python-send-delay ar-python-send-delay
 ar-python2-command ar-python2-command
 ar-python2-modeline-display ar-python2-modeline-display
 ar-python3-command ar-python3-command
 ar-python3-modeline-display ar-python3-modeline-display
 ar-python3-send-delay ar-python3-send-delay
 ar-pythonpath ar-pythonpath
 ar-register-char ar-register-char
 ar-register-shell-buffer-p ar-register-shell-buffer-p
 ar-remove-cwd-from-path ar-remove-cwd-from-path
 ar-result ar-result
 ar-return-key ar-return-key
 ar-return-re ar-return-re
 ar-section-end ar-section-end
 ar-section-re ar-section-re
 ar-separator-char ar-separator-char
 ar-session-p ar-session-p
 ar-set-complete-keymap-p ar-set-complete-keymap-p
 ar-set-pager-cat-p ar-set-pager-cat-p
 ar-sexp-function ar-sexp-function
 ar-sexp-use-expression-p ar-sexp-use-expression-p
 ar-shebang-regexp ar-shebang-regexp
 ar-shebang-startstring ar-shebang-startstring
 ar-shell--first-prompt-received ar-shell--first-prompt-received
 ar-shell--font-lock-buffer ar-shell--font-lock-buffer
 ar-shell--package-depth ar-shell--package-depth
 ar-shell--parent-buffer ar-shell--parent-buffer
 ar-shell--prompt-calculated-input-regexp ar-shell--prompt-calculated-input-regexp
 ar-shell--prompt-calculated-output-regexp ar-shell--prompt-calculated-output-regexp
 ar-shell-compilation-regexp-alist ar-shell-compilation-regexp-alist
 ar-shell-complete-debug ar-shell-complete-debug
 ar-shell-complete-p ar-shell-complete-p
 ar-shell-completion-native-output-timeout ar-shell-completion-native-output-timeout
 ar-shell-completion-native-redirect-buffer ar-shell-completion-native-redirect-buffer
 ar-shell-completion-native-try-output-timeout ar-shell-completion-native-try-output-timeout
 ar-shell-completion-setup-code ar-shell-completion-setup-code
 ar-shell-completion-string-code ar-shell-completion-string-code
 ar-shell-exec-path ar-shell-exec-path
 ar-shell-extra-pythonpaths ar-shell-extra-pythonpaths
 ar-shell-first-prompt-hook ar-shell-first-prompt-hook
 ar-shell-fontify-p ar-shell-fontify-p
 ar-shell-hook ar-shell-hook
 ar-shell-input-prompt-1-regexp ar-shell-input-prompt-1-regexp
 ar-shell-input-prompt-2-regexps ar-shell-input-prompt-2-regexps
 ar-shell-local-path ar-shell-local-path
 ar-shell-mode-syntax-table ar-shell-mode-syntax-table
 ar-shell-name ar-shell-name
 ar-shell-output-filter-buffer ar-shell-output-filter-buffer
 ar-shell-output-filter-in-progress ar-shell-output-filter-in-progress
 ar-shell-output-prompt-regexps ar-shell-output-prompt-regexps
 ar-shell-process-environment ar-shell-process-environment
 ar-shell-prompt-detect-p ar-shell-prompt-detect-p
 ar-shell-prompt-output-regexp ar-shell-prompt-output-regexp
 ar-shell-prompt-pdb-regexp ar-shell-prompt-pdb-regexp
 ar-shell-prompt-read-only ar-shell-prompt-read-only
 ar-shell-prompt-regexp ar-shell-prompt-regexp
 ar-shell-remote-exec-path ar-shell-remote-exec-path
 ar-shell-template ar-shell-template
 ar-shell-toggle-1 ar-shell-toggle-1
 ar-shell-toggle-2 ar-shell-toggle-2
 ar-shell-unbuffered ar-shell-unbuffered
 ar-shell-virtualenv-root ar-shell-virtualenv-root
 ar-smart-indentation ar-smart-indentation
 ar-split-window-on-execute-threshold ar-split-window-on-execute-threshold
 ar-split-windows-on-execute-function ar-split-windows-on-execute-function
 ar-start-in-virtualenv-p ar-start-in-virtualenv-p
 ar-statement-re ar-statement-re
 ar-store-result-p ar-store-result-p
 ar-string-delim-re ar-string-delim-re
 ar-switch-buffers-on-execute-p ar-switch-buffers-on-execute-p
 ar-symbol-re ar-symbol-re
 ar-tab-indent ar-tab-indent
 ar-tab-shifts-region-p ar-tab-shifts-region-p
 ar-temp-directory ar-temp-directory
 ar-this-result ar-this-result
 ar-timer-close-completions-p ar-timer-close-completions-p
 ar-top-level-re ar-top-level-re
 ar-traceback-line-re ar-traceback-line-re
 ar-trailing-whitespace-smart-delete-p ar-trailing-whitespace-smart-delete-p
 ar-try-if-face ar-try-if-face
 ar-uncomment-indents-p ar-uncomment-indents-p
 ar-update-gud-pdb-history-p ar-update-gud-pdb-history-p
 ar-use-current-dir-when-execute-p ar-use-current-dir-when-execute-p
 ar-use-font-lock-doc-face-p ar-use-font-lock-doc-face-p
 ar-use-local-default ar-use-local-default
 ar-variable-name-face ar-variable-name-face
 ar-verbose-p ar-scala-verbose-p
 ar-version ar-version
 ar-which-bufname ar-which-bufname
 )

(setq extended-scala-mode-map
      (let ((map (make-sparse-keymap)))
        ;; (let ((map ar-mode-map))
        ;; electric keys
        (define-key map [(:)] (quote ar-electric-colon))
        (define-key map [(\#)] (quote ar-electric-comment))
        (define-key map [(delete)] (quote ar-electric-delete))
        (define-key map [(control backspace)] (quote ar-hungry-delete-backwards))
        (define-key map [(control c) (delete)] (quote ar-hungry-delete-forward))
        ;; (define-key map [(control y)] (quote ar-electric-yank))
        ;; moving point
        (define-key map [(control c) (control p)] (quote ar-backward-statement))
        (define-key map [(control c) (control n)] (quote ar-forward-statement))
        (define-key map [(control c) (control u)] (quote ar-backward-block))
        (define-key map [(control c) (control q)] (quote ar-forward-block))
        (define-key map [(control meta a)] (quote ar-backward-def-or-class))
        (define-key map [(control meta e)] (quote ar-forward-def-or-class))
        ;; (define-key map [(meta i)] (quote ar-indent-forward-line))
        ;; (define-key map [(control j)] (quote ar-newline-and-indent))
	(define-key map (kbd "C-j") (quote newline))
        ;; Most Pythoneers expect RET ‘ar-newline-and-indent’
	;; which is default of var ar-return-key’
        (define-key map (kbd "RET") ar-return-key)
        ;; (define-key map (kbd "RET") (quote newline))
        ;; (define-key map (kbd "RET") (quote ar-newline-and-dedent))
        (define-key map [(super backspace)] (quote ar-dedent))
        ;; (define-key map [(control return)] (quote ar-newline-and-dedent))
        ;; indentation level modifiers
        (define-key map [(control c) (control l)] (quote ar-shift-left))
        (define-key map [(control c) (control r)] (quote ar-shift-right))
        (define-key map [(control c) (<)] (quote ar-shift-left))
        (define-key map [(control c) (>)] (quote ar-shift-right))
        ;; (define-key map [(control c) (tab)] (quote ar-indent-region))
	(define-key map (kbd "C-c TAB") (quote ar-indent-region))
        (define-key map [(control c) (:)] (quote ar-guess-indent-offset))
        ;; subprocess commands
        (define-key map [(control c) (control c)] (quote ar-execute-buffer))
        (define-key map [(control c) (control m)] (quote ar-execute-import-or-reload))
        (define-key map [(control c) (control s)] (quote ar-execute-string))
        (define-key map [(control c) (|)] (quote ar-execute-region))
        (define-key map [(control meta x)] (quote ar-execute-def-or-class))
        (define-key map [(control c) (!)] (quote ar-shell))
        (define-key map [(control c) (control t)] (quote ar-toggle-shell))
        (define-key map [(control meta h)] (quote ar-mark-def-or-class))
        (define-key map [(control c) (control k)] (quote ar-mark-block-or-clause))
        (define-key map [(control c) (.)] (quote ar-expression))
        (define-key map [(control c) (?,)] (quote ar-partial-expression))
        ;; Miscellaneous
        ;; (define-key map [(super q)] (quote ar-coar-statement))
        (define-key map [(control c) (control d)] (quote ar-pdbtrack-toggle-stack-tracking))
        (define-key map [(control c) (control f)] (quote ar-sort-imports))
        (define-key map [(control c) (\#)] (quote ar-comment-region))
        (define-key map [(control c) (\?)] (quote ar-describe-mode))
        (define-key map [(control c) (control e)] (quote ar-help-at-point))
        (define-key map [(control c) (-)] (quote ar-up-exception))
        (define-key map [(control c) (=)] (quote ar-down-exception))
        (define-key map [(control x) (n) (d)] (quote ar-narrow-to-def-or-class))
        ;; information
        (define-key map [(control c) (control b)] (quote ar-submit-bug-report))
        (define-key map [(control c) (control v)] (quote ar-version))
        (define-key map [(control c) (control w)] (quote ar-pychecker-run))
        ;; (define-key map (kbd "TAB") (quote ar-indent-line))
        (define-key map (kbd "TAB") (quote ar-indent-line))
	;; (if ar-complete-function
        ;;     (progn
        ;;       (define-key map [(meta tab)] ar-complete-function)
        ;;       (define-key map [(esc) (tab)] ar-complete-function))
        ;;   (define-key map [(meta tab)] (quote ar-shell-complete))
        ;;   (define-key map [(esc) (tab)] (quote ar-shell-complete)))
        (substitute-key-definition (quote complete-symbol) (quote completion-at-point)
                                   map global-map)
        (substitute-key-definition (quote backward-up-list) (quote ar-up)
                                   map global-map)
        (substitute-key-definition (quote down-list) (quote ar-down)
                                   map global-map)
	(when ar-use-menu-p
	  (setq map (ar-define-menu map)))

        map))

(provide 'extended-scala-mode)
;;; extended-scala-mode.el ends here
