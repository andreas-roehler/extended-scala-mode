;;; scala-vars.el --- Edit, debug, develop and run programs. -*- lexical-binding: t; -*-

(require 'ansi-color)
(ignore-errors (require 'subr-x))
(require 'cc-cmds)
(require 'comint)
(require 'compile)
(require 'custom)
(require 'ert)
(require 'flymake)
(require 'hippie-exp)
(require 'hideshow)
(require 'json)
(require 'shell)
(require 'thingatpt)
(require 'which-func)
(require 'tramp)
(require 'tramp-sh)
(require 'org-loaddefs)
(unless (functionp 'mapcan)
  (require 'cl-extra)
  ;; mapcan does not exist in Emacs 25
  (defalias 'mapcan 'cl-mapcan)
  )

(define-minor-mode scala-electric-backspace-mode
  "When on, <backspace> key will delete all whitespace chars before point.

Default is nil"
  :group 'ar-scala-scala-mode
  :lighter " eb"
  (if scala-electric-backspace-mode
      (if (ignore-errors (functionp 'keymap-local-set))
          (keymap-local-set "<backspace>" 'ar-scala-electric-backspace)
        (local-set-key "<backspace>" 'ar-scala-electric-backspace))
    (if (ignore-errors (functionp 'keymap-local-unset))
        (keymap-local-unset "<backspace>")
      (local-unset-key "<backspace>"))))

(defvar comint-mime-setup-script-dir nil
  "Avoid compiler warning")

(defvar comint-mime-enabled-types nil
  "Avoid compiler warning")

(defvar comint-mime-setup-function-alist nil
  "Avoid compiler warning")

(defvar comint-mime-setup-function-alist nil
  "Avoid compiler warning")

(defgroup scala-mode nil
  "Support for the Python programming language, <http://www.python.org/>"
  :group 'languages
  :prefix "ar-scala-")

(defconst scala-version "6.3.1")

(defvar scala-install-directory nil
  "Make sure it exists.")

(defcustom scala-install-directory nil
  "Directory where scala-mode.el and its subdirectories should be installed.

Needed for completion and other environment stuff only."

  :type 'string
   :tag "ar-scala-scala-install-directory"
  :group 'ar-scala-scala-mode)

(defcustom scala-font-lock-defaults-p t
  "If fontification is not required, avoiding it might speed up things."

  :type 'boolean
   :tag "ar-scala-scala-font-lock-defaults-p"
  :group 'ar-scala-scala-mode
  :safe 'booleanp)

(defcustom scala-register-shell-buffer-p nil
  "If non-nil, register new scala-shell according to scala-register-char as REGISTER.

Default is nil.
See ‘window-configuration-to-register’"

  :type 'boolean
   :tag "ar-scala-scala-register-shell-buffer-p"
  :group 'ar-scala-scala-mode
  :safe 'booleanp)

(defcustom scala-register-char ?y
  "Char used by scala-register-shell-buffer-p

Default is ‘y’.
See also ‘window-configuration-to-register’"

  :type 'char
   :tag "ar-scala-scala-register-char"
  :group 'ar-scala-scala-mode
  :safe 'characterp)

(defcustom scala-pythonpath ""
  "Define $PYTHONPATH here, if needed.

Emacs does not read .bashrc"

  :type 'string
   :tag "ar-scala-scala-pythonpath"
  :group 'ar-scala-scala-mode)

(defcustom scala-mode-modeline-display "Py"
  "String to display in Emacs modeline."

  :type 'string
  :tag "python-mode-modeline-display"
  :group 'ar-scala-scala-mode)

(defcustom scala-python2-modeline-display "Py2"
  "String to display in Emacs modeline."

  :type 'string
  :tag "python2-mode-modeline-display"
  :group 'ar-scala-scala-mode)

(defcustom scala-python3-modeline-display "Py3"
  "String to display in Emacs modeline."

  :type 'string
  :tag "python3-mode-modeline-display"
  :group 'ar-scala-scala-mode)

(defcustom scala-ipython-modeline-display "IPy"
  "String to display in Emacs modeline."

  :type 'string
  :tag "ipython-modeline-display"
  :group 'ar-scala-scala-mode)

(defcustom scala-jython-modeline-display "Jy"
  "String to display in Emacs modeline."

  :type 'string
  :tag "jython-modeline-display"
  :group 'ar-scala-scala-mode)

(defcustom scala-extensions "ar-scala-extensions.el"
  "File where extensions to scala-mode.el should be installed.

Used by virtualenv support."

  :type 'string
   :tag "ar-scala-scala-extensions"
  :group 'ar-scala-scala-mode)

(defcustom info-lookup-mode "python"
  "Which Python documentation should be queried.

Make sure it is accessible from Emacs by \\<emacs-lisp-mode-map> \\[info] ...
See INSTALL-INFO-FILES for help."

  :type 'string
  :tag "info-lookup-mode"
  :group 'ar-scala-scala-mode)

(defcustom scala-fast-process-p nil
  "Use ‘ar-fast-process’.

Commands prefixed \"ar-scala-fast-...\" suitable for large output

See: large output makes Emacs freeze, lp:1253907

Results arrive in output buffer, which is not in comint-mode"

  :type 'boolean
   :tag "ar-scala-scala-fast-process-p"
  :group 'ar-scala-scala-mode
  :safe 'booleanp)

;; credits to python.el
(defcustom scala-shell-compilation-regexp-alist
  `((,(rx line-start (1+ (any " \t")) "File \""
          (group (1+ (not (any "\"<")))) ; avoid ‘<stdin>’ &c
          "\", line " (group (1+ digit)))
     1 2)
    (,(rx " in file " (group (1+ not-newline)) " on line "
          (group (1+ digit)))
     1 2)
    (,(rx line-start "> " (group (1+ (not (any "(\"<"))))
          "(" (group (1+ digit)) ")" (1+ (not (any "("))) "()")
     1 2))
  "‘compilation-error-regexp-alist’ for ‘ar-shell’."
  :type '(alist string)
   :tag "ar-scala-scala-scala-shell-compilation-regexp-alist"
  :group 'ar-scala-scala-mode)

(defcustom scala-shift-require-transient-mark-mode-p t
  "If scala-shift commands require variable ‘transient-mark-mode’ set to t.

Default is t"

  :type 'boolean
   :tag "ar-scala-scala-shift-require-transient-mark-mode-p"
  :group 'ar-scala-scala-mode
  :safe 'booleanp)

(defvar scala-fast-output-buffer "*Python Fast*"
  "Internally used. ‘buffer-name’ for fast-processes.")

(defvar scala-this-result nil
  "Internally used, store return-value.")

(defconst scala-coding-re
  "\\(# *coding[ \t]*=\\|#[ \t]*\-*\-[ \t]*coding:\\|#[ \t]*encoding:\\)[ \t]*\\([[:graph:]+]\\)"
 "Fetch the coding cookie maybe.")

(defcustom scala-comment-auto-fill-p nil
  "When non-nil, fill comments.

Defaut is nil"

  :type 'boolean
   :tag "ar-scala-scala-comment-auto-fill-p"
  :group 'ar-scala-scala-mode
  :safe 'booleanp)

(defcustom scala-sexp-use-expression-p nil
  "If non-nil, ‘forward-sexp’ will call ‘ar-forward-expression’.

Respective ‘backward-sexp’ will call ‘ar-backward-expression’
Default is t"
  :type 'boolean
   :tag "ar-scala-scala-sexp-use-expression-p"
  :group 'ar-scala-scala-mode
  :safe 'booleanp)

(defcustom scala-session-p t
  "If commands would use an existing process.

Default is t"

  :type 'boolean
   :tag "ar-scala-scala-session-p"
  :group 'ar-scala-scala-mode
  :safe 'booleanp)

(defvar scala-chars-before " \t\n\r\f"
  "Used by ‘ar-scala--string-strip’.")

(defvar scala-chars-after " \t\n\r\f"
    "Used by ‘ar-scala--string-strip’.")

(defcustom scala-max-help-buffer-p nil
  "If \"\*Python-Help\*\"-buffer should appear as the only visible.

Default is nil.  In ‘help-buffer’, \"q\" will close it."

  :type 'boolean
   :tag "ar-scala-scala-max-help-buffer-p"
  :group 'ar-scala-scala-mode
  :safe 'booleanp)

(defcustom scala-highlight-error-source-p nil
  "Respective code in source-buffer will be highlighted.

Default is nil.

\\<python-mode-map> ‘ar-remove-overlays-at-point’ removes that highlighting."
  :type 'boolean
   :tag "ar-scala-scala-highlight-error-source-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-set-pager-cat-p nil
  "If the shell environment variable $PAGER should set to ‘cat’.

Avoids lp:783828,
 \"Terminal not fully functional\", for help('COMMAND') in scala-shell

When non-nil, imports module ‘os’"

  :type 'boolean
   :tag "ar-scala-scala-set-pager-cat-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-empty-line-closes-p nil
  "When non-nil, dedent after empty line following block.

if True:
    print(\"Part of the if-statement\")

print(\"Not part of the if-statement\")

Default is nil"

  :type 'boolean
   :tag "ar-scala-scala-empty-line-closes-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-prompt-on-changed-p t
  "Ask for save before a changed buffer is sent to interpreter.

Default is t"

  :type 'boolean
   :tag "ar-scala-scala-prompt-on-changed-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-dedicated-process-p nil
  "If commands executing code use a dedicated shell.

Default is nil

When non-nil and ‘ar-session-p’, an existing
dedicated process is re-used instead of default
 - which allows executing stuff in parallel."
  :type 'boolean
   :tag "ar-scala-scala-dedicated-process-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-store-result-p nil
  "Put resulting string of ‘ar-execute-...’ into ‘kill-ring’.

Default is nil"

  :type 'boolean
   :tag "ar-scala-scala-dedicated-process-p"
  :group 'ar-scala-scala-mode)

(defvar scala-shell--font-lock-buffer "*PSFLB*"
  "May contain the ‘ar-buffer-name’ currently fontified." )

(defvar scala-return-result-p nil
  "Internally used.

When non-nil, return resulting string of ‘ar-execute-...’.
Imports will use it with nil.
Default is nil")

(defcustom scala--execute-use-temp-file-p nil
 "Assume execution at a remote machine.

 where write-access is not given."

 :type 'boolean
  :tag "ar-scala-scala-scala--execute-use-temp-file-p"
 :group 'ar-scala-scala-mode)

(defvar scala--match-paren-forward-p nil
  "Internally used by ‘ar-match-paren’.")

(defvar scala-new-session-p t
  "Internally used.  See lp:1393882.

Restart ‘ar-shell’ once with new Emacs/‘python-mode’.")

(defcustom scala-electric-close-active-p nil
  "Close completion buffer if no longer needed.

Works around a bug in ‘choose-completion’.
Default is nil"
  :type 'boolean
   :tag "ar-scala-scala-electric-close-active-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-hide-show-minor-mode-p nil
  "If hide-show minor-mode should be on, default is nil."

  :type 'boolean
   :tag "ar-scala-scala-hide-show-minor-mode-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-do-completion-p t
  "Permits disabling all scala-mode native completion.

Default is ‘t’.
See #144, how to disable process spawn for autocompletion"

  :type 'boolean
   :tag "ar-scala-scala-do-completion-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-load-skeletons-p nil
  "If skeleton definitions should be loaded, default is nil.

If non-nil and variable ‘abbrev-mode’ on, block-skeletons will inserted.
Pressing \"if<SPACE>\" for example will prompt for the if-condition."

  :type 'boolean
   :tag "ar-scala-scala-load-skeletons-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-if-name-main-permission-p t
  "Allow execution of code inside blocks started.

by \"if __name__== '__main__':\".
Default is non-nil"

  :type 'boolean
   :tag "ar-scala-scala-if-name-main-permission-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-use-font-lock-doc-face-p nil
  "If documention string inside of def or class get ‘font-lock-doc-face’.

‘font-lock-doc-face’ inherits ‘font-lock-string-face’.
Call \\<emacs-lisp-mode-map> \\[customize-face] in order to have a effect."

  :type 'boolean
   :tag "ar-scala-scala-use-font-lock-doc-face-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-empty-comment-line-separates-paragraph-p t
  "Consider paragraph start/end lines with nothing inside but comment sign.

Default is  non-nil"
  :type 'boolean
   :tag "ar-scala-scala-empty-comment-line-separates-paragraph-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-indent-honors-inline-comment nil
  "If non-nil, indents to column of inlined comment start.
Default is nil."
  :type 'boolean
   :tag "ar-scala-scala-indent-honors-inline-comment"
  :group 'ar-scala-scala-mode)

(defcustom scala-auto-fill-mode nil
  "If ‘python-mode’ should set ‘fill-column’.

according to values
in ‘ar-comment-fill-column’ and ‘ar-docstring-fill-column’.
Default is  nil"

  :type 'boolean
   :tag "ar-scala-scala-auto-fill-mode"
  :group 'ar-scala-scala-mode)

(defcustom scala-error-markup-delay 4
  "Seconds error's are highlighted in exception buffer."

  :type 'integer
   :tag "ar-scala-scala-error-markup-delay"
  :group 'ar-scala-scala-mode)

(defcustom scala-fast-completion-delay 0.1
  "Used by ‘ar-fast-send-string’."

  :type 'float
   :tag "ar-scala-scala-fast-completion-delay"
  :group 'ar-scala-scala-mode)

(defcustom scala-new-shell-delay
    (if (eq system-type 'windows-nt)
      2.0
    1.0)

  "If a new comint buffer is connected to Python.
Commands like completion might need some delay."

  :type 'float
   :tag "ar-scala-scala-new-shell-delay"
  :group 'ar-scala-scala-mode)

(defcustom scala-autofill-timer-delay 1
  "Delay when idle."
  :type 'integer
   :tag "ar-scala-scala-autofill-timer-delay"
  :group 'ar-scala-scala-mode)

(defcustom scala-docstring-fill-column 72
  "Value of ‘fill-column’ to use when filling a docstring.
Any non-integer value means do not use a different value of
‘fill-column’ when filling docstrings."
  :type '(choice (integer)
                 (const :tag "Use the current ‘fill-column’" t))
   :tag "ar-scala-scala-docstring-fill-column"
  :group 'ar-scala-scala-mode)

(defcustom scala-comment-fill-column 79
  "Value of ‘fill-column’ to use when filling a comment.
Any non-integer value means do not use a different value of
‘fill-column’ when filling docstrings."
  :type '(choice (integer)
		 (const :tag "Use the current ‘fill-column’" t))
   :tag "ar-scala-scala-comment-fill-column"
  :group 'ar-scala-scala-mode)

(defcustom scala-fontify-shell-buffer-p nil
  "If code in Python shell should be highlighted as in script buffer.

Default is nil.

If t, related vars like ‘comment-start’ will be set too.
Seems convenient when playing with stuff in IPython shell
Might not be TRT when a lot of output arrives"

  :type 'boolean
   :tag "ar-scala-scala-fontify-shell-buffer-p"
  :group 'ar-scala-scala-mode)

(defvar scala-modeline-display ""
  "Internally used.")

(defcustom scala-modeline-display-full-path-p nil
  "If the full PATH/TO/PYTHON be in modeline.

Default is nil. Note: when ‘ar-scala-command’ is
specified with path, it is shown as an acronym in
‘buffer-name’ already."

  :type 'boolean
   :tag "ar-scala-scala-modeline-display-full-path-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-modeline-acronym-display-home-p nil
  "If the modeline acronym should contain chars indicating the home-directory.

Default is nil"
  :type 'boolean
   :tag "ar-scala-scala-modeline-acronym-display-home-p"
  :group 'ar-scala-scala-mode)

(defvar highlight-indent-active nil)
;; (defvar autopair-mode nil)

(defvar-local scala--editbeg nil
  "Internally used by ‘ar-edit-docstring’ and others")

(defvar-local scala--editend nil
  "Internally used by ‘ar-edit-docstring’ and others")

(defvar scala--oldbuf nil
  "Internally used by ‘ar-edit-docstring’.")

(defvar scala-edit-buffer "Edit docstring"
  "Name of the temporary buffer to use when editing.")

(defvar scala--edit-register nil)

(defvar scala-result nil
  "Internally used.  May store result from Python process.

See var ‘ar-return-result-p’ and command ‘ar-toggle-ar-return-result-p’")

(defvar scala-error nil
  "Takes the error-messages from Python process.")

(defvar scala-python-completions "*Python Completions*"
  "Buffer name for Python-shell completions, internally used.")

(defvar scala-ipython-completions "*IPython Completions*"
  "Buffer name for IPython-shell completions, internally used.")

(defcustom scala-timer-close-completions-p t
  "If ‘ar-timer-close-completion-buffer’ should run, default is non-nil."

  :type 'boolean
   :tag "ar-scala-scala-timer-close-completions-p"
  :group 'ar-scala-scala-mode)

;; (defcustom scala-autopair-mode nil
;;   "If ‘python-mode’ calls (autopair-mode-on)

;; Default is nil
;; Load ‘autopair-mode’ written by Joao Tavora <joaotavora [at] gmail.com>
;; URL: http://autopair.googlecode.com"
;;   :type 'boolean
;;    :tag "ar-scala-scala-autopair-mode"
;;   :group 'ar-scala-scala-mode)

(defcustom scala-indent-no-completion-p nil
  "If completion function should insert a TAB when no completion found.

Default is nil"
  :type 'boolean
   :tag "ar-scala-scala-indent-no-completion-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-company-pycomplete-p nil
  "Load company-pycomplete stuff.  Default is  nil."

  :type 'boolean
   :tag "ar-scala-scala-company-pycomplete-p"
  :group 'ar-scala-scala-mode)

(defvar scala-last-position nil
    "Used by ‘ar-help-at-point’.

Avoid repeated call at identic pos.")

(defvar scala-auto-completion-mode-p nil
  "Internally used by ‘ar-auto-completion-mode’.")

(defvar scala-complete-last-modified nil
  "Internally used by ‘ar-auto-completion-mode’.")

(defvar scala--auto-complete-timer nil
  "Internally used by ‘ar-auto-completion-mode’.")

(defvar scala-auto-completion-buffer nil
  "Internally used by ‘ar-auto-completion-mode’.")

(defvar scala--auto-complete-timer-delay 1
  "Seconds Emacs must be idle to trigger auto-completion.

See ‘ar-auto-completion-mode’")

(defcustom scala-auto-complete-p nil
  "Run scala-mode's built-in auto-completion via ‘ar-complete-function’.

Default is  nil."

  :type 'boolean
   :tag "ar-scala-scala-auto-complete-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-tab-shifts-region-p nil
  "If t, TAB will indent/cycle the region, not just the current line.

Default is  nil
See also ‘ar-tab-indents-region-p’"

  :type 'boolean
   :tag "ar-scala-scala-tab-shifts-region-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-tab-indents-region-p nil
  "When t and first TAB does not shift, ‘indent-region’ is called.

Default is  nil
See also ‘ar-tab-shifts-region-p’"

  :type 'boolean
   :tag "ar-scala-scala-tab-indents-region-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-block-comment-prefix-p t
  "If scala-comment inserts ‘ar-block-comment-prefix’.

Default is t"

  :type 'boolean
   :tag "ar-scala-scala-block-comment-prefix-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-org-cycle-p nil
  "When non-nil, command ‘org-cycle’ is available at shift-TAB, <backtab>.

Default is nil."
  :type 'boolean
   :tag "ar-scala-scala-org-cycle-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-set-complete-keymap-p  nil
  "If ‘ar-complete-initialize’.

Sets up enviroment for Pymacs based scala-complete.
 Should load its keys into ‘python-mode-map’
Default is nil.
See also resp. edit ‘ar-complete-set-keymap’"

  :type 'boolean
   :tag "ar-scala-scala-set-complete-keymap-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-outline-minor-mode-p t
  "If outline minor-mode should be on, default is t."
  :type 'boolean
   :tag "ar-scala-scala-outline-minor-mode-p"
  :group 'ar-scala-scala-mode)

(defvar scala-guess-ar-install-directory-p nil
  "If in cases, ‘ar-install-directory’ is not set,  ‘scala-set-load-path’ guess it.")

(defcustom scala-guess-ar-install-directory-p nil
  "If in cases, ‘ar-install-directory’ is not set, ‘scala-set-load-path’ guesses it."
  :type 'boolean
   :tag "ar-scala-scala-guess-ar-install-directory-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-load-pymacs-p nil
  "If Pymacs related stuff should be loaded. Default is nil.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.ca"
  :type 'boolean
   :tag "ar-scala-scala-load-pymacs-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-verbose-p nil
  "If functions should report results.

Default is nil."
  :type 'boolean
   :tag "ar-scala-scala-verbose-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-sexp-function nil
  "Called instead of ‘forward-sexp’, ‘backward-sexp’.

Default is nil."

  :type '(choice

          (const :tag "default" nil)
          (const  :tag "ar-scala-scala-forward-partial-expression" scala-forward-partial-expression)
          (const  :tag "ar-scala-scala-forward-expression" scala-forward-expression))
   :tag "ar-scala-scala-sexp-function"
  :group 'ar-scala-scala-mode)

(defcustom scala-close-provides-newline t
  "If a newline is inserted, when line after block is not empty.

Default is non-nil.
When non-nil, ‘ar-forward-def’ and related will work faster"
  :type 'boolean
   :tag "ar-scala-scala-close-provides-newline"
  :group 'ar-scala-scala-mode)

(defcustom scala-dedent-keep-relative-column t
  "If point should follow dedent or kind of electric move to end of line.

Default is t - keep relative position."
  :type 'boolean
   :tag "ar-scala-scala-dedent-keep-relative-column"
  :group 'ar-scala-scala-mode)

(defcustom scala-indent-list-style 'line-up-with-first-element
  "Sets the basic indentation style of lists.

The term ‘list’ here is seen from Emacs Lisp editing purpose.
A list symbolic expression means everything delimited by
brackets, parentheses or braces.

Setting here might be ignored in case of canonical indent.

‘line-up-with-first-element’ indents to 1+ column
of opening delimiter

def foo (a,
         b):

but ‘one-level-to-beginning-of-statement’ in case of EOL at list-start

def foo (
    a,
    b):

‘one-level-to-beginning-of-statement’ adds
‘ar-indent-offset’ to beginning

def long_function_name(
    var_one, var_two, var_three,
    var_four):
    print(var_one)

‘one-level-from-first-element’ adds ‘ar-indent-offset’ from first element
def foo():
    if (foo &&
            baz):
        bar()"
  :type '(choice
          (const :tag "line-up-with-first-element" line-up-with-first-element)
          (const :tag "one-level-to-beginning-of-statement" one-level-to-beginning-of-statement)
          (const :tag "one-level-from-first-element" one-level-from-first-element)
          )
   :tag "ar-scala-scala-indent-list-style"
  :group 'ar-scala-scala-mode)
(make-variable-buffer-local 'ar-scala-indent-list-style)

(defcustom scala-closing-list-dedents-bos nil
  "When non-nil, indent lists closing delimiter like start-column.

It will be lined up under the first character of
 the line that starts the multi-line construct, as in:

my_list = [
    1, 2, 3,
    4, 5, 6
]

result = some_function_that_takes_arguments(
    \\='a\\=', \\='b\\=', \\='c\\=',
    \\='d\\=', \\='e\\=', \\='f\\='
)

Default is nil, i.e.

my_list = [
    1, 2, 3,
    4, 5, 6
    ]

result = some_function_that_takes_arguments(
    \\='a\\=', \\='b\\=', \\='c\\=',
    \\='d\\=', \\='e\\=', \\='f\\='
    )

Examples from PEP8
URL: https://www.python.org/dev/peps/pep-0008/#indentation"
  :type 'boolean
   :tag "ar-scala-scala-closing-list-dedents-bos"
  :group 'ar-scala-scala-mode)

(defvar scala-imenu-max-items 99)
(defcustom scala-imenu-max-items 99
 "Python-mode specific ‘imenu-max-items’."
 :type 'number
  :tag "ar-scala-scala-imenu-max-items"
 :group 'ar-scala-scala-mode)

(defcustom scala-closing-list-space 1
  "Number of chars, closing parenthesis outdent from opening, default is 1."
  :type 'number
   :tag "ar-scala-scala-closing-list-space"
  :group 'ar-scala-scala-mode)

(defcustom scala-max-specpdl-size 99
  "Heuristic exit.
e
Limiting number of recursive calls by ‘ar-forward-statement’ and related.
Default is ‘max-specpdl-size’.

This threshold is just an approximation.  It might set far higher maybe.

See lp:1235375. In case code is not to navigate due to errors,
command ‘which-function-mode’ and others might make Emacs hang.

Rather exit than."

  :type 'number
   :tag "ar-scala-scala-max-specpdl-size"
  :group 'ar-scala-scala-mode)

(defcustom scala-closing-list-keeps-space nil
  "If non-nil, closing parenthesis dedents onto column of opening.
Adds ‘ar-closing-list-space’.
Default is nil."
  :type 'boolean
   :tag "ar-scala-scala-closing-list-keeps-space"
  :group 'ar-scala-scala-mode)

(defcustom scala-electric-colon-active-p nil
  "‘ar-scala-electric-colon’ feature.

Default is nil.  See lp:837065 for discussions.
See also ‘ar-scala-electric-colon-bobl-only’"
  :type 'boolean
   :tag "ar-scala-scala-electric-colon-active-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-electric-colon-bobl-only t

  "When inserting a colon, do not indent lines unless at beginning of block.

See lp:1207405 resp. ‘ar-scala-electric-colon-active-p’"

  :type 'boolean
   :tag "ar-scala-scala-electric-colon-bobl-only"
  :group 'ar-scala-scala-mode)

(defcustom scala-electric-yank-active-p nil
  "When non-nil, ‘yank’ will be followed by an ‘indent-according-to-mode’.

Default is nil"
  :type 'boolean
   :tag "ar-scala-scala-electric-yank-active-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-electric-colon-greedy-p nil
  "If ‘ar-scala-electric-colon’ should indent to the outmost reasonable level.

If nil, default, it will not move from at any reasonable level."
  :type 'boolean
   :tag "ar-scala-scala-electric-colon-greedy-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-electric-colon-newline-and-indent-p nil
  "If non-nil, ‘ar-scala-electric-colon’ will call ‘newline-and-indent’.

Default is nil."
  :type 'boolean
   :tag "ar-scala-scala-electric-colon-newline-and-indent-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-electric-comment-p nil
  "If \"#\" should call ‘ar-scala-electric-comment’. Default is nil."
  :type 'boolean
   :tag "ar-scala-scala-electric-comment-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-electric-comment-add-space-p nil
  "If ‘ar-scala-electric-comment’ should add a space.  Default is nil."
  :type 'boolean
   :tag "ar-scala-scala-electric-comment-add-space-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-defun-use-top-level-p nil
 "If ‘beginning-of-defun’, ‘end-of-defun’ calls function ‘top-level’ form.

Default is nil.

beginning-of defun, ‘end-of-defun’ forms use
commands ‘ar-backward-top-level’, ‘ar-forward-top-level’

‘mark-defun’ marks function ‘top-level’ form at point etc."

 :type 'boolean
   :tag "ar-scala-scala-defun-use-top-level-p"
 :group 'ar-scala-scala-mode)

(defcustom scala-tab-indent t
  "Non-nil means TAB in Python mode calls ‘ar-indent-line’."
  :type 'boolean
   :tag "ar-scala-scala-tab-indent"
  :group 'ar-scala-scala-mode)

(defcustom scala-return-key 'ar-scala-newline-and-indent
  "Which command <return> should call."
  :type '(choice

          (const :tag "default" scala-newline-and-indent)
          (const :tag "newline" newline)
          (const  :tag "ar-scala-scala-newline-and-dedent" scala-newline-and-dedent)
          )
   :tag "ar-scala-scala-return-key"
  :group 'ar-scala-scala-mode)

(defcustom scala-complete-function 'ar-scala-fast-complete
  "When set, enforces function todo completion, default is ‘ar-fast-complete’.

Might not affect IPython, as ‘ar-scala-shell-complete’ is the only known working here.
Normally ‘python-mode’ knows best which function to use."
  :type '(choice

          (const :tag "default" nil)
          (const :tag "Pymacs and company based scala-complete" scala-complete)
          (const  :tag "ar-scala-scala-scala-shell-complete" scala-shell-complete)
          (const  :tag "ar-scala-scala-indent-or-complete" scala-indent-or-complete)
	  (const  :tag "ar-scala-scala-fast-complete" scala-fast-complete)
          )
   :tag "ar-scala-scala-complete-function"
  :group 'ar-scala-scala-mode)

(defcustom scala-encoding-string " # -*- coding: utf-8 -*-"
  "Default string specifying encoding of a Python file."
  :type 'string
   :tag "ar-scala-scala-encoding-string"
  :group 'ar-scala-scala-mode)

(defcustom scala-shebang-startstring "#! /bin/env"
  "Detecting the shell in head of file."
  :type 'string
   :tag "ar-scala-scala-shebang-startstring"
  :group 'ar-scala-scala-mode)

(defcustom scala-flake8-command ""
  "Which command to call flake8.

If empty, ‘python-mode’ will guess some"
  :type 'string
   :tag "ar-scala-scala-flake8-command"
  :group 'ar-scala-scala-mode)

(defcustom scala-flake8-command-args ""
  "Arguments used by flake8.

Default is the empty string."
  :type 'string
   :tag "ar-scala-scala-flake8-command-args"
  :group 'ar-scala-scala-mode)

(defvar scala-flake8-history nil
  "Used by flake8, resp. ‘ar-flake8-command’.

Default is nil.")

(defcustom scala-message-executing-temporary-file t
  "If execute functions using a temporary file should message it.

Default is t.
Messaging increments the prompt counter of IPython shell."
  :type 'boolean
   :tag "ar-scala-scala-message-executing-temporary-file"
  :group 'ar-scala-scala-mode)

(defcustom scala-execute-no-temp-p nil
  "Seems Emacs-24.3 provided a way executing stuff without temporary files."
  :type 'boolean
   :tag "ar-scala-scala-execute-no-temp-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-lhs-inbound-indent 1
  "When line starts a multiline-assignment.

How many colums indent more than opening bracket, brace or parenthesis."
  :type 'integer
   :tag "ar-scala-scala-lhs-inbound-indent"
  :group 'ar-scala-scala-mode)

(defcustom scala-continuation-offset 2
  "Additional amount of offset to give for some continuation lines.
Continuation lines are those that immediately follow a backslash
terminated line."
  :type 'integer
   :tag "ar-scala-scala-continuation-offset"
  :group 'ar-scala-scala-mode)

(defcustom scala-indent-tabs-mode nil
  "Python-mode starts ‘indent-tabs-mode’ with the value specified here.

Default is nil."
  :type 'boolean
   :tag "ar-scala-scala-indent-tabs-mode"
  :group 'ar-scala-scala-mode)

(defcustom scala-smart-indentation nil
  "Guess ‘ar-indent-offset’.  Default is nil.

Setting it to t seems useful only in cases where customizing
‘ar-indent-offset’ is no option - for example because the
indentation step is unknown or differs inside the code.

When this variable is non-nil, ‘ar-indent-offset’ is guessed from existing code.

Which might slow down the proceeding."

  :type 'boolean
   :tag "ar-scala-scala-smart-indentation"
  :group 'ar-scala-scala-mode)

(defcustom scala-block-comment-prefix "##"
  "String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands will not get confused (i.e., the string
should be of the form ‘#x...’ where ‘x’ is not a blank or a tab, and
 ‘...’ is arbitrary).  However, this string should not end in whitespace."
  :type 'string
   :tag "ar-scala-scala-block-comment-prefix"
  :group 'ar-scala-scala-mode)

(defcustom scala-indent-offset 4
  "Amount of offset per level of indentation.
‘\\[py-guess-indent-offset]’ can usually guess a good value when
you're editing someone else's Python code."
  :type 'integer
   :tag "ar-scala-scala-indent-offset"
  :group 'ar-scala-scala-mode)
(make-variable-buffer-local 'ar-scala-indent-offset)

(defcustom scala-backslashed-lines-indent-offset 5
  "Amount of offset per level of indentation of backslashed.
No semantic indent,  which diff to ‘ar-indent-offset’ indicates"
  :type 'integer
   :tag "ar-scala-scala-backslashed-lines-indent-offset"
  :group 'ar-scala-scala-mode)

(defcustom scala-shell-completion-native-output-timeout 5.0
  "Time in seconds to wait for completion output before giving up."
  :version "25.1"
  :type 'float
   :tag "ar-scala-scala-scala-shell-completion-native-output-timeout"
  :group 'ar-scala-scala-mode)

(defcustom scala-shell-completion-native-try-output-timeout 1.0
  "Time in seconds to wait for *trying* native completion output."
  :version "25.1"
  :type 'float
   :tag "ar-scala-scala-scala-shell-completion-native-try-output-timeout"
  :group 'ar-scala-scala-mode)

(defvar scala-shell--first-prompt-received-output-buffer nil)
(defvar scala-shell--first-prompt-received nil)

(defcustom scala-shell-first-prompt-hook nil
  "Hook run upon first (non-pdb) shell prompt detection.
This is the place for shell setup functions that need to wait for
output.  Since the first prompt is ensured, this helps the
current process to not hang while waiting.  This is useful to
safely attach setup code for long-running processes that
eventually provide a shell."
  :version "25.1"
  :type 'hook
   :tag "ar-scala-scala-scala-shell-first-prompt-hook"
  :group 'ar-scala-scala-mode)

(defvar scala-shell--parent-buffer nil)

(defvar scala-shell--package-depth 10)

(defcustom scala-indent-comments t
  "When t, comment lines are indented."
  :type 'boolean
   :tag "ar-scala-scala-indent-comments"
  :group 'ar-scala-scala-mode)

(defcustom scala-uncomment-indents-p nil
  "When non-nil, after uncomment indent lines."
  :type 'boolean
   :tag "ar-scala-scala-uncomment-indents-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-separator-char "/"
  "The character, which separates the system file-path components.

Precedes guessing when not empty, returned by function ‘ar-separator-char’."
  :type 'string
   :tag "ar-scala-scala-separator-char"
  :group 'ar-scala-scala-mode)

(defvar scala-separator-char "/"
  "Values set by defcustom only will not be seen in batch-mode.")

(and
 ;; used as a string finally
 ;; kept a character not to break existing customizations
 (characterp scala-separator-char)(setq scala-separator-char (char-to-string scala-separator-char)))

(defcustom scala-custom-temp-directory ""
  "If set, will take precedence over guessed values from ‘ar-temp-directory’.

Default is the empty string."
  :type 'string
   :tag "ar-scala-scala-custom-temp-directory"
  :group 'ar-scala-scala-mode)

(defcustom scala-beep-if-tab-change t
  "Ring the bell if ‘tab-width’ is changed.
If a comment of the form

                           \t# vi:set tabsize=<number>:

is found before the first code line when the file is entered, and the
current value of (the general Emacs variable) ‘tab-width’ does not
equal <number>, ‘tab-width’ is set to <number>, a message saying so is
displayed in the echo area, and if ‘ar-beep-if-tab-change’ is non-nil
the Emacs bell is also rung as a warning."
  :type 'boolean
   :tag "ar-scala-scala-beep-if-tab-change"
  :group 'ar-scala-scala-mode)

(defcustom scala-jump-on-exception t
  "Jump to innermost exception frame in Python output buffer.
When this variable is non-nil and an exception occurs when running
Python code synchronously in a subprocess, jump immediately to the
source code of the innermost traceback frame."
  :type 'boolean
   :tag "ar-scala-scala-jump-on-exception"
  :group 'ar-scala-scala-mode)

(defcustom scala-ask-about-save t
  "If not nil, ask about which buffers to save before executing some code.
Otherwise, all modified buffers are saved without asking."
  :type 'boolean
   :tag "ar-scala-scala-ask-about-save"
  :group 'ar-scala-scala-mode)

(defcustom scala-delete-function 'delete-char
  "Function called by ‘ar-scala-electric-delete’ when deleting forwards."
  :type 'function
   :tag "ar-scala-scala-delete-function"
  :group 'ar-scala-scala-mode)

(defcustom scala-import-check-point-max
  20000
  "Max number of characters to search Java-ish import statement.

When ‘python-mode’ tries to calculate the shell
-- either a CPython or a Jython shell --
it looks at the so-called ‘shebang’.
If that's not available, it looks at some of the
file heading imports to see if they look Java-like."
  :type 'integer
   :tag "ar-scala-scala-import-check-point-max
"
  :group 'ar-scala-scala-mode)

(defcustom scala-known-shells
  (list
   "ipython"
   "ipython2.7"
   "ipython3"
   "jython"
   "python"
   "python2"
   "python3"
   "pypy"
   )
  "A list of available shells instrumented for commands.
Expects its executables installed

Edit for your needs."
  :type '(repeat string)
   :tag "ar-scala-scala-shells"
  :group 'ar-scala-scala-mode)

(defcustom scala-known-shells-extended-commands
  (list "ipython"
	"python"
	"python3"
	"pypy"
	)
  "A list of shells for finer grained commands.
like ‘ar-execute-statement-ipython’
Expects its executables installed

Edit for your needs."
  :type '(repeat string)
   :tag "ar-scala-scala-shells"
  :group 'ar-scala-scala-mode)

(defcustom scala-jython-packages
  '("java" "javax")
  "Imported packages that imply ‘jython-mode’."
  :type '(repeat string)
   :tag "ar-scala-scala-jython-packages
"
  :group 'ar-scala-scala-mode)

(defcustom scala-current-defun-show t
  "If ‘ar-current-defun’ should jump to the definition.

Highlights it while waiting PY-WHICH-FUNC-DELAY seconds.
Afterwards returning to previous position.

Default is t."

  :type 'boolean
   :tag "ar-scala-scala-current-defun-show"
  :group 'ar-scala-scala-mode)

(defcustom scala-current-defun-delay 2
  "‘ar-current-defun’ waits PY-WHICH-FUNC-DELAY seconds.

Before returning to previous position."

  :type 'number
   :tag "ar-scala-scala-current-defun-delay"
  :group 'ar-scala-scala-mode)

(defcustom scala-python-send-delay 1
  "Seconds to wait for output, used by ‘ar-scala--send-...’ functions.

See also ‘ar-ipython-send-delay’"

  :type 'number
   :tag "ar-scala-scala-python-send-delay"
  :group 'ar-scala-scala-mode)

(defcustom scala-python3-send-delay 1
  "Seconds to wait for output, used by ‘ar-scala--send-...’ functions.

See also ‘ar-ipython-send-delay’"

  :type 'number
   :tag "ar-scala-scala-python3-send-delay"
  :group 'ar-scala-scala-mode)

(defcustom scala-ipython-send-delay 1
  "Seconds to wait for output, used by ‘ar-scala--send-...’ functions.

See also ‘ar-scala-send-delay’"

  :type 'number
   :tag "ar-scala-scala-ipython-send-delay"
  :group 'ar-scala-scala-mode)

(defcustom scala-master-file nil
  "Execute the named master file instead of the buffer's file.

Default is nil.
With relative path variable ‘default-directory’ is prepended.

Beside you may set this variable in the file's local
variable section, e.g.:

                           # Local Variables:
                           # scala-master-file: \"master.py\"
                           # End:"
  :type 'string
   :tag "ar-scala-scala-master-file"
  :group 'ar-scala-scala-mode)
(make-variable-buffer-local 'ar-scala-master-file)

(defcustom scala-pychecker-command "pychecker"
  "Shell command used to run Pychecker."
  :type 'string
   :tag "ar-scala-scala-pychecker-command"
  :group 'ar-scala-scala-mode)

(defcustom scala-pychecker-command-args "--stdlib"
  "String arguments to be passed to pychecker."
  :type 'string
   :tag "ar-scala-scala-pychecker-command-args"
  :group 'ar-scala-scala-mode)

(defcustom scala-pyflakes3-command "pyflakes3"
  "Shell command used to run Pyflakes3."
  :type 'string
   :tag "ar-scala-scala-pyflakes3-command"
  :group 'ar-scala-scala-mode)

(defcustom scala-pyflakes3-command-args ""
  "String arguments to be passed to pyflakes3.

Default is \"\""
  :type 'string
   :tag "ar-scala-scala-pyflakes3-command-args"
  :group 'ar-scala-scala-mode)

(defcustom scala-pep8-command "pep8"
  "Shell command used to run pep8."
  :type 'string
   :tag "ar-scala-scala-pep8-command"
  :group 'ar-scala-scala-mode)

(defcustom scala-pep8-command-args ""
  "String arguments to be passed to pylint.

Default is \"\""
  :type 'string
   :tag "ar-scala-scala-pep8-command-args"
  :group 'ar-scala-scala-mode)

(defcustom scala-pyflakespep8-command (concat scala-install-directory "/pyflakespep8.py")
  "Shell command used to run ‘pyflakespep8’."
  :type 'string
   :tag "ar-scala-scala-pyflakespep8-command"
  :group 'ar-scala-scala-mode)

(defcustom scala-pyflakespep8-command-args ""
  "String arguments to be passed to pyflakespep8.

Default is \"\""
  :type 'string
   :tag "ar-scala-scala-pyflakespep8-command-args"
  :group 'ar-scala-scala-mode)

(defcustom scala-pylint-command "pylint"
  "Shell command used to run Pylint."
  :type 'string
   :tag "ar-scala-scala-pylint-command"
  :group 'ar-scala-scala-mode)

(defcustom scala-pylint-command-args '("--errors-only")
  "String arguments to be passed to pylint.

Default is \"--errors-only\""
  :type '(repeat string)
   :tag "ar-scala-scala-pylint-command-args"
  :group 'ar-scala-scala-mode)

(defvar scala-pdbtrack-input-prompt "^[(<]*[Ii]?[Pp]y?db[>)]+ *"
  "Recognize the prompt.")
(setq scala-pdbtrack-input-prompt "^[(<]*[Ii]?[Pp]y?db[>)]+ *")

(defcustom scala-shell-input-prompt-1-regexp ">>> "
  "A regular expression to match the input prompt of the shell."
  :type 'regexp
   :tag "ar-scala-scala-scala-shell-input-prompt-1-regexp"
  :group 'ar-scala-scala-mode)

(defcustom scala-shell-input-prompt-2-regexp "[.][.][.]:? "
  "A regular expression to match the input prompt.

Applies to the shell after the first line of input."
  :type 'string
   :tag "ar-scala-scala-scala-shell-input-prompt-2-regexp"
  :group 'ar-scala-scala-mode)

(defvar scala-shell-ipython-input-prompt-1-regexp "In \\[[0-9]+\\]: "
  "Regular Expression matching input prompt of python shell.
It should not contain a caret (^) at the beginning.")

(defvar scala-shell-ipython-input-prompt-2-regexp "   \\.\\.\\.: "
  "Regular Expression matching second level input prompt of python shell.
It should not contain a caret (^) at the beginning.")

(defcustom scala-shell-input-prompt-2-regexps
  '(">>> " "\\.\\.\\. "                 ; Python
    "In \\[[0-9]+\\]: "                 ; IPython
    "   \\.\\.\\.: "                    ; IPython
    ;; Using ipdb outside IPython may fail to cleanup and leave static
    ;; IPython prompts activated, this adds some safeguard for that.
    "In : " "\\.\\.\\.: ")
  "List of regular expressions matching input prompts."
  :type '(repeat string)
  :version "24.4"
   :tag "ar-scala-scala-scala-shell-input-prompt-2-regexps"
  :group 'ar-scala-scala-mode)

(defcustom scala-shell-input-prompt-regexps
  '(">>> " "\\.\\.\\. "                 ; Python
    "In \\[[0-9]+\\]: "                 ; IPython
    "   \\.\\.\\.: "                    ; IPython
    ;; Using ipdb outside IPython may fail to cleanup and leave static
    ;; IPython prompts activated, this adds some safeguard for that.
    "In : " "\\.\\.\\.: ")
  "List of regular expressions matching input prompts."
  :type '(repeat regexp)
  :version "24.4"
   :tag "ar-scala-scala-scala-shell-input-prompt-regexps"
  :group 'ar-scala-scala-mode)

(defvar scala-ipython-output-prompt-re "^Out\\[[0-9]+\\]: "
  "A regular expression to match the output prompt of IPython.")

(defcustom scala-shell-output-prompt-regexps
  '(""                                  ; Python
    "Out\\[[0-9]+\\]: "                 ; IPython
    "Out :")                            ; ipdb safeguard
  "List of regular expressions matching output prompts."
  :type '(repeat string)
  :version "24.4"
   :tag "ar-scala-scala-scala-shell-output-prompt-regexps"
  :group 'ar-scala-scala-mode)

(defvar scala-pydbtrack-input-prompt "^[(]*ipydb[>)]+ "
  "Recognize the pydb-prompt.")
;; (setq scala-pdbtrack-input-prompt "^[(< \t]*[Ii]?[Pp]y?db[>)]*.*")

(defvar scala-ipython-input-prompt-re "In \\[?[0-9 ]*\\]?: *\\|^[ ]\\{3\\}[.]\\{3,\\}: *"
  "A regular expression to match the IPython input prompt.")

(defvar scala-shell-prompt-regexp
  (concat "\\("
	  (mapconcat 'identity
		     (delq nil
			   (list
			    scala-shell-input-prompt-1-regexp
			    scala-shell-input-prompt-2-regexp
			    scala-ipython-input-prompt-re
			    scala-ipython-output-prompt-re
			    scala-pdbtrack-input-prompt
			    scala-pydbtrack-input-prompt
			    "[.]\\{3,\\}:? *"
			    ))
		     "\\|")
	  "\\)")
  "Internally used by ‘ar-fast-filter’.
‘ansi-color-filter-apply’ might return
Result: \"\\nIn [10]:    ....:    ....:    ....: 1\\n\\nIn [11]: \"")

(defvar scala-fast-filter-re
  (concat "\\("
	  (mapconcat 'identity
		     (delq nil
			   (list
			    scala-shell-input-prompt-1-regexp
			    scala-shell-input-prompt-2-regexp
			    scala-ipython-input-prompt-re
			    scala-ipython-output-prompt-re
			    scala-pdbtrack-input-prompt
			    scala-pydbtrack-input-prompt
			    "[.]\\{3,\\}:? *"
			    ))
		     "\\|")
	  "\\)")
  "Internally used by ‘ar-fast-filter’.
‘ansi-color-filter-apply’ might return
Result: \"\\nIn [10]:    ....:    ....:    ....: 1\\n\\nIn [11]: \"")

(defcustom scala-shell-prompt-detect-p nil
  "Non-nil enables autodetection of interpreter prompts."
  :type 'boolean
  :safe 'booleanp
  :version "24.4"
   :tag "ar-scala-scala-scala-shell-prompt-detect-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-shell-prompt-read-only t
  "If non-nil, the python prompt is read only.

Setting this variable will only effect new shells."
  :type 'boolean
   :tag "ar-scala-scala-scala-shell-prompt-read-only"
  :group 'ar-scala-scala-mode)

(setq scala-fast-filter-re
  (concat "\\("
	  (mapconcat 'identity
		     (delq nil
			   (list
			    scala-shell-input-prompt-1-regexp
			    scala-shell-input-prompt-2-regexp
			    scala-ipython-input-prompt-re
			    scala-ipython-output-prompt-re
			    scala-pdbtrack-input-prompt
			    scala-pydbtrack-input-prompt
			    "[.]\\{3,\\}:? *"
			    ))
		     "\\|")
	  "\\)"))

(defcustom scala-honor-IPYTHONDIR-p nil
  "When non-nil ipython-history file is constructed by $IPYTHONDIR.

Default is nil.
Otherwise value of ‘ar-ipython-history’ is used."
  :type 'boolean
   :tag "ar-scala-scala-honor-IPYTHONDIR-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-ipython-history "~/.ipython/history"
  "Ipython-history default file.

Used when ‘ar-honor-IPYTHONDIR-p’ is nil - th default"

  :type 'string
   :tag "ar-scala-scala-ipython-history"
  :group 'ar-scala-scala-mode)

(defcustom scala-honor-PYTHONHISTORY-p nil
  "When non-nil scala-history file is set by $PYTHONHISTORY.

Default is nil.
Otherwise value of ‘ar-scala-history’ is used."
  :type 'boolean
   :tag "ar-scala-scala-honor-PYTHONHISTORY-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-python-history "~/.python_history"
  "Python-history default file.

Used when ‘ar-honor-PYTHONHISTORY-p’ is nil (default)."

  :type 'string
   :tag "ar-scala-scala-python-history"
  :group 'ar-scala-scala-mode)

(defcustom scala-switch-buffers-on-execute-p nil
  "When non-nil switch to the Python output buffer.

If ‘ar-keep-windows-configuration’ is t, this will take precedence
over setting here."

  :type 'boolean
   :tag "ar-scala-scala-switch-buffers-on-execute-p"
  :group 'ar-scala-scala-mode)
;; made buffer-local as pdb might need t in all circumstances
(make-variable-buffer-local 'ar-scala-switch-buffers-on-execute-p)

(defcustom scala-split-window-on-execute 'just-two
  "When non-nil split windows.

Default is just-two - when code is send to interpreter.
Splits screen into source-code buffer and current ‘ar-shell’ result.
Other buffer will be hidden that way.

When set to t, ‘python-mode’ tries to reuse existing windows
and will split only if needed.

With \\='always, results will displayed in a new window.

Both t and ‘always’ is experimental still.

For the moment: If a multitude of scala-shells/buffers should be
visible, open them manually and set ‘ar-keep-windows-configuration’ to t.

See also ‘ar-keep-windows-configuration’"
  :type `(choice
          (const :tag "default" just-two)
	  (const :tag "reuse" t)
          (const :tag "no split" nil)
          (const :tag "always" always))
   :tag "ar-scala-scala-split-window-on-execute"
  :group 'ar-scala-scala-mode)

(defcustom scala-split-window-on-execute-threshold 3
  "Maximal number of displayed windows.

Honored, when ‘ar-split-window-on-execute’ is t, i.e. \"reuse\".
Do not split when max number of displayed windows is reached."
  :type 'number
   :tag "ar-scala-scala-split-window-on-execute-threshold"
  :group 'ar-scala-scala-mode)

(defcustom scala-split-windows-on-execute-function 'split-window-vertically
  "How window should get splitted to display results of scala-execute-... functions."
  :type '(choice (const :tag "split-window-vertically" split-window-vertically)
                 (const :tag "split-window-horizontally" split-window-horizontally)
                 )
   :tag "ar-scala-scala-split-windows-on-execute-function"
  :group 'ar-scala-scala-mode)

(defcustom scala-shell-fontify-p 'input
  "Fontify current input in Python shell. Default is input.

INPUT will leave output unfontified.

At any case only current input gets fontified."
  :type '(choice (const :tag "Default" all)
                 (const :tag "Input" input)
		 (const :tag "Nil" nil)
                 )
   :tag "ar-scala-scala-scala-shell-fontify-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-hide-show-keywords
  '("class"    "def"    "elif"    "else"    "except"
    "for"      "if"     "while"   "finally" "try"
    "with"     "match"  "case")
  "Keywords composing visible heads."
  :type '(repeat string)
   :tag "ar-scala-scala-hide-show-keywords
"
  :group 'ar-scala-scala-mode)

(defcustom scala-hide-show-hide-docstrings t
  "Controls if doc strings can be hidden by hide-show."
  :type 'boolean
   :tag "ar-scala-scala-hide-show-hide-docstrings"
  :group 'ar-scala-scala-mode)

(defcustom scala-hide-comments-when-hiding-all t
  "Hide the comments too when you do an ‘hs-hide-all’."
  :type 'boolean
   :tag "ar-scala-scala-hide-comments-when-hiding-all"
  :group 'ar-scala-scala-mode)

(defcustom scala-outline-mode-keywords
  '("class"    "def"    "elif"    "else"    "except"
    "for"      "if"     "while"   "finally" "try"
    "with"     "match"  "case")
  "Keywords composing visible heads."
  :type '(repeat string)
   :tag "ar-scala-scala-outline-mode-keywords
"
  :group 'ar-scala-scala-mode)

(defcustom scala-mode-hook nil
  "Hook run when entering Python mode."

  :type 'hook
  :tag "python-mode-hook"
  :group 'ar-scala-scala-mode
  )

;; (defcustom scala-shell-name
;;   (if (eq system-type 'windows-nt)
;;       "C:/Python27/python"
;;     "python")

;;   "A PATH/TO/EXECUTABLE or default value ‘ar-shell’ may look for.

;; If no shell is specified by command.

;; On Windows default is C:/Python27/python
;; --there is no garantee it exists, please check your system--

;; Else python"
;;   :type 'string
;;    :tag "ar-scala-scala-scala-shell-name
;; "
;;   :group 'ar-scala-scala-mode)

(defcustom scala-python-command
  (if (eq system-type 'windows-nt)
      ;; "C:\\Python27\\python.exe"
      "python"
   ;; "C:/Python33/Lib/site-packages/IPython"
    "python")

  "Make sure directory in in the PATH-variable.

Windows: edit in \"Advanced System Settings/Environment Variables\"
Commonly \"C:\\\\Python27\\\\python.exe\"
With Anaconda for example the following works here:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\python.exe\"

Else /usr/bin/python"

  :type 'string
   :tag "ar-scala-scala-python-command
"
  :group 'ar-scala-scala-mode)

(defvar scala-shell-name scala-python-command)
;; (defvaralias 'ar-scala-shell-name 'ar-scala-python-command)

(defcustom scala-python-command-args '("-i")
  "String arguments to be used when starting a Python shell."
  :type '(repeat string)
   :tag "ar-scala-scala-python-command-args"
  :group 'ar-scala-scala-mode)

(defcustom scala-python2-command
  (if (eq system-type 'windows-nt)
      "C:\\Python27\\python"
    ;; "python2"
    "python2")

  "Make sure, the directory where python.exe resides in in the PATH-variable.

Windows: If needed, edit in
\"Advanced System Settings/Environment Variables\"
Commonly
\"C:\\\\Python27\\\\python.exe\"
With Anaconda for example the following works here:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\python.exe\"

Else /usr/bin/python"

  :type 'string
   :tag "ar-scala-scala-python2-command
"
  :group 'ar-scala-scala-mode)

(defcustom scala-python2-command-args '("-i")
  "String arguments to be used when starting a Python shell."
  :type '(repeat string)
   :tag "ar-scala-scala-python2-command-args"
  :group 'ar-scala-scala-mode)

;; "/usr/bin/python3"
(defcustom scala-python3-command
  (if (eq system-type 'windows-nt)
    "C:/Python33/python"
    "python3")

  "A PATH/TO/EXECUTABLE or default value ‘ar-shell’ may look for.

Unless shell is specified by command.

On Windows see C:/Python3/python.exe
--there is no garantee it exists, please check your system--

At GNU systems see /usr/bin/python3"

  :type 'string
   :tag "ar-scala-scala-python3-command
"
  :group 'ar-scala-scala-mode)

(defcustom scala-python3-command-args '("-i")
  "String arguments to be used when starting a Python3 shell."
  :type '(repeat string)
   :tag "ar-scala-scala-python3-command-args"
  :group 'ar-scala-scala-mode)

(defcustom scala-ipython-command
  (if (eq system-type 'windows-nt)
    "C:\\Python27\\python"
    ;; "C:/Python33/Lib/site-packages/IPython"
    ;; "/usr/bin/ipython"
    "ipython")

  "A PATH/TO/EXECUTABLE or default value.

`M-x IPython RET' may look for,
Unless IPython-shell is specified by command.

On Windows default is \"C:\\\\Python27\\\\python.exe\"
While with Anaconda for example the following works here:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\ipython.exe\"

Else /usr/bin/ipython"

  :type 'string
   :tag "ar-scala-scala-ipython-command
"
  :group 'ar-scala-scala-mode)

(defcustom scala-ipython-command-args
  (if (eq system-type 'windows-nt)
      '("-i" "C:\\Python27\\Scripts\\ipython-script.py")
    ;; --simple-prompt seems to exist from IPython 5.
    (if (string-match "^[0-4]" (ignore-errors (shell-command-to-string (concat "ipython" " -V"))))
	'("--pylab" "--automagic")
      '("--pylab" "--automagic" "--simple-prompt")))
  "String arguments to be used when starting a IPython shell.

At Windows make sure ipython-script.py is PATH.
Also setting PATH/TO/SCRIPT here should work, for example;
C:\\Python27\\Scripts\\ipython-script.py
With Anaconda the following is known to work:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\ipython-script-py\""
  :type '(repeat string)
   :tag "ar-scala-scala-ipython-command-args"
  :group 'ar-scala-scala-mode)

(defcustom scala-jython-command
  (if (eq system-type 'windows-nt)
      '("jython")
    '("/usr/bin/jython"))

  "A PATH/TO/EXECUTABLE or default value.
`M-x Jython RET' may look for, if no Jython-shell is specified by command.

Not known to work at windows
Default /usr/bin/jython"

  :type '(repeat string)
   :tag "ar-scala-scala-jython-command
"
  :group 'ar-scala-scala-mode)

(defcustom scala-jython-command-args '("-i")
  "String arguments to be used when starting a Jython shell."
  :type '(repeat string)
   :tag "ar-scala-scala-jython-command-args"
  :group 'ar-scala-scala-mode)

(defcustom scala-shell-toggle-1 scala-python2-command
  "A PATH/TO/EXECUTABLE or default value used by ‘ar-toggle-shell’."
  :type 'string
   :tag "ar-scala-scala-scala-shell-toggle-1"
  :group 'ar-scala-scala-mode)

(defcustom scala-shell-toggle-2 scala-python3-command
  "A PATH/TO/EXECUTABLE or default value used by ‘ar-toggle-shell’."
  :type 'string
   :tag "ar-scala-scala-scala-shell-toggle-2"
  :group 'ar-scala-scala-mode)

(defcustom scala--imenu-create-index-p nil
  "Non-nil means Python mode creates and displays an index menu.

Of functions and global variables."
  :type 'boolean
   :tag "ar-scala-scala-scala--imenu-create-index-p"
  :group 'ar-scala-scala-mode)

(defvar scala-history-filter-regexp "\\‘\\s-*\\S-?\\S-?\\s-*\\'\\|''’/tmp/"
  "Input matching this regexp is not saved on the history list.
Default ignores all inputs of 0, 1, or 2 non-blank characters.")

(defcustom scala-match-paren-mode nil
  "Non-nil means, cursor will jump to beginning or end of a block.
This vice versa, to beginning first.
Sets ‘ar-match-paren-key’ in ‘python-mode-map’.
Customize ‘ar-match-paren-key’ which key to use."
  :type 'boolean
   :tag "ar-scala-scala-match-paren-mode"
  :group 'ar-scala-scala-mode)

(defcustom scala-match-paren-key "%"
  "String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands will not get confused (i.e., the string
should be of the form ‘#x...’ where ‘x’ is not a blank or a tab, and
                               ‘...’ is arbitrary).
However, this string should not end in whitespace."
  :type 'string
   :tag "ar-scala-scala-match-paren-key"
  :group 'ar-scala-scala-mode)

(defcustom scala-kill-empty-line t
  "If t, ‘ar-indent-forward-line’ kills empty lines."
  :type 'boolean
   :tag "ar-scala-scala-kill-empty-line"
  :group 'ar-scala-scala-mode)

(defcustom scala-imenu-show-method-args-p nil
  "Controls echoing of arguments of functions & methods in the Imenu buffer.
When non-nil, arguments are printed."
  :type 'boolean
   :tag "ar-scala-scala-imenu-show-method-args-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-use-local-default nil
  "If t, ‘ar-shell’ will use ‘ar-scala-shell-local-path’.

Alternative to default Python.

Making switch between several virtualenv's easier,‘python-mode’ should
deliver an installer, named-shells pointing to virtualenv's will be available."
  :type 'boolean
   :tag "ar-scala-scala-use-local-default"
  :group 'ar-scala-scala-mode)

(defcustom scala-edit-only-p nil
  "Do not check for installed Python executables.

Default is nil.

See bug report at launchpad, lp:944093."
  :type 'boolean
   :tag "ar-scala-scala-edit-only-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-force-ar-scala-shell-name-p nil
  "When t, execution specified in ‘ar-scala-shell-name’ is enforced.

Possibly shebang does not take precedence."

  :type 'boolean
   :tag "ar-scala-scala-force-ar-scala-shell-name-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-mode-v5-behavior-p nil
  "Execute region through ‘shell-command-on-region’.

As v5 did it - lp:990079.
This might fail with certain chars - see UnicodeEncodeError lp:550661"

  :type 'boolean
  :tag "python-mode-v5-behavior-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-trailing-whitespace-smart-delete-p nil
  "Default is nil.

When t, ‘python-mode’ calls
\(add-hook \\='before-save-hook \\='delete-trailing-whitespace nil \\='local)

Also commands may delete trailing whitespace by the way.
When editing other peoples code, this may produce a larger diff than expected"
  :type 'boolean
   :tag "ar-scala-scala-trailing-whitespace-smart-delete-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-newline-delete-trailing-whitespace-p t
  "Delete trailing whitespace maybe left by ‘ar-newline-and-indent’.

Default is t. See lp:1100892"
  :type 'boolean
   :tag "ar-scala-scala-newline-delete-trailing-whitespace-p"
  :group 'ar-scala-scala-mode)

(defcustom scala--warn-tmp-files-left-p nil
  "Warn, when ‘ar-temp-directory’ contains files susceptible being left.

WRT previous Python-mode sessions. See also lp:987534."
  :type 'boolean
   :tag "ar-scala-scala-scala--warn-tmp-files-left-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-complete-ac-sources '(ac-source-pycomplete)
  "List of ‘auto-complete’ sources assigned to ‘ac-sources’.

In ‘ar-complete-initialize’.

Default is known to work an Ubuntu 14.10 - having scala-
mode, pymacs and auto-complete-el, with the following minimal
Emacs initialization:

\(require \\='pymacs)
\(require \\='auto-complete-config)
\(ac-config-default)"
  :type 'hook
   :tag "ar-scala-scala-complete-ac-sources"
  :options '(ac-source-pycomplete ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers)
  :group 'ar-scala-scala-mode)

(defcustom scala-remove-cwd-from-path t
  "Whether to allow loading of Python modules from the current directory.
If this is non-nil, Emacs removes '' from sys.path when starting
a Python process.  This is the default, for security
reasons, as it is easy for the Python process to be started
without the user's realization (e.g. to perform completion)."
  :type 'boolean
   :tag "ar-scala-scala-remove-cwd-from-path"
  :group 'ar-scala-scala-mode)

(defcustom scala-shell-local-path ""
  "‘ar-shell’ will use EXECUTABLE indicated here incl. path.

If ‘ar-use-local-default’ is non-nil."

  :type 'string
   :tag "ar-scala-scala-scala-shell-local-path"
  :group 'ar-scala-scala-mode)

(defcustom scala-python-edit-version "python3"
  "Default is \"python3\".

When empty, version is guessed via ‘ar-choose-shell’."

  :type 'string
   :tag "ar-scala-scala-python-edit-version"
  :group 'ar-scala-scala-mode)

(defcustom scala-ipython-execute-delay 0.3
  "Delay needed by execute functions when no IPython shell is running."
  :type 'float
   :tag "ar-scala-scala-ipython-execute-delay"
  :group 'ar-scala-scala-mode)

(defvar scala-shell-completion-setup-code
  "try:
    import readline
except ImportError:
    def __COMPLETER_all_completions(text): []
else:
    import rlcompleter
    readline.set_completer(rlcompleter.Completer().complete)
    def __COMPLETER_all_completions(text):
        import sys
        completions = []
        try:
            i = 0
            while True:
                res = readline.get_completer()(text, i)
                if not res: break
                i += 1
                completions.append(res)
        except NameError:
            pass
        return completions"
  "Code used to setup completion in Python processes.")

(defvar scala-shell-module-completion-code "';'.join(__COMPLETER_all_completions('''%s'''))"
  "Python code used to get completions separated by semicolons for imports.")

(defvar scala-ipython-module-completion-code
  "import IPython
version = IPython.__version__
if \'0.10\' < version:
    from IPython.core.completerlib import module_completion
"
  "For IPython v0.11 or greater.
Use the following as the value of this variable:

';'.join(module_completion('''%s'''))")

(defvar scala-ipython-module-completion-string
  "';'.join(module_completion('''%s'''))"
  "See also ‘ar-ipython-module-completion-code’.")

(defcustom scala--imenu-create-index-function 'ar-scala--imenu-index
  "Switch between ‘ar-scala--imenu-create-index-new’  and series 5. index-machine."
  :type '(choice
	  (const :tag "'ar-scala--imenu-create-index-new, also lists modules variables " scala--imenu-create-index-new)

	  (const  :tag "ar-scala-scala-scala--imenu-create-index, series 5. index-machine" scala--imenu-create-index)
	  (const  :tag "ar-scala-scala-scala--imenu-index, honor type annotations" scala--imenu-index)

	  )
   :tag "ar-scala-scala-scala--imenu-create-index-function"
  :group 'ar-scala-scala-mode)

(defvar scala-line-re "^"
  "Used by generated functions." )

(defvar scala-input-filter-re "\\‘\\s-*\\S-?\\S-?\\s-*\\’"
  "Input matching this regexp is not saved on the history list.
Default ignores all inputs of 0, 1, or 2 non-blank characters.")

(defvar strip-chars-before  "\\`[ \t\r\n]*"
  "Regexp indicating which chars shall be stripped before STRING.

See also ‘string-chars-preserve’")

(defvar strip-chars-after  "[ \t\r\n]*\\'"
  "Regexp indicating which chars shall be stripped after STRING.

See also ‘string-chars-preserve’")

(defcustom scala-docstring-style 'pep-257-nn
  "Implemented styles:

 are DJANGO, ONETWO, PEP-257, PEP-257-NN,SYMMETRIC, and NIL.

A value of NIL wo not care about quotes
position and will treat docstrings a normal string, any other
value may result in one of the following docstring styles:

DJANGO:

    \"\"\"
    Process foo, return bar.
    \"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

ONETWO:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

PEP-257:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

PEP-257-NN:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

SYMMETRIC:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\""
  :type '(choice

          (const :tag "Do not format docstrings" nil)
          (const :tag "Django's coding standards style." django)
          (const :tag "One newline and start and Two at end style." onetwo)
          (const :tag "PEP-257 with 2 newlines at end of string." pep-257)
          (const :tag "PEP-257-nn with 1 newline at end of string." pep-257-nn)
          (const :tag "Symmetric style." symmetric))
   :tag "ar-scala-scala-docstring-style"
  :group 'ar-scala-scala-mode)

(defcustom scala-execute-directory nil
  "Stores the file's default directory-name scala-execute-... functions act upon.

Used by Python-shell for output of ‘ar-execute-buffer’ and related commands.
See also ‘ar-use-current-dir-when-execute-p’"
  :type 'string
   :tag "ar-scala-scala-execute-directory"
  :group 'ar-scala-scala-mode)

(defcustom scala-use-current-dir-when-execute-p t
  "Current directory used for output.

See also ‘ar-execute-directory’"
  :type 'boolean
   :tag "ar-scala-scala-use-current-dir-when-execute-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-keep-shell-dir-when-execute-p nil
  "Do not change Python shell's current working directory when sending code.

See also ‘ar-execute-directory’"
  :type 'boolean
   :tag "ar-scala-scala-keep-shell-dir-when-execute-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-fileless-buffer-use-default-directory-p t
  "‘default-directory’ sets current working directory of Python output shell.

When ‘ar-use-current-dir-when-execute-p’ is non-nil and no buffer-file exists."
  :type 'boolean
   :tag "ar-scala-scala-fileless-buffer-use-default-directory-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-check-command "pychecker --stdlib"
  "Command used to check a Python file."
  :type 'string
   :tag "ar-scala-scala-check-command"
  :group 'ar-scala-scala-mode)

;; (defvar scala-this-abbrevs-changed nil
;;   "Internally used by ‘python-mode-hook’.")

(defvar scala-buffer-name nil
  "Internal use.

The buffer last output was sent to.")

(defvar scala-orig-buffer-or-file nil
  "Internal use.")

(defcustom scala-keep-windows-configuration nil
  "Takes precedence over:

 ‘ar-split-window-on-execute’ and ‘ar-switch-buffers-on-execute-p’.
See lp:1239498

To suppres window-changes due to error-signaling also.
Set ‘ar-keep-windows-configuration’ onto \\'force

Default is nil"

  :type '(choice
          (const :tag "nil" nil)
          (const :tag "t" t)
          (const :tag "force" force))
   :tag "ar-scala-scala-keep-windows-configuration"
  :group 'ar-scala-scala-mode)

(defvar scala-output-buffer ""
      "Used if ‘python-mode-v5-behavior-p’ is t.

Otherwise output buffer is created dynamically according to version process.")

(defcustom scala-force-default-output-buffer-p nil
  "Enforce sending output to the default output ‘buffer-name’.

Set by defvar ‘ar-output-buffer’
Bug #31 - wrong fontification caused by string-delimiters in output"

  :type 'boolean
   :tag "ar-scala-scala-force-default-output-buffer-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-shell-unbuffered t
  "Should shell output be unbuffered?.
When non-nil, this may prevent delayed and missing output in the
Python shell.  See commentary for details."
  :type 'boolean
  :safe 'booleanp
   :tag "ar-scala-scala-scala-shell-unbuffered"
  :group 'ar-scala-scala-mode)

(defcustom scala-shell-process-environment nil
  "List of overridden environment variables for subprocesses to inherit.
Each element should be a string of the form ENVVARNAME=VALUE.
When this variable is non-nil, values are exported into the
process environment before starting it.  Any variables already
present in the current environment are superseded by variables
set here."
  :type '(repeat string)
   :tag "ar-scala-scala-scala-shell-process-environment"
  :group 'ar-scala-scala-mode)

(defcustom scala-shell-extra-pythonpaths nil
  "List of extra pythonpaths for Python shell.
When this variable is non-nil, values added at the beginning of
the PYTHONPATH before starting processes.  Any values present
here that already exists in PYTHONPATH are moved to the beginning
of the list so that they are prioritized when looking for
modules."
  :type '(repeat string)
   :tag "ar-scala-scala-scala-shell-extra-pythonpaths"
  :group 'ar-scala-scala-mode)

(defcustom scala-shell-exec-path nil
  "List of paths for searching executables.
When this variable is non-nil, values added at the beginning of
the PATH before starting processes.  Any values present here that
already exists in PATH are moved to the beginning of the list so
that they are prioritized when looking for executables."
  :type '(repeat string)
   :tag "ar-scala-scala-scala-shell-exec-path"
  :group 'ar-scala-scala-mode)

(defcustom scala-shell-remote-exec-path nil
  "List of paths to be ensured remotely for searching executables.
When this variable is non-nil, values are exported into remote
hosts PATH before starting processes.  Values defined in
‘ar-scala-shell-exec-path’ will take precedence to paths defined
here.  Normally you wont use this variable directly unless you
plan to ensure a particular set of paths to all Python shell
executed through tramp connections."
  :version "25.1"
  :type '(repeat string)
   :tag "ar-scala-scala-scala-shell-remote-exec-path"
  :group 'ar-scala-scala-mode)

(defcustom scala-shell-virtualenv-root nil
  "Path to virtualenv root.
This variable, when set to a string, makes the environment to be
modified such that shells are started within the specified
virtualenv."
  :type '(choice (const nil) string)
   :tag "ar-scala-scala-scala-shell-virtualenv-root"
  :group 'ar-scala-scala-mode)

(defcustom scala-start-in-virtualenv-p nil
  "When ‘ar-scala-shell-virtualenv-root’ is set, Emacs should start there."
  :type 'boolean
   :tag "ar-scala-scala-start-in-virtualenv-p"
  :group 'ar-scala-scala-mode)

(defvar scala-shell-completion-native-redirect-buffer
  " *Py completions redirect*"
  "Buffer to be used to redirect output of readline commands.")

(defvar scala-shell--block-prompt nil
  "Input block prompt for inferior python shell.
Do not set this variable directly, instead use
‘ar-scala-shell-prompt-set-calculated-regexps’.")

(defvar scala-shell-output-filter-in-progress nil)
(defvar scala-shell-output-filter-buffer nil)

(defvar scala-shell--prompt-calculated-input-regexp nil
  "Calculated input prompt regexp for inferior python shell.
Do not set this variable directly.

Iff ‘ar-scala-shell--prompt-calculated-input-regexp’
or ‘ar-scala-shell--prompt-calculated-output-regexp’ are set
‘ar-scala-shell-prompt-set-calculated-regexps’ is not run.")

(defvar scala-shell--prompt-calculated-output-regexp nil
  "Calculated output prompt regexp for inferior python shell.

‘ar-scala-shell-prompt-set-calculated-regexps’
Do not set this variable directly.

Iff ‘ar-scala-shell--prompt-calculated-input-regexp’
or ‘ar-scala-shell--prompt-calculated-output-regexp’ are set
‘ar-scala-shell-prompt-set-calculated-regexps’ is not run.")

(defvar scala-shell-prompt-output-regexp ""
  "See ‘ar-scala-shell-prompt-output-regexps’.")

(defvar scala-shell-prompt-output-regexps
  '(""                                  ; Python
    "Out\\[[0-9]+\\]: "                 ; IPython
    "Out :")                            ; ipdb safeguard
  "List of regular expressions matching output prompts.")

(defvar scala-underscore-word-syntax-p t
  "This is set later by defcustom, only initial value here.

If underscore chars should be of ‘syntax-class’ ‘word’, not of ‘symbol’.
Underscores in word-class makes ‘forward-word’.
Travels the indentifiers. Default is t.
See also command ‘ar-toggle-underscore-word-syntax-p’")

(defvar scala-autofill-timer nil)
(defvar scala-fill-column-orig fill-column
  "Used to reset fill-column")

;; defvared value is not updated maybe
(defvar scala-mode-message-string
  (if (or (string= "python-mode.el" (buffer-name))
	  (ignore-errors (string-match "python-mode.el" (scala--buffer-filename-remote-maybe))))
      "python-mode.el"
    "ar-scala-mode")
  "Internally used. Reports the ‘python-mode’ branch.")

;; defvared value is not updated maybe
(setq scala-mode-message-string
  (if (or (string= "python-mode.el" (buffer-name))
	  (ignore-errors (string-match "python-mode.el" (scala--buffer-filename-remote-maybe))))
      "python-mode.el"
    "ar-scala-mode"))

(defvar scala-mode-syntax-table nil
  "Give punctuation syntax to ASCII that normally has symbol.

Syntax or has word syntax and is not a letter.")

(setq scala-mode-syntax-table
      (let ((table (make-syntax-table)))
        ;; Give punctuation syntax to ASCII that normally has symbol
        ;; syntax or has word syntax and is not a letter.
        (let ((symbol (string-to-syntax "_"))
              (sst (standard-syntax-table)))
          (dotimes (i 128)
            (unless (= i ?_)
              (if (equal symbol (aref sst i))
                  (modify-syntax-entry i "." table)))))
        (modify-syntax-entry ?$ "." table)
        (modify-syntax-entry ?% "." table)
        ;; exceptions
        (modify-syntax-entry ?# "<" table)
        (modify-syntax-entry ?\n ">" table)
        (modify-syntax-entry ?' "\"" table)
        (modify-syntax-entry ?` "$" table)
        (if scala-underscore-word-syntax-p
            (modify-syntax-entry ?\_ "w" table)
          (modify-syntax-entry ?\_ "_" table))
        table))

(defvar scala-shell-mode-syntax-table nil
  "Set from scala-shell")

(defvar scala-ipython-completion-command-string nil
  "Select command according to IPython version.

Either ‘ar-ipython0.10-completion-command-string’
or ‘ar-ipython0.11-completion-command-string’.

‘ar-ipython0.11-completion-command-string’ also covers version 0.12")

(defvar scala-ipython0.10-completion-command-string
  "print(';'.join(__IP.Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n"
  "The string send to ipython to query for all possible completions.")

(defvar scala-ipython0.11-completion-command-string
  "print(';'.join(get_ipython().Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n"
  "The string send to ipython to query for all possible completions.")

(defvar scala-encoding-string-re "^[ \t]*#[ \t]*-\\*-[ \t]*coding:.+-\\*-"
  "Matches encoding string of a Python file.")

(defvar scala-shebang-regexp "#![ \t]?\\([^ \t\n]+\\)[ \t]*\\([biptj]+ython[^ \t\n]*\\)"
  "Detecting the shell in head of file.")

(defvar scala-temp-directory
  (let ((ok #'(lambda (x)
               (and x
                    (setq x (expand-file-name x)) ; always true
                    (file-directory-p x)
                    (file-writable-p x)
                    x)))
        erg)
    (or
     (and (not (string= "" scala-custom-temp-directory))
          (if (funcall ok scala-custom-temp-directory)
              (setq erg (expand-file-name scala-custom-temp-directory))
            (if (file-directory-p (expand-file-name scala-custom-temp-directory))
                (error "Py-custom-temp-directory set but not writable")
              (error "Py-custom-temp-directory not an existing directory"))))
     (and (funcall ok (getenv "TMPDIR"))
          (setq erg (getenv "TMPDIR")))
     (and (funcall ok (getenv "TEMP/TMP"))
          (setq erg (getenv "TEMP/TMP")))
     (and (funcall ok "/usr/tmp")
          (setq erg "/usr/tmp"))
     (and (funcall ok "/tmp")
          (setq erg "/tmp"))
     (and (funcall ok "/var/tmp")
          (setq erg "/var/tmp"))
     (and (eq system-type 'darwin)
          (funcall ok "/var/folders")
          (setq erg "/var/folders"))
     (and (or (eq system-type 'ms-dos)(eq system-type 'windows-nt))
          (funcall ok (concat "c:" scala-separator-char "Users"))
          (setq erg (concat "c:" scala-separator-char "Users")))
     ;; (funcall ok ".")
     (error
      "Could not find a usable temp directory -- set ‘ar-temp-directory’"))
    (when erg (setq scala-temp-directory erg)))
  "Directory used for temporary files created by a *Python* process.
By default, guesses the first directory from this list that exists and that you
can write into: the value (if any) of the environment variable TMPDIR,
/usr/tmp, /tmp, /var/tmp, or the current directory.

 ‘ar-custom-temp-directory’ will take precedence when setq")

(defvar scala-exec-command nil
  "Internally used.")

(defvar scala-which-bufname "Python")

(defvar scala-pychecker-history nil)

(defvar scala-pyflakes3-history nil)

(defvar scala-pep8-history nil)

(defvar scala-pyflakespep8-history nil)

(defvar scala-pylint-history nil)

(defvar scala-mode-output-map nil
  "Keymap used in *Python Output* buffers.")

(defvar hs-hide-comments-when-hiding-all t
  "Defined in hideshow.el, silence compiler warnings here.")

(defvar scala-shell-complete-debug nil
  "For interal use when debugging, stores completions." )

(defvar scala-debug-p nil
  "Activate extra code for analysis and test purpose when non-nil.

Temporary files are not deleted. Other functions might implement
some logging, etc.
For normal operation, leave it set to nil, its default.
Defined with a defvar form to allow testing the loading of new versions.")

(defcustom scala-shell-complete-p nil
  "Enable native completion."

  :type 'boolean
   :tag "ar-scala-scala-scala-shell-complete-p"
  :group 'ar-scala-scala-mode)
(make-variable-buffer-local 'ar-scala-shell-complete-p)

(defcustom scala-section-start "# {{"
  "Delimit arbitrary chunks of code."
  :type 'string
   :tag "ar-scala-scala-section-start"
  :group 'ar-scala-scala-mode)

(defcustom scala-section-end "# }}"
  "Delimit arbitrary chunks of code."
  :type 'string
   :tag "ar-scala-scala-section-end"
  :group 'ar-scala-scala-mode)

(defvar scala-section-re scala-section-start)

(defvar scala-last-window-configuration nil
  "Internal use.

Restore ‘ar-restore-window-configuration’.")

(defvar scala-exception-buffer nil
  "Will be set internally.

Remember source buffer where error might occur.")

(defvar scala-string-delim-re "\\(\"\"\"\\|'''\\|\"\\|'\\)"
  "When looking at beginning of string.")

(defvar scala-star-labelled-re "[ \\t]*[\\*-] +[[:graph:]]"
  "When looking at a star label.")

(defvar scala-colon-labelled-re "[ \\t]*[[:graph:]]* *: *[[:graph:]]+"
  "When looking at a colon label.")
;; (setq scala-colon-labelled-re "[ \\t]*[[:graph:]]* *: *[[:graph:]]+\\|[ \\t]*[\\*-] +[[:graph:]]")

(defvar scala-labelled-re (concat scala-colon-labelled-re "\\|" scala-star-labelled-re)
  "When looking at label.")

;; "[ \t]+\\c.+"
(defvar scala-symbol-re "[ \t]*\\c.+[ \t]*$"
  "Matching lines only containing symbols.")
(setq scala-symbol-re "[ \t]*\\c.+[ \t]*")

(defvar scala-expression-skip-regexp "[^ (=:#\t\r\n\f]"
  "Expression possibly composing a ‘ar-expression’.")

(defvar scala-expression-skip-chars "^ (=#\t\r\n\f"
  "Chars composing a ‘ar-expression’.")

(setq scala-expression-skip-chars "^ [{(=#\t\r\n\f")

(defvar scala-expression-re "[^ =#\t\r\n\f]+"
  "Expression possibly composing a ‘ar-expression’.")

(defcustom scala-paragraph-re paragraph-start
  "Allow Python specific ‘paragraph-start’ var."
  :type 'string
   :tag "ar-scala-scala-paragraph-re"
  :group 'ar-scala-scala-mode)

(defvar scala-not-expression-regexp "[ .=#\t\r\n\f)]+"
  "Regexp indicated probably will not compose a ‘ar-expression’.")

(defvar scala-not-expression-chars " #\t\r\n\f"
  "Chars indicated probably will not compose a ‘ar-expression’.")

;; (defvar scala-partial-expression-stop-backward-chars "^] .=,\"'()[{}:#\t\r\n\f"
(defvar scala-partial-expression-stop-backward-chars "^] .=,\"'()[{}:#\t\r\n\f"
    "Chars indicated which not possibly compose a ‘ar-partial-expression’,
stop at it.")
;; (setq scala-partial-expression-stop-backward-chars "^] .=,\"'()[{}:#\t\r\n\f")

(defvar scala-partial-expression-forward-chars "^ .\"')}]:#\t\r\n\f")
;; (setq scala-partial-expression-forward-chars "^ .\"')}]:#\t\r\n\f")

(defvar scala-partial-expression-re (concat "[" scala-partial-expression-stop-backward-chars (substring scala-partial-expression-forward-chars 1) "]+"))
(setq scala-partial-expression-re (concat "[" scala-partial-expression-stop-backward-chars "]+"))

(defvar scala-statement-re scala-partial-expression-re)
(defvar scala-indent-re ".+"
  "This var is introduced for regularity only.")
(setq scala-indent-re ".+")

(defvar scala-operator-re "[ \t]*\\(\\.\\|+\\|-\\|*\\|//\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!=\\|=\\)[ \t]*"
  "Matches most of Python syntactical meaningful characters.

See also ‘ar-assignment-re’")

;; (setq scala-operator-re "[ \t]*\\(\\.\\|+\\|-\\|*\\|//\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!=\\|=\\)[ \t]*")

(defvar scala-delimiter-re "\\(\\.[[:alnum:]]\\|,\\|;\\|:\\)[ \t\n]"
  "Delimiting elements of lists or other programming constructs.")

(defvar scala-line-number-offset 0
  "When an exception occurs as a result of ‘ar-execute-region’.

A subsequent ‘ar-scala-up-exception’ needs the line number where the region
started, in order to jump to the correct file line.
This variable is set in ‘ar-execute-region’ and used in ‘ar-scala--jump-to-exception’.")

(defvar scala-match-paren-no-use-syntax-pps nil)

(defvar scala-traceback-line-re
  "[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)"
  "Regular expression that describes tracebacks.")

(defvar scala-XXX-tag-face 'ar-scala-XXX-tag-face)

(defvar scala-pseudo-keyword-face 'ar-scala-pseudo-keyword-face)

(defface scala-variable-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face method decorators."
   :tag "ar-scala-scala-variable-name-face"
  :group 'ar-scala-scala-mode)

(defvar scala-variable-name-face 'ar-scala-variable-name-face)
(setq scala-variable-name-face 'ar-scala-variable-name-face)

(defvar scala-number-face 'ar-scala-number-face)

(defvar scala-decorators-face 'ar-scala-decorators-face)

(defvar scala-object-reference-face 'ar-scala-object-reference-face)

(defvar scala-builtins-face 'ar-scala-builtins-face)

(defvar scala-class-name-face 'ar-scala-class-name-face)

(defvar scala-def-face 'ar-scala-def-face)

(defvar scala-exception-name-face 'ar-scala-exception-name-face)

(defvar scala-import-from-face 'ar-scala-import-from-face)

(defvar scala-def-class-face 'ar-scala-def-class-face)

(defvar scala-try-if-face 'ar-scala-try-if-face)

(defvar scala-file-queue nil
  "Queue of Python temp files awaiting execution.
Currently-active file is at the head of the list.")

(defvar jython-mode-hook nil
  "Hook called by ‘jython-mode’.
‘jython-mode’ also calls ‘python-mode-hook’.")

(defvar scala-shell-hook nil
  "Hook called by ‘ar-shell’.")

;; (defvar scala-font-lock-keywords nil)

(defvar scala-dotted-expression-syntax-table
  (let ((table (make-syntax-table scala-mode-syntax-table)))
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?."_" table)
    table)
  "Syntax table used to identify Python dotted expressions.")

(defvar scala-default-template "if"
  "Default template to expand by ‘python-expand-template’.
Updated on each expansion.")

(defvar-local scala-already-guessed-indent-offset nil
  "Internal use by ‘ar-indent-line’.

When ‘this-command’ is ‘eq’ to ‘last-command’, use the guess already computed.")

(defvar scala-shell-template "
\(defun NAME (&optional argprompt)
  \"Start an DOCNAME interpreter in another window.

With optional \\\\[universal-argument] user is prompted
for options to pass to the DOCNAME interpreter. \"
  (interactive \"P\")
  (let\* ((scala-shell-name \"FULLNAME\"))
    (scala-shell argprompt)
    (when (called-interactively-p 'interactive)
      (switch-to-buffer (current-buffer))
      (goto-char (point-max)))))
")

;; Constants
(defconst scala-block-closing-keywords-re
  "[ \t]*\\_<\\(return\\|raise\\|break\\|continue\\|pass\\)\\_>[ \n\t]*"
  "Matches the beginning of a class, method or compound statement.")

(setq scala-block-closing-keywords-re
  "[ \t]*\\_<\\(return\\|raise\\|break\\|continue\\|pass\\)\\_>[ \n\t]*")

(defconst scala-finally-re
  "[ \t]*\\_<finally:"
  "Regular expression matching keyword which closes a try-block.")

(defconst scala-except-re "[ \t]*\\_<except\\_>"
  "Matches the beginning of a ‘except’ block.")

;; (defconst scala-except-re
;;   "[ \t]*\\_<except\\_>[:( \n\t]*"
;;   "Regular expression matching keyword which composes a try-block.")

(defconst scala-return-re
  ".*:?[ \t]*\\_<\\(return\\)\\_>[ \n\t]*"
  "Regular expression matching keyword which typically closes a function.")

(defconst scala-decorator-re
  "[ \t]*@[^ ]+\\_>[ \n\t]*"
  "Regular expression matching keyword which typically closes a function.")

(defcustom scala-outdent-re-raw
  (list
   "case"
   "elif"
   "else"
   "except"
   "finally"
   )
  "Used by ‘ar-outdent-re’."
  :type '(repeat string)
   :tag "ar-scala-scala-outdent-re-raw"
  :group 'ar-scala-scala-mode
  )

(defconst scala-outdent-re
  (concat
   "[ \t]*"
   (regexp-opt scala-outdent-re-raw 'symbols)
   "[)\t]*")
  "Regular expression matching statements to be dedented one level.")

(defcustom scala-no-outdent-re-raw
  (list
   "break"
   "continue"
   "import"
   "pass"
   "raise"
   "return")
  "Uused by ‘ar-no-outdent-re’."
  :type '(repeat string)
   :tag "ar-scala-scala-no-outdent-re-raw"
  :group 'ar-scala-scala-mode)

(defconst scala-no-outdent-re
  (concat
   "[ \t]*"
   (regexp-opt scala-no-outdent-re-raw 'symbols)
   "[)\t]*$")
"Regular expression matching lines not to augment indent after.

See ‘ar-no-outdent-re-raw’ for better readable content")

(defconst scala-assignment-re "\\(\\_<\\w+\\_>[[:alnum:]:, \t]*[ \t]*\\)\\(=\\|+=\\|*=\\|%=\\|&=\\|^=\\|<<=\\|-=\\|/=\\|**=\\||=\\|>>=\\|//=\\)\\(.*\\)"
  "If looking at the beginning of an assignment.")

;; 'name':
(defconst scala-dict-re "'\\_<\\w+\\_>':")

(defcustom scala-block-re-raw
  (list
   "async def"
   "async for"
   "async with"
   "class"
   "def"
   "for"
   "if"
   "match"
   "try"
   "while"
   "with"
   )
  "Matches the beginning of a compound statement but not its clause."
  :type '(repeat string)
   :tag "ar-scala-scala-block-re-raw"
  :group 'ar-scala-scala-mode)

(defconst scala-block-re (concat
		       ;; "[ \t]*"
		       (regexp-opt scala-block-re-raw 'symbols)
		       ".*[:( \n\t]"
		       )
  "Matches the beginning of a compound statement.")

(defconst scala-minor-block-re-raw (list
				      "async for"
				      "async with"
                                      "case"
				      "except"
				      "for"
				      "if"
                                      "match"
				      "try"
				      "with"
				      )
  "Matches the beginning of an case ‘for’, ‘if’, ‘try’, ‘except’ or ‘with’ block.")

(defconst scala-minor-block-re
  (concat
   "[ \t]*"
   (regexp-opt scala-minor-block-re-raw 'symbols)
   "[:( \n\t]")

  "Regular expression matching lines not to augment indent after.

See ‘ar-minor-block-re-raw’ for better readable content")

(defconst scala-try-re "[ \t]*\\_<try\\_>[: \n\t]"
  "Matches the beginning of a ‘try’ block.")

(defconst scala-case-re "[ \t]*\\_<case\\_>[: \t][^:]*:"
  "Matches a ‘case’ clause.")

(defconst scala-match-case-re "[ \t]*\\_<match\\|case\\_>[: \t][^:]*:"
  "Matches a ‘match case’ clause.")

(defconst scala-for-re "[ \t]*\\_<\\(async for\\|for\\)\\_> +[[:alpha:]_][[:alnum:]_]* +in +[[:alpha:]_][[:alnum:]_()]* *[: \n\t]"
  "Matches the beginning of a ‘try’ block.")

(defconst scala-if-re "[ \t]*\\_<if\\_>[ (]+"
  "Matches the beginning of an ‘if’ block.")

(defconst scala-else-re "[ \t]*\\_<else:"
  "Matches the beginning of an ‘else’ block.")

(setq scala-else-re "else")

(defconst scala-elif-re "[ \t]*\\_<\\elif\\_>[( \n\t]"
  "Matches the beginning of a compound if-statement's clause exclusively.")

;; (defconst scala-elif-block-re "[ \t]*\\_<elif\\_> +[[:alpha:]_][[:alnum:]_]* *[: \n\t]"
;;   "Matches the beginning of an ‘elif’ block.")

(defconst scala-class-re "[ \t]*\\_<\\(class\\)\\_>[ \n\t]"
  "Matches the beginning of a class definition.")

(defconst scala-def-or-class-re "[ \t]*\\_<\\(async def\\|class\\|def\\)\\_>[ \n\t]+\\([[:alnum:]_]*\\)"
  "Matches the beginning of a class- or functions definition.

Second group grabs the name")

;; (setq scala-def-or-class-re "[ \t]*\\_<\\(async def\\|class\\|def\\)\\_>[ \n\t]")

;; (defconst scala-def-re "[ \t]*\\_<\\(async def\\|def\\)\\_>[ \n\t]"
(defconst scala-def-re "[ \t]*\\_<\\(def\\|async def\\)\\_>[ \n\t]"
  "Matches the beginning of a functions definition.")

(defcustom scala-block-or-clause-re-raw
  (list
   "async for"
   "async with"
   "async def"
   "async class"
   "class"
   "def"
   "elif"
   "else"
   "except"
   "finally"
   "for"
   "if"
   "try"
   "while"
   "with"
   "match"
   "case"
   )
  "Matches the beginning of a compound statement or its clause."
  :type '(repeat string)
   :tag "ar-scala-scala-block-or-clause-re-raw"
  :group 'ar-scala-scala-mode)

(defvar scala-block-or-clause-re
  (concat
   "[ \t]*"
   (regexp-opt  scala-block-or-clause-re-raw 'symbols)
   "[( \t]*.*:?")
  "See ‘ar-block-or-clause-re-raw’, which it reads.")

(defcustom scala-extended-block-or-clause-re-raw
  (list
   "async def"
   "async for"
   "async with"
   "class"
   "def"
   "elif"
   "else"
   "except"
   "finally"
   "for"
   "if"
   "try"
   "while"
   "with"
   "match"
   "case"
   )
  "Matches the beginning of a compound statement or its clause."
  :type '(repeat string)
   :tag "ar-scala-scala-extended-block-or-clause-re-raw"
  :group 'ar-scala-scala-mode)

(defconst scala-extended-block-or-clause-re
  (concat
   "[ \t]*"
   (regexp-opt  scala-extended-block-or-clause-re-raw 'symbols)
   "[( \t:]+")
  "See ‘ar-block-or-clause-re-raw’, which it reads.")

(defconst scala-clause-re scala-extended-block-or-clause-re
  "See also scala-minor-clause re.")

(defcustom scala-minor-clause-re-raw
  (list
   "case"
   "elif"
   "else"
   "except"
   "finally"
   )
  "Matches the beginning of a clause."
    :type '(repeat string)
     :tag "ar-scala-scala-minor-clause-re-raw"
    :group 'ar-scala-scala-mode)

(defconst scala-minor-clause-re
  (concat
   "[ \t]*"
   (regexp-opt  scala-minor-clause-re-raw 'symbols)
   "[( \t]*.*:")
  "See ‘ar-minor-clause-re-raw’, which it reads.")

(defcustom scala-top-level-re
  (concat
   "^[a-zA-Z_]"
   (regexp-opt  scala-extended-block-or-clause-re-raw)
   "[( \t]*.*:?")
  "A form which starts at zero indent level, but is not a comment."
  :type '(regexp)
   :tag "ar-scala-scala-top-level-re"
  :group 'ar-scala-scala-mode
  )

(defvar scala-comment-re "#[ \t]*"
  "Needed for normalized processing.")

(defconst scala-block-keywords
   (regexp-opt scala-block-or-clause-re-raw 'symbols)
  "Matches known keywords opening a block.

Customizing ‘ar-block-or-clause-re-raw’  will change values here")

(defconst scala-try-clause-re
  (concat
   "[ \t]*\\_<\\("
   (mapconcat 'identity
              (list
               "else"
               "except"
               "finally")
              "\\|")
   "\\)\\_>[( \t]*.*:")
  "Matches the beginning of a compound try-statement's clause.")

(defcustom scala-compilation-regexp-alist
  `((,(rx line-start (1+ (any " \t")) "File \""
          (group (1+ (not (any "\"<")))) ; avoid ‘<stdin>’ &c
          "\", line " (group (1+ digit)))
     1 2)
    (,(rx " in file " (group (1+ not-newline)) " on line "
          (group (1+ digit)))
     1 2)
    (,(rx line-start "> " (group (1+ (not (any "(\"<"))))
          "(" (group (1+ digit)) ")" (1+ (not (any "("))) "()")
     1 2))
  "Fetch errors from Py-shell.
hooked into ‘compilation-error-regexp-alist’"
  :type '(alist string)
   :tag "ar-scala-scala-compilation-regexp-alist"
  :group 'ar-scala-scala-mode)

(defconst scala-font-lock-syntactic-keywords
  ;; Make outer chars of matching triple-quote sequences into generic
  ;; string delimiters.  Fixme: Is there a better way?
  ;; First avoid a sequence preceded by an odd number of backslashes.
  `((,(concat "\\(?:^\\|[^\\]\\(?:\\\\.\\)*\\)" ;Prefix.
              "\\(?1:\"\\)\\(?2:\"\\)\\(?3:\"\\)\\(?4:\"\\)\\(?5:\"\\)\\(?6:\"\\)\\|\\(?1:\"\\)\\(?2:\"\\)\\(?3:\"\\)\\|\\(?1:'\\)\\(?2:'\\)\\(?3:'\\)\\(?4:'\\)\\(?5:'\\)\\(?6:'\\)\\|\\(?1:'\\)\\(?2:'\\)\\(?3:'\\)\\(?4:'\\)\\(?5:'\\)\\(?6:'\\)\\|\\(?1:'\\)\\(?2:'\\)\\(?3:'\\)")
     (1 (scala--quote-syntax 1) t t)
     (2 (scala--quote-syntax 2) t t)
     (3 (scala--quote-syntax 3) t t)
     (6 (scala--quote-syntax 1) t t))))

(defconst scala--windows-config-register 313465889
  "Internal used by ‘window-configuration-to-register’.")

;; (setq scala--windows-config-register 313;; 465889)

(put 'ar-scala-indent-offset 'safe-local-variable 'integerp)

;; testing
(defvar scala-ert-test-default-executables
  (list "python" "python3" "ipython")
  "Serialize tests employing dolist.")

(defcustom scala-shell-unfontify-p t
  "Run ‘ar-scala--run-unfontify-timer’ unfontifying the shell banner-text.

Default is nil"

  :type 'boolean
   :tag "ar-scala-scala-scala-shell-unfontify-p"
  :group 'ar-scala-scala-mode)

;; Pdb
;; #62, pdb-track in a shell buffer
(defcustom pdb-track-stack-from-shell-p t
  "If t, track source from shell-buffer.

Default is t.
Add hook \\='comint-output-filter-functions \\='ar-scala--pdbtrack-track-stack-file"

  :type 'boolean
  :tag "pdb-track-stack-from-shell-p"
  :group 'ar-scala-scala-mode)

(defvar gud-pdb-history ""
  "Silence compiler warning.")

(defcustom scala-update-gud-pdb-history-p t
  "If pdb should provide suggestions WRT file to check and ‘ar-pdb-path’.

Default is t
See lp:963253"
  :type 'boolean
   :tag "ar-scala-scala-update-gud-pdb-history-p"
  :group 'ar-scala-scala-mode)

(defcustom scala-pdb-executable nil
  "Indicate PATH/TO/pdb.

Default is nil
See lp:963253"
  :type 'string
   :tag "ar-scala-scala-pdb-executable"
  :group 'ar-scala-scala-mode)

(defcustom scala-pdb-path
  (if (or (eq system-type 'ms-dos)(eq system-type 'windows-nt))
      (quote c:/python27/python\ -i\ c:/python27/Lib/pdb.py)
    '/usr/lib/python2.7/pdb.py)
  "Where to find pdb.py.  Edit this according to your system.
For example \"/usr/lib/python3.4\" might be an option too.

If you ignore the location `M-x scala-guess-pdb-path' might display it."
  :type 'variable
   :tag "ar-scala-scala-pdb-path"
  :group 'ar-scala-scala-mode)

(defvar scala-python-ms-pdb-command ""
  "MS-systems might use that.")

(defcustom scala-shell-prompt-pdb-regexp "[(<]*[Ii]?[Pp]db[>)]+ "
  "Regular expression matching pdb input prompt of Python shell.
It should not contain a caret (^) at the beginning."
  :type 'string
   :tag "ar-scala-scala-scala-shell-prompt-pdb-regexp"
  :group 'ar-scala-scala-mode)

(defcustom scala-pdbtrack-stacktrace-info-regexp
  "> \\([^\"(<]+\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()"
  "Regular expression matching stacktrace information.
Used to extract the current line and module being inspected."
  :type 'string
  :safe 'stringp
   :tag "ar-scala-scala-pdbtrack-stacktrace-info-regexp"
  :group 'ar-scala-scala-mode)

(defvar scala-pdbtrack-tracked-buffer nil
  "Variable containing the value of the current tracked buffer.
Never set this variable directly, use
‘ar-pdbtrack-set-tracked-buffer’ instead.")

(defvar scala-pdbtrack-buffers-to-kill nil
  "List of buffers to be deleted after tracking finishes.")

(defcustom scala-pdbtrack-do-tracking-p t
  "Controls whether the pdbtrack feature is enabled or not.
When non-nil, pdbtrack is enabled in all comint-based buffers,
e.g. shell buffers and the *Python* buffer.  When using pdb to debug a
Python program, pdbtrack notices the pdb prompt and displays the
source file and line that the program is stopped at, much the same way
as ‘gud-mode’ does for debugging C programs with gdb."
  :type 'boolean
   :tag "ar-scala-scala-pdbtrack-do-tracking-p"
  :group 'ar-scala-scala-mode)
(make-variable-buffer-local 'ar-scala-pdbtrack-do-tracking-p)

(defcustom scala-pdbtrack-filename-mapping nil
  "Supports mapping file paths when opening file buffers in pdbtrack.
When non-nil this is an alist mapping paths in the Python interpreter
to paths in Emacs."
  :type 'alist
   :tag "ar-scala-scala-pdbtrack-filename-mapping"
  :group 'ar-scala-scala-mode)

(defcustom scala-pdbtrack-minor-mode-string " PDB"
  "String to use in the minor mode list when pdbtrack is enabled."
  :type 'string
   :tag "ar-scala-scala-pdbtrack-minor-mode-string"
  :group 'ar-scala-scala-mode)

(defconst scala-pdbtrack-stack-entry-regexp
   (concat ".*\\("ar-scala-scala-shell-input-prompt-1-regexp">\\|"ar-scala-ipython-input-prompt-re">\\|>\\) *\\(.*\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>()]+\\)()")
  "Regular expression pdbtrack uses to find a stack trace entry.")

(defconst scala-pdbtrack-marker-regexp-file-group 2
  "Group position in gud-pydb-marker-regexp that matches the file name.")

(defconst scala-pdbtrack-marker-regexp-line-group 3
  "Group position in gud-pydb-marker-regexp that matches the line number.")

(defconst scala-pdbtrack-marker-regexp-funcname-group 4
  "Group position in gud-pydb-marker-regexp that matches the function name.")

(defconst scala-pdbtrack-track-range 10000
  "Max number of characters from end of buffer to search for stack entry.")

(defvar scala-pdbtrack-is-tracking-p nil)

(defvar scala--docbeg nil
  "Internally used by ‘ar-scala--write-edit’.")

(defvar scala--docend nil
  "Internally used by ‘ar-scala--write-edit’.")

(defvar scala-completion-setup-code  "def __PYTHON_EL_get_completions(text):
    completions = []
    completer = None

    try:
        import readline

        try:
            import __builtin__
        except ImportError:
            # Python 3
            import builtins as __builtin__
        builtins = dir(__builtin__)

        is_ipython = ('__IPYTHON__' in builtins or
                      '__IPYTHON__active' in builtins)
        splits = text.split()
        is_module = splits and splits[0] in ('from', 'import')

        if is_ipython and is_module:
            from IPython.core.completerlib import module_completion
            completions = module_completion(text.strip())
        elif is_ipython and '__IP' in builtins:
            completions = __IP.complete(text)
        elif is_ipython and 'get_ipython' in builtins:
            completions = get_ipython().Completer.all_completions(text)
        else:
            # Try to reuse current completer.
            completer = readline.get_completer()
            if not completer:
                # importing rlcompleter sets the completer, use it as a
                # last resort to avoid breaking customizations.
                import rlcompleter
                completer = readline.get_completer()
            if getattr(completer, 'PYTHON_EL_WRAPPED', False):
                completer.print_mode = False
            i = 0
            while True:
                completion = completer(text, i)
                if not completion:
                    break
                i += 1
                completions.append(completion)
    except:
        pass
    finally:
        if getattr(completer, 'PYTHON_EL_WRAPPED', False):
            completer.print_mode = True
    return completions"
  "Code used to setup completion in inferior Python processes.")

(defcustom scala-completion-setup-code
  "
def __PYTHON_EL_get_completions(text):
    completions = []
    completer = None

    try:
        import readline

        try:
            import __builtin__
        except ImportError:
            # Python 3
            import builtins as __builtin__
        builtins = dir(__builtin__)

        is_ipython = ('__IPYTHON__' in builtins or
                      '__IPYTHON__active' in builtins)
        splits = text.split()
        is_module = splits and splits[0] in ('from', 'import')

        if is_ipython and is_module:
            from IPython.core.completerlib import module_completion
            completions = module_completion(text.strip())
        elif is_ipython and '__IP' in builtins:
            completions = __IP.complete(text)
        elif is_ipython and 'get_ipython' in builtins:
            completions = get_ipython().Completer.all_completions(text)
        else:
            # Try to reuse current completer.
            completer = readline.get_completer()
            if not completer:
                # importing rlcompleter sets the completer, use it as a
                # last resort to avoid breaking customizations.
                import rlcompleter
                completer = readline.get_completer()
            if getattr(completer, 'PYTHON_EL_WRAPPED', False):
                completer.print_mode = False
            i = 0
            while True:
                completion = completer(text, i)
                if not completion:
                    break
                i += 1
                completions.append(completion)
    except:
        pass
    finally:
        if getattr(completer, 'PYTHON_EL_WRAPPED', False):
            completer.print_mode = True
    return completions"
  "Code used to setup completion in inferior Python processes."
  :type 'string
   :tag "ar-scala-scala-completion-setup-code"
  :group 'ar-scala-scala-mode)

(defcustom scala-shell-completion-string-code
  "';'.join(__PYTHON_EL_get_completions('''%s'''))"
  "Python code used to get a string of completions separated by semicolons.
The string passed to the function is the current python name or
the full statement in the case of imports."
  :type 'string
   :tag "ar-scala-scala-scala-shell-completion-string-code"
  :group 'ar-scala-scala-mode)

(defface scala-XXX-tag-face
  '((t (:inherit font-lock-string-face)))
  "XXX\\|TODO\\|FIXME "
   :tag "ar-scala-scala-XXX-tag-face"
  :group 'ar-scala-scala-mode)

(defface scala-pseudo-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for pseudo keywords in Python mode, like self, True, False,
  Ellipsis.

See also ‘ar-object-reference-face’"
   :tag "ar-scala-scala-pseudo-keyword-face"
  :group 'ar-scala-scala-mode)

(defface scala-object-reference-face
  '((t (:inherit scala-pseudo-keyword-face)))
  "Face when referencing object members from its class resp. method.,
commonly \"cls\" and \"self\""
   :tag "ar-scala-scala-object-reference-face"
  :group 'ar-scala-scala-mode)

(defface scala-number-face
 '((t (:inherit nil)))
  "Highlight numbers."
   :tag "ar-scala-scala-number-face"
  :group 'ar-scala-scala-mode)

(defface scala-try-if-face
  '((t (:inherit font-lock-keyword-face)))
  "Highlight keywords."
   :tag "ar-scala-scala-try-if-face"
  :group 'ar-scala-scala-mode)

(defface scala-import-from-face
  '((t (:inherit font-lock-keyword-face)))
  "Highlight keywords."
   :tag "ar-scala-scala-import-from-face"
  :group 'ar-scala-scala-mode)

(defface scala-def-class-face
  '((t (:inherit font-lock-keyword-face)))
  "Highlight keywords."
   :tag "ar-scala-scala-def-class-face"
  :group 'ar-scala-scala-mode)

 ;; PEP 318 decorators
(defface scala-decorators-face
  '((t (:inherit font-lock-keyword-face)))
  "Face method decorators."
   :tag "ar-scala-scala-decorators-face"
  :group 'ar-scala-scala-mode)

(defface scala-builtins-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for builtins like TypeError, object, open, and exec."
   :tag "ar-scala-scala-builtins-face"
  :group 'ar-scala-scala-mode)

(defface scala-class-name-face
  '((t (:inherit font-lock-type-face)))
  "Face for classes."
   :tag "ar-scala-scala-class-name-face"
  :group 'ar-scala-scala-mode)

(defface scala-def-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for definitions."
   :tag "ar-scala-scala-def-face"
  :group 'ar-scala-scala-mode)

(defface scala-exception-name-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for Python exceptions."
   :tag "ar-scala-scala-exception-name-face"
  :group 'ar-scala-scala-mode)

;; subr-x.el might not exist yet
;; #73, Byte compilation on Emacs 25.3 fails on different trim-right signature

(defsubst scala--string-trim-left (strg &optional regexp)
  "Trim STRING of leading string matching REGEXP.

REGEXP defaults to \"[ \\t\\n\\r]+\"."
  (if (string-match (concat "\\`\\(?:" (or regexp "[ \t\n\r]+") "\\)") strg)
      (replace-match "" t t strg)
    strg))

(defsubst scala--string-trim-right (strg &optional regexp)
  "Trim STRING of trailing string matching REGEXP.

REGEXP defaults to \"[ \\t\\n\\r]+\"."
  (if (string-match (concat "\\(?:" (or regexp "[ \t\n\r]+") "\\)\\'") strg)
      (replace-match "" t t strg)
    strg))

(defsubst scala--string-trim (strg &optional trim-left trim-right)
  "Trim STRING of leading and trailing strings matching TRIM-LEFT and TRIM-RIGHT.

TRIM-LEFT and TRIM-RIGHT default to \"[ \\t\\n\\r]+\"."
  (scala--string-trim-left (scala--string-trim-right strg trim-right) trim-left))

(defcustom scala-empty-line-p-chars "^[ \t\r]*$"
  "Empty-line-p-chars."
  :type 'regexp
   :tag "ar-scala-scala-empty-line-p-chars"
  :group 'ar-scala-scala-mode)

(defcustom scala-default-working-directory ""
  "If not empty used by ‘scala-set-current-working-directory’."
  :type 'string
   :tag "ar-scala-scala-default-working-directory"
  :group 'ar-scala-scala-mode)

(defcustom scala-python-ffap-setup-code
  "
def __FFAP_get_module_path(objstr):
    try:
        import inspect
        import os.path
        # NameError exceptions are delayed until this point.
        obj = eval(objstr)
        module = inspect.getmodule(obj)
        filename = module.__file__
        ext = os.path.splitext(filename)[1]
        if ext in ('.pyc', '.pyo'):
            # Point to the source file.
            filename = filename[:-1]
        if os.path.exists(filename):
            return filename
        return ''
    except:
        return ''"
  "Python code to get a module path."
  :type 'string
   :tag "ar-scala-scala-python-ffap-setup-code"
  :group 'ar-scala-scala-mode)

;; (defvar scala-ffap-string-code
;;   "__FFAP_get_module_path('''%s''')\n"
;;   "Python code used to get a string with the path of a module.")

(defcustom scala-ffap-string-code
  "__FFAP_get_module_path('''%s''')"
  "Python code used to get a string with the path of a module."
  :type 'string
   :tag "ar-scala-scala-python-ffap-string-code"
  :group 'ar-scala-scala-mode)

(defvar scala-mode-map nil)

(defvar scala-debug-p nil
  "Used for development purposes.")

;; This and other stuff from python.el

(defvar scala-last-exeption-buffer nil
  "Internal use only - when ‘ar-scala-up-exception’ is called.

In source-buffer, this will deliver the exception-buffer again.")

(defcustom scala-electric-backspace-p nil
  "When ‘t’, <backspace> key will delete all whitespace chars before point.

Default nil"

  :type 'boolean
   :tag "ar-scala-scala-electric-backspace-p"
  :group 'ar-scala-scala-mode
  :safe 'booleanp
  :set (lambda (symbol value)
         (set-default symbol value)
         (scala-electric-backspace-mode (if value 1 0))))



(provide 'ar-scala-vars)
;;; scala-vars.el ends here
