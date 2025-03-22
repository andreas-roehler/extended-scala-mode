;; extended-scala-compute-indentation.el --- Part of extended-scala-mode -*- lexical-binding: t; -*-

;; Helper functions

;; URL: https://gitlab.com/python-mode-devs

;; Keywords: languages, processes

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
;;

;;; Code:

;;  Keymap

;;  Utility stuff

(defun extended-scala--computer-closing-inner-list ()
  "Compute indentation according to extended-scala-closing-list-dedents-bos."
  (if extended-scala-closing-list-dedents-bos
      (+ (current-indentation) extended-scala-indent-offset)
    (1+ (current-column))))

(defun extended-scala-compute-indentation-according-to-list-style-intern()
  (pcase extended-scala-indent-list-style
    (`line-up-with-first-element
     (if (looking-at "\\s([ \\t]*$")
         (cond ((save-excursion
                  (back-to-indentation)
                  (looking-at extended-scala-if-re))
                ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=42513
                0)
               ((looking-back "^[ \\t]+ " (line-beginning-position))
                ;; line with 'sword', from a single opener
                ;; var5: Sequence[Mapping[str, Sequence[str]]] = [
                ;;     {
                ;;      'red': ['scarlet', 'vermilion', 'ruby'],
                ;;      'green': ['emerald', 'aqua']
                ;;     },
                ;;     {
                ;;                 'sword': ['cutlass', 'rapier']
                ;;     }
                ;; ]
                (+ (current-indentation) 1))
               (t
                (+ (current-indentation) extended-scala-indent-offset)))
       (+ (current-column) 1)))
    (`one-level-to-beginning-of-statement
     (+ (current-indentation) extended-scala-indent-offset))
    (`one-level-from-first-element
     (+ 1 (current-column) extended-scala-indent-offset))))

(defun extended-scala-compute-indentation-according-to-list-style (pps line-beginning-position)
  "See ‘extended-scala-indent-list-style’

Choices are:

\\='line-up-with-first-element (default)
\\='one-level-to-beginning-of-statement
\\='one-level-from-opener

See also extended-scala-closing-list-dedents-bos"
  (let ((orig (point))
        (lines (extended-scala-count-lines))
        (just-at-closer (save-excursion
                          (or (looking-back "^[ \t]*\\s)" (line-beginning-position))
                              (and (looking-at "[ \t]*\\s)")
                                   (looking-back "^[ \t]*" (line-beginning-position))))))
        (lines-from
         (progn (goto-char (nth 1 pps))
                (extended-scala-count-lines))))
    ;; now at start of inner list
    (cond
     ((save-excursion
        ;; from last 'pk': chained lists as special case
        ;; data = {'key': {
        ;;     'objlist': [
        ;;         {'pk': 1,
        ;;          'name': 'first'},
        ;;         {'pk': 2,
        ;;          'name': 'second'}
        ;;     ]
        ;; }}
        (and
         ;; list starts at current line
         (< line-beginning-position (nth 1 pps))
         ;; if previous line contains in another list, indent according to its start
         (progn
           (beginning-of-line)
           (skip-chars-backward " \t\r\n\f")
           (skip-chars-backward "^[[:alnum:]]")
           (eq (nth 0 pps) (nth 0 (parse-partial-sexp (point-min) (point)))))))
      (beginning-of-line)
      (skip-chars-backward " \t\r\n\f")
      (skip-chars-backward "^[[:alnum:]]")
      (goto-char (nth 1 (parse-partial-sexp (point-min) (point))))
      (current-indentation))
     (just-at-closer
      ;; dedents at opener or at openers indentation
      (cond ((or (eq line-beginning-position (line-beginning-position)) extended-scala-closing-list-dedents-bos)
             (current-indentation))
            (t (extended-scala-compute-indentation-according-to-list-style-intern))))
     ((save-excursion
        (and
         (not just-at-closer)
         (< 1 (- lines lines-from))
         (progn
           (goto-char orig)
           (forward-line -1)
           ;; ignore if in higher nesting
           (eq (nth 0 pps) (nth 0 (parse-partial-sexp (point-min) (point)))))))
      (progn
        (goto-char orig)
        (forward-line -1)
        (current-indentation)))
     ((eq line-beginning-position (line-beginning-position))
      ;; (and (eq line-beginning-position (line-beginning-position))  (looking-back [^ \\t]*))
      (current-indentation))
     ((eq (current-column) 0)
      ;; List starts at BOL or indent,
      ;; https://bugs.launchpad.net/python-mode/+bug/328842
      (+ (current-indentation) extended-scala-indent-offset))
     (t (extended-scala-compute-indentation-according-to-list-style-intern)))))

(defun extended-scala-compute-comment-indentation (pps iact orig origline closing line nesting repeat indent-offset liep)
  (cond ((nth 8 pps)
         (goto-char (nth 8 pps))
         (cond ((and line (eq (current-column) (current-indentation)))
                (current-indentation))
               ((and (eq liep (line-end-position)) py-indent-honors-inline-comment)
                (current-column))
               ((extended-scala--line-backward-maybe)
                (setq line t)
                (skip-chars-backward " \t")
                (extended-scala-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
               (t (if extended-scala-indent-comments
                      (progn
                        (extended-scala-backward-comment)
                        (extended-scala-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
                    0))))
        ((and
          (looking-at (concat "[ \t]*" comment-start))
          (looking-back "^[ \t]*" (line-beginning-position))(not line)
          (eq liep (line-end-position)))
         (if extended-scala-indent-comments
             (progn
               (setq line t)
               (skip-chars-backward " \t\r\n\f")
               ;; as previous comment-line might
               ;; be wrongly unindented, travel
               ;; whole commented section
               (extended-scala-backward-comment)
               (extended-scala-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
           0))
        ((and
          (looking-at (concat "[ \t]*" comment-start))
          (looking-back "^[ \t]*" (line-beginning-position))
          (not (eq liep (line-end-position))))
         (current-indentation))
        ((and (eq 11 (syntax-after (point))) line extended-scala-indent-honors-inline-comment)
         (current-column))))

(defun extended-scala-compute-indentation--at-closer-maybe (erg)
  (goto-char erg)
  (backward-sexp)
  (if extended-scala-closing-list-dedents-bos
      (current-indentation)
    (+ (current-indentation) extended-scala-indent-offset)))

(defun extended-scala-compute-indentation--at-closer-p ()
  "If on a line on with just on or more chars closing a list."
  ;; (interactive)
  (or
   (and (looking-back "^[ \\t]*[\])}]+[ \\t]*" (line-beginning-position))(match-end 0))
   (and (looking-back "^ *" (line-beginning-position))
        (looking-at "[ \\t]*[\]})]+[ \\t]*$")
        (match-end 0)
        )))

(defun extended-scala-compute-indentation (&optional iact orig origline closing line nesting repeat indent-offset liep beg)
  "Compute Python indentation.

When HONOR-BLOCK-CLOSE-P is non-nil, statements such as ‘return’,
‘raise’, ‘break’, ‘continue’, and ‘pass’ force one level of dedenting.

ORIG keeps original position
ORIGLINE keeps line where compute started
CLOSING is t when started at a char delimiting a list as \"]})\"
LINE indicates being not at origline now
NESTING is currently ignored, if executing from inside a list
REPEAT counter enables checks against ‘extended-scala-max-specpdl-size’
INDENT-OFFSET allows calculation of block-local values
LIEP stores line-end-position at point-of-interest
"
  (interactive "p")
  ;; (and (not line) (< (current-column) (current-indentation)) (back-to-indentation))
  (let ((beg
         (or beg
             (and (comint-check-proc (current-buffer))
                  (re-search-backward (concat extended-scala-shell-prompt-regexp "\\|" extended-scala-ipython-output-prompt-re "\\|" extended-scala-ipython-input-prompt-re) nil t 1))
             (point-min))))
    (save-excursion
      (save-restriction
        (narrow-to-region beg (line-end-position))
        ;; in shell, narrow from previous prompt
        ;; needed by closing
        (let* ((orig (or orig (copy-marker (point))))
               (origline (or origline (extended-scala-count-lines (point-min) (point))))
               ;; closing indicates: when started, looked
               ;; at a single closing parenthesis
               ;; line: moved already a line backward
               (liep (or liep (line-end-position)))
	       (line (or line (not (eq origline (extended-scala-count-lines (point-min) (point))))))
               ;; (line line)
               (pps (progn
		      (unless (eq (current-indentation) (current-column))(skip-chars-backward " " (line-beginning-position)))
		      ;; (when (eq 5 (car (syntax-after (1- (point)))))
		      ;;   (forward-char -1))
		      (parse-partial-sexp (point-min) (point))))

               ;; in a recursive call already
               (repeat (if repeat
                           (setq repeat (1+ repeat))
                         0))
               ;; nesting: started nesting a list
               (nesting nesting)
               indent this-line)
          (if (< extended-scala-max-specpdl-size repeat)
              (error "‘extended-scala-compute-indentation’ reached loops max.")
            (setq nesting (nth 0 pps))
            (setq indent
                  (cond
                   ((bobp)
		    (cond ((eq liep (line-end-position))
                           0)
			  ;; - ((looking-at extended-scala-outdent-re)
			  ;; - (+ (or indent-offset (and extended-scala-smart-indentation (extended-scala-guess-indent-offset)) extended-scala-indent-offset) (current-indentation)))
			  ((and line (looking-at extended-scala-block-or-clause-re))
			   extended-scala-indent-offset)
                          ((looking-at extended-scala-outdent-re)
                           (+ (or indent-offset (and extended-scala-smart-indentation (extended-scala-guess-indent-offset)) extended-scala-indent-offset) (current-indentation)))
                          (t
                           (current-indentation))))
		   ;; in string
		   ((and (nth 3 pps) (nth 8 pps))
		    (cond
		     ((extended-scala--docstring-p (nth 8 pps))
		      (save-excursion
			;; (goto-char (match-beginning 0))
			(back-to-indentation)
			(if (looking-at "[uUrR]?\"\"\"\\|[uUrR]?'''")
			    (progn
			      (skip-chars-backward " \t\r\n\f")
			      (back-to-indentation)
			      (if (looking-at extended-scala-def-or-class-re)
				  (+ (current-column) extended-scala-indent-offset)
				(current-indentation)))
			  (skip-chars-backward " \t\r\n\f")
			  (back-to-indentation)
			  (current-indentation))))
                     ;; string in list
                     ((save-excursion (goto-char (nth 8 pps))(nth 0 (parse-partial-sexp (point-min) (point))))
                      (if
                          (or line (save-excursion (goto-char (nth 8 pps))(< (extended-scala-count-lines (point-min) (point)) origline)))
                          (progn
                            (goto-char (nth 8 pps)) (current-column))
                        (goto-char (nth 8 pps))
                        (extended-scala-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg)))
                     ((or line (< (extended-scala-count-lines (point-min) (point)) origline))
                      (goto-char (nth 8 pps))(current-indentation))
		     (t 0)))
		   ((and (looking-at "\"\"\"\\|'''") (not (bobp)))
		    (extended-scala-backward-statement)
		    (extended-scala-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg))
		   ;; comments
		   ((or
		     (nth 8 pps)
		     (and
		      (looking-at (concat "[ \t]*" comment-start))
		      (looking-back "^[ \t]*" (line-beginning-position))(not line))
		     (and (eq 11 (syntax-after (point))) line extended-scala-indent-honors-inline-comment))
		    (extended-scala-compute-comment-indentation pps iact orig origline closing line nesting repeat indent-offset liep))
		   ;; lists
		   ;; ((and line (nth 1 pps))
		   ((nth 1 pps)
                    (extended-scala-compute-indentation-according-to-list-style pps (line-beginning-position)))
		   ;; (if (< (nth 1 pps) (line-beginning-position))
                   ;; Compute according to ‘extended-scala-indent-list-style’

                   ;; Choices are:

                   ;; \\='line-up-with-first-element (default)
                   ;; \\='one-level-to-beginning-of-statement
                   ;; \\='one-level-from-opener"

                   ;; See also extended-scala-closing-list-dedents-bos
		   ;;   (extended-scala-compute-indentation-in-list pps line closing orig)
		   ;; (back-to-indentation)
		   ;; (extended-scala-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg)))
		   ((and (eq (char-after) (or ?\( ?\{ ?\[)) line)
		    (1+ (current-column)))
		   ((extended-scala-preceding-line-backslashed-p)
		    (progn
		      (extended-scala-backward-statement)
		      (setq this-line (extended-scala-count-lines))
		      (if (< 1 (- origline this-line))
                          (extended-scala--fetch-indent-line-above orig)
			(if (looking-at "from +\\([^ \t\n]+\\) +import")
			    extended-scala-backslashed-lines-indent-offset
                          (if (< 20 (line-end-position))
                              8
                            (+ (current-indentation) extended-scala-continuation-offset))))))
		   ((and (looking-at extended-scala-block-closing-keywords-re)
                         (eq liep (line-end-position)))
		    (skip-chars-backward "[ \t\r\n\f]")
		    (extended-scala-backward-statement)
		    (cond ((looking-at extended-scala-extended-block-or-clause-re)
			   (+
			    ;; (if extended-scala-smart-indentation (extended-scala-guess-indent-offset) indent-offset)
			    (or indent-offset (and extended-scala-smart-indentation (extended-scala-guess-indent-offset)) extended-scala-indent-offset)
			    (current-indentation)))
                          ((looking-at extended-scala-block-closing-keywords-re)
			   (- (current-indentation) (or indent-offset extended-scala-indent-offset)))
                          (t (current-column))))
		   ((looking-at extended-scala-block-closing-keywords-re)
		    (if (< (line-end-position) orig)
			;; #80, Lines after return cannot be correctly indented
			(if (looking-at "return[ \\t]*$")
			    (current-indentation)
			  (- (current-indentation) (or indent-offset extended-scala-indent-offset)))
		      (extended-scala-backward-block-or-clause)
		      (current-indentation)))
		   ((and (looking-at extended-scala-minor-clause-re) (not line)
                         (eq liep (line-end-position)))
		    (cond
                     ((looking-at extended-scala-case-re)
                      (and (extended-scala--backward-regexp (quote extended-scala-match-case-re) nil '>)
                           ;; (+ (current-indentation) extended-scala-indent-offset)
                           (current-indentation)))
                     ((looking-at extended-scala-minor-clause-re)
		      (and (extended-scala--backward-regexp (quote extended-scala-block-or-clause-re)
                                                ;; an arbitray number, larger than an real expected indent
                                                (* 99 extended-scala-indent-offset)
                                                '<)
                           (current-indentation)))

                     ((looking-at extended-scala-outdent-re)
		      (and (extended-scala--backward-regexp (quote extended-scala-block-or-clause-re)
                                                ;; an arbitray number, larger than an real expected indent
                                                (* 99 extended-scala-indent-offset)
                                                '<)))
		     ((bobp) 0)
		     (t (save-excursion
			  ;; (skip-chars-backward " \t\r\n\f")
			  (if (extended-scala-backward-block)
			      ;; (extended-scala--backward-regexp (quote extended-scala-block-or-clause-re))
			      (+ extended-scala-indent-offset (current-indentation))
			    0)))))
		   ((looking-at extended-scala-extended-block-or-clause-re)
		    (cond ((and (not line)
				(eq liep (line-end-position)))
			   (when (extended-scala--line-backward-maybe) (setq line t))
			   (extended-scala-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg))
                          (t (+
			      (cond (indent-offset)
				    (extended-scala-smart-indentation
				     (extended-scala-guess-indent-offset))
				    (t extended-scala-indent-offset))
			      (current-indentation)))))
		   ((and
		     (< (line-end-position) liep)
		     (eq (current-column) (current-indentation)))
		    (and
		     (looking-at extended-scala-assignment-re)
		     (goto-char (match-end 0)))
		    ;; multiline-assignment
		    (if (and nesting (looking-at " *[[{(]") (not (looking-at ".+[]})][ \t]*$")))
			(+ (current-indentation) (or indent-offset extended-scala-indent-offset))
		      (current-indentation)))
		   ((looking-at extended-scala-assignment-re)
		    (extended-scala-backward-statement)
		    (extended-scala-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg))
		   ((and (< (current-indentation) (current-column))(not line))
		    (back-to-indentation)
		    (unless line
		      (setq nesting (nth 0 (parse-partial-sexp (point-min) (point)))))
		    (extended-scala-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg))
		   ((and (not (extended-scala--beginning-of-statement-p)) (not (and line (eq 11 (syntax-after (point))))))
		    (if (bobp)
			(current-column)
		      (if (eq (point) orig)
                          (progn
			    (when (extended-scala--line-backward-maybe) (setq line t))
			    (extended-scala-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg))
			(extended-scala-backward-statement)
			(extended-scala-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg))))
		   ((or (extended-scala--statement-opens-block-p extended-scala-extended-block-or-clause-re) (looking-at "@"))
		    (if (< (extended-scala-count-lines) origline)
			(+ (or indent-offset (and extended-scala-smart-indentation (extended-scala-guess-indent-offset)) extended-scala-indent-offset) (current-indentation))
		      (skip-chars-backward " \t\r\n\f")
		      (setq line t)
		      (back-to-indentation)
		      (extended-scala-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg)))
		   ((and extended-scala-empty-line-closes-p (extended-scala--after-empty-line))
		    (progn (extended-scala-backward-statement)
			   (- (current-indentation) (or indent-offset extended-scala-indent-offset))))
		   ;; still at original line
		   ((and (eq liep (line-end-position))
                         (save-excursion
			   (and
                            (extended-scala--go-to-keyword (quote extended-scala-extended-block-or-clause-re) nil (* extended-scala-indent-offset 99))
                            (if (looking-at (concat extended-scala-block-re "\\|" extended-scala-outdent-re))
		                (+ (current-indentation)
                                   (if extended-scala-smart-indentation
				       (or indent-offset (extended-scala-guess-indent-offset))
				     (or indent-offset extended-scala-indent-offset)))
                              (current-indentation))))))
		   ((and (not line)
                         (eq liep (line-end-position))
                         (extended-scala--beginning-of-statement-p))
		    (extended-scala-backward-statement)
		    (extended-scala-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg))
		   (t (current-indentation))))
            ;; (when (or (eq 1 (prefix-numeric-value iact)) extended-scala-verbose-p) (message "%s" indent))
            (when extended-scala-verbose-p (message "%s" indent))
            indent))))))

(provide 'extended-scala-compute-indentation)
 ;;; extended-scala-compute-indentation.el ends here
