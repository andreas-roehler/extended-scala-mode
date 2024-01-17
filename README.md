# ar-emacs-scala-mode
Extend ‘emacs-scala-mode’ with navigation

Jump to beginning or end of function or class definition with keys

‘C-M-a’ and ‘C-M-e’.

Provides commands

ar-scala-backward-def

ar-scala-backward-class

ar-scala-backward-def-or-class

ar-scala-forward-def

ar-scala-forward-class

ar-scala-forward-def-or-class

Key setting here is

(define-key scala-mode-map [(control meta)(a)] 'ar-scala-backward-def-or-class)

(define-key scala-mode-map [(control meta)(e)] 'ar-scala-forward-def-or-class)
