# extended-scala-mode
Navigation in Emacs scala-mode 

Jump to beginning or end of function or class

<kbd>C-M-a</kbd> and <kbd>C-M-e</kbd>.

Provides commands

- ar-scala-backward-def

- ar-scala-backward-class (trait)

- ar-scala-backward-def-or-class (def-or-trait)

- ar-scala-forward-def

- ar-scala-forward-class (trait)

- ar-scala-forward-def-or-class (def-or-trait)

Key setting here is

```(define-key scala-mode-map [(control meta)(a)] 'ar-scala-backward-def-or-class)```

```(define-key scala-mode-map [(control meta)(e)] 'ar-scala-forward-def-or-class)```
