;;; ar-emacs-scala-forward-function-tests.el --- Test scala-mode navigation  -*- lexical-binding: t -*-

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

(require 'ert)
(require 'ar-emacs-scala-mode-setup-tests)

(defun ar-emacs-scala-forward-def-test-ocYQwh ()
  ""
  (interactive)
  (with-temp-buffer
    (insert "def foo(p: Seq[String], q: Seq[Int]): Map[Int, String] = ???")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (ar-scala-forward-def)
    (should (looking-back "???" (line-beginning-position)))))

(defun ar-emacs-scala-forward-def-test-O1YRXs ()
  ""
(interactive)
  (with-temp-buffer
(insert "def foo(p: Seq[String], q: Seq[Int]): Map[Int, String] = ???")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (ar-scala-forward-def-or-class)
    (should (looking-back "???" (line-beginning-position)))
    ))

(defun ar-emacs-scala-forward-def-test-S8VNtl ()
  ""
(interactive)
  (with-temp-buffer
      (insert "def foo(p: Seq[String], q: Seq[Int]): Map[Int, String] =
  ???")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (ar-scala-forward-def)
    (should (looking-back "???" (line-beginning-position)))
    ))

(defun ar-emacs-scala-forward-def-test-XNuwtY ()
  ""
(interactive)
  (with-temp-buffer
      (insert "def foo(p: Seq[String], q: Seq[Int]): Map[Int, String] =
  ???")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (ar-scala-forward-def-or-class)
    (should (looking-back "???" (line-beginning-position)))
    ))

(defun ar-emacs-scala-forward-def-test-yVEnBi ()
  ""
(interactive)
  (with-temp-buffer
      (insert "def aktualisieren(p: D4, x: Double): D4 = p match { case (min, max, sum, length) =>
  (math.min(x, min), math.max(x, max), x + sum, length + 1)
}

def multiLeftFoldInt(a: Seq[Double]): (Double, Double, Double) = {
  val init: D4 = (Double.PositiveInfinity, Double.NegativeInfinity, 0.0, 0)
  val (min, max, sum, length) = a.foldLeft(init)(aktualisieren)
  (min, max, sum/length)
}

val result =  multiLeftFoldInt(Seq(1.0, 1.5, 2.0, 2.5, 3.0))
val expected =  (1.0,3.0,2.0)

assert(result == expected)
")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (ar-scala-forward-def)
    (should (eq (char-before) ?}))
    ))

(defun ar-emacs-scala-forward-def-test-wweans ()
  ""
(interactive)
  (with-temp-buffer
     (insert "def aktualisieren(p: D4, x: Double): D4 = p match { case (min, max, sum, length) =>
  (math.min(x, min), math.max(x, max), x + sum, length + 1)
}

def multiLeftFoldInt(a: Seq[Double]): (Double, Double, Double) = {
  val init: D4 = (Double.PositiveInfinity, Double.NegativeInfinity, 0.0, 0)
  val (min, max, sum, length) = a.foldLeft(init)(aktualisieren)
  (min, max, sum/length)
}

val result =  multiLeftFoldInt(Seq(1.0, 1.5, 2.0, 2.5, 3.0))
val expected =  (1.0,3.0,2.0)

assert(result == expected)
")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (ar-scala-forward-def)
    (should (eq (char-before) ?}))
    ))

(defun ar-emacs-scala-forward-def-test-uRKqXt ()
  ""
(interactive)
  (with-temp-buffer
      (insert "// some comment
def aktualisieren(p: D4, x: Double): D4 = p match { case (min, max, sum, length) =>
  (math.min(x, min), math.max(x, max), x + sum, length + 1)
}

def multiLeftFoldInt(a: Seq[Double]): (Double, Double, Double) = {
  val init: D4 = (Double.PositiveInfinity, Double.NegativeInfinity, 0.0, 0)
  val (min, max, sum, length) = a.foldLeft(init)(aktualisieren)
  (min, max, sum/length)
}

val result =  multiLeftFoldInt(Seq(1.0, 1.5, 2.0, 2.5, 3.0))
val expected =  (1.0,3.0,2.0)

assert(result == expected)
")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (ar-scala-forward-def)
    (should (eq (char-before) ?}))
    ))

(defun ar-emacs-scala-forward-def-test-CqlQP0 ()
  ""
(interactive)
  (with-temp-buffer
      (insert "def aktualisieren(p: D4, x: Double): D4 = p match { case (min, max, sum, length) =>
  (math.min(x, min), math.max(x, max), x + sum, length + 1)
}

def multiLeftFoldInt(a: Seq[Double]): (Double, Double, Double) = {
  val init: D4 = (Double.PositiveInfinity, Double.NegativeInfinity, 0.0, 0)
  val (min, max, sum, length) = a.foldLeft(init)(aktualisieren)
  (min, max, sum/length)
}

val result =  multiLeftFoldInt(Seq(1.0, 1.5, 2.0, 2.5, 3.0))
val expected =  (1.0,3.0,2.0)

assert(result == expected)
")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (ar-scala-forward-def)
    (should (eq (char-before) ?}))
    ))

(defun ar-emacs-scala-forward-def-test-YcRxyu ()
  ""
(interactive)
  (with-temp-buffer
      (insert "def aktualisieren(p: D4, x: Double): D4 = p match { case (min, max, sum, length) =>
  (math.min(x, min), math.max(x, max), x + sum, length + 1)
}

def multiLeftFoldInt(a: Seq[Double]): (Double, Double, Double) = {
  val init: D4 = (Double.PositiveInfinity, Double.NegativeInfinity, 0.0, 0)
  val (min, max, sum, length) = a.foldLeft(init)(aktualisieren)
  (min, max, sum/length)
}

val result =  multiLeftFoldInt(Seq(1.0, 1.5, 2.0, 2.5, 3.0))
val expected =  (1.0,3.0,2.0)

assert(result == expected)
")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (search-forward "}")
    (ar-scala-forward-def)
    (should (eq (char-before) ?}))
    ))

(defun ar-emacs-scala-forward-def-test-g2JtZe ()
  ""
(interactive)
  (with-temp-buffer
      (insert "type D4 = (Double, Double, Double, Int)
// val a: Seq[Double] =  List(1.0, 2.0, 3.4)

def aktualisieren(p: D4, x: Double): D4 = p match { case (min, max, sum, length) =>
  (math.min(x, min), math.max(x, max), x + sum, length + 1)
}

def multiLeftFoldInt(a: Seq[Double]): (Double, Double, Double) = {
  val init: D4 = (Double.PositiveInfinity, Double.NegativeInfinity, 0.0, 0)
  val (min, max, sum, length) = a.foldLeft(init)(aktualisieren)
  (min, max, sum/length)
}

val result =  multiLeftFoldInt(Seq(1.0, 1.5, 2.0, 2.5, 3.0))
val expected =  (1.0,3.0,2.0)

assert(result == expected)
")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (ar-scala-forward-def)
    (should (eq (char-before) ?}))
    ))

(defun ar-emacs-scala-forward-def-test-d2KZi1 ()
  ""
(interactive)
  (with-temp-buffer
(insert "object LargestTree {
  def largestTree(a: Seq[List[Int]]): Seq[List[Int]] = {
    a.map{ k=>k.sortBy(k=>(-k))}.map{ k => k.take(3) }
  }
  def main(args: Array[String]) {
    val expected = Seq(List(50, 30, 10), List(100), List(200, 20, 2))
    val result =  this.largestTree(Seq(List(50, 30, 10), List(100), List(200, 20, 2)))
    assert(result == expected)
    if (result == expected) println(\"solution2.1.7.10_object_main result: %s\n\".format(result))

  }
}")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (ar-scala-forward-def)
    (should (eq (char-before) ?}))
    ))

(defun ar-emacs-scala-forward-def-test-bZp3fF ()
  ""
(interactive)
  (with-temp-buffer
    (insert "object LargestTree {
  def largestTree(a: Seq[List[Int]]): Seq[List[Int]] = {
    a.map{ k=>k.sortBy(k=>(-k))}.map{ k => k.take(3) }
  }
  def main(args: Array[String]) {
    val expected = Seq(List(50, 30, 10), List(100), List(200, 20, 2))
    val result =  this.largestTree(Seq(List(50, 30, 10), List(100), List(200, 20, 2)))
    assert(result == expected)
    if (result == expected) println(\"solution2.1.7.10_object_main result: %s\n\".format(result))

  }
}")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (search-forward " main")
    (ar-scala-forward-def)
    (should (eq (char-before) ?}))
    (forward-char -1)
    (should-not (bolp))))

(defun ar-emacs-scala-forward-def-or-class-test-CB1v8d ()
  ""
(interactive)
  (with-temp-buffer
    (insert "// some comment
object LargestTree {
  def largestTree(a: Seq[List[Int]]): Seq[List[Int]] = {
    a.map{ k=>k.sortBy(k=>(-k))}.map{ k => k.take(3) }
  }
  def main(args: Array[String]) {
    val expected = Seq(List(50, 30, 10), List(100), List(200, 20, 2))
    val result =  this.largestTree(Seq(List(50, 30, 10), List(100), List(200, 20, 2)))
    assert(result == expected)
    if (result == expected) println(\"solution2.1.7.10_object_main result: %s\n\".format(result))

  }
}")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (search-forward "expected")
    (ar-scala-forward-def-or-class)
    (should (eq (char-before) ?}))
    (forward-char -1)
    (should-not (bolp))))

(defun ar-emacs-scala-forward-def-or-class-test-9Au9zk ()
  ""
(interactive)
  (with-temp-buffer
    (insert "// some comment
case class Rectangle(width: Int, height: Int) {
  val area = width * height
  val neu = \"asf\"
}
")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (ar-scala-forward-def-or-class)
    (should (eq (char-before) ?}))))

(defun ar-emacs-scala-forward-def-or-class-test-8S6M05 ()
  ""
(interactive)
  (with-temp-buffer
      (insert "// some comment
case class Rectangle(width: Int, height: Int) {
  val area = width * height
  val neu = \"asf\"
}
")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (ar-scala-forward-def-or-class)
    (should (eq (char-before) ?}))
    ))

(defun ar-emacs-scala-forward-def-or-class-test-5vQnkh ()
  ""
(interactive)
  (with-temp-buffer
(insert "// some comment
trait Pet {
    def speak = println(\"Yo\")     // concrete implementation of a speak method
    def comeToMaster(): Unit      // abstract
}")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (ar-scala-forward-def-or-class)
    (should (eq (char-before) ?}))
    ))

(defun ar-emacs-scala-forward-def-or-class-test-45R3LH ()
  ""
(interactive)
  (with-temp-buffer
(insert "// some comment
trait Pet {
    def speak = println(\"Yo\")     // concrete implementation of a speak method
    def comeToMaster(): Unit      // abstract
}")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (search-forward "def")
    (ar-scala-forward-def-or-class)
    (should (eq (char-before) ?\)))
    ))

(defun ar-emacs-scala-forward-def-or-class-test-nALcPi ()
  ""
  (interactive)
  (with-temp-buffer
    (insert "// some comment
trait Pet {
    def speak = println(\"Yo\")     //  concrete implementation of a speak method
    def comeToMaster(): Unit      // abstract
}
")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-max))
    (search-backward "//" nil t 2)
    (ar-scala-forward-def-or-class)
    (back-to-indentation)
    (should (looking-at "def comeToMaster"))))

(defun ar-emacs-scala-forward-def-or-class-test-L3Ki5e ()
  ""
(interactive)
  (with-temp-buffer
    (insert "val expected =  (1.0,3.0,2.0)

assert(result == expected)

// def update(p: D4, x: Double): D4 = p match { case (min, max, sum, length) =>
// (math.min(x, min), math.max(x, max), x + sum, length + 1)
// }

// Now we can write the code of the required function:
// def f(xs: Seq[Double]): (Double, Double, Double) = {
//   val init: D4 = (Double.PositiveInfinity, Double.NegativeInfinity, 0, 0)
//   val (min, max, sum, length) = xs.foldLeft(init)(update)
//   (min, max, sum/length)
// }

// scala> multiLeftFoldInt(Seq(1.0, 1.5, 2.0, 2.5, 3.0))
// res4: (Double, Double, Double) = (1.0,3.0,2.0)

")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (ar-scala-forward-def-or-class)
    (should (looking-back "expected)" (line-beginning-position)))))

(defun ar-emacs-scala-forward-def-or-class-test-r8Auha ()
  ""
(interactive)
  (with-temp-buffer
    (insert "object LargestTree {
  def largestTree(a: Seq[List[Int]]): Seq[List[Int]] = {
    a.map{ k=>k.sortBy(k=>(-k))}.map{ k => k.take(3) }
  }
  def main(args: Array[String]) {
    val expected = Seq(List(50, 30, 10), List(100), List(200, 20, 2))
    val result =  this.largestTree(Seq(List(50, 30, 10), List(100), List(200, 20, 2)))
    assert(result == expected)
    if (result == expected) println(\"solution2.1.7.10_object_main result: %s\n\".format(result))

  }
}

LargestTree.main(Array())

// s$ scala solution2.1.7.10_object_main.scala
// solution2.1.7.10_object_main.scala:31: warning: Script has a main object but statement is disallowed
// LargestTree.main(Array())
//                 ^
// one warning found
// solution2.1.7.10_object_main result: List(List(50, 30, 10), List(100), List(200, 20, 2))
")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (ar-scala-forward-def-or-class)
    (back-to-indentation)
    (should (eq (char-after) ?}))
    (should (bolp))))

(defun ar-emacs-scala-forward-def-or-class-test-lGhLdK ()
  ""
(interactive)
  (with-temp-buffer
(insert "object LargestTree {
  def largestTree(a: Seq[List[Int]]): Seq[List[Int]] = {
    a.map{ k=>k.sortBy(k=>(-k))}.map{ k => k.take(3) }
  }
  def main(args: Array[String]) {
    val expected = Seq(List(50, 30, 10), List(100), List(200, 20, 2))
    val result =  this.largestTree(Seq(List(50, 30, 10), List(100), List(200, 20, 2)))
    assert(result == expected)
    if (result == expected) println(\"solution2.1.7.10_object_main result: %s\n\".format(result))

  }
}

LargestTree.main(Array())

// s$ scala solution2.1.7.10_object_main.scala
// solution2.1.7.10_object_main.scala:31: warning: Script has a main object but statement is disallowed
// LargestTree.main(Array())
//                 ^
// one warning found
// solution2.1.7.10_object_main result: List(List(50, 30, 10), List(100), List(200, 20, 2))
")
    (scala-mode)
    (when ar-debug-p (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (end-of-line)
    (ar-scala-forward-def-or-class)
    (back-to-indentation)
    (should (eq (char-after) ?}))
    (should (bolp))
    ))

(defun ar-emacs-scala-forward-def-or-class-test-RZEaEC ()
   ""
   (interactive)
   (with-temp-buffer
     (insert "object LargestTree {
  def largestTree(a: Seq[List[Int]]): Seq[List[Int]] = {
    a.map{ k=>k.sortBy(k=>(-k))}.map{ k => k.take(3) }
  }
  def main(args: Array[String]) {
    val expected = Seq(List(50, 30, 10), List(100), List(200, 20, 2))
    val result =  this.largestTree(Seq(List(50, 30, 10), List(100), List(200, 20, 2)))
    assert(result == expected)
    if (result == expected) println(\"solution2.1.7.10_object_main result: %s
\".format(result))

  }
}
")
     (scala-mode)
     (when ar-debug-p (switch-to-buffer (current-buffer)))
     (goto-char (point-min))
     (search-forward "largest")
     (ar-scala-forward-def-or-class)
     (back-to-indentation)
     (should (eq (char-after) ?}))
     (should-not (eq (char-before) 20))))

"@tailrec def binSearch(xs: Seq[Int], goal: Int)(left: Int = 0, right: Int =
  xs.length): Int = {
  // Check whether ‘goal‘ is at one of the boundaries.
  if (right - left <= 1 || xs(left) == goal) left
  else {
    val middle = (left + right) / 2
    // Determine which half of the array contains ‘target‘.
    // Update the accumulator accordingly.
    val (newLeft, newRight) =
      if (goal < xs(middle)) (left, middle)
      else (middle, right)
        binSearch(xs, goal)(newLeft, newRight) // Tail-recursive call.
  }
}
"

(provide 'ar-emacs-scala-forward-function-tests)
;;; ar-emacs-scala-forward-function-tests.el ends here
