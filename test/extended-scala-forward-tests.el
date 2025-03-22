;;; extended-scala-forward-tests.el --- Test scala-mode navigation  -*- lexical-binding: t -*-

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

(require 'extended-scala-mode-setup-tests)

(ert-deftest extended-scala-forward-def-test-ocYQwh ()
  (ar-test-point-min
      "def foo(p: Seq[String], q: Seq[Int]): Map[Int, String] = ???"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (extended-scala-forward-def)
    (should (looking-back "???" (line-beginning-position)))
    ))

(ert-deftest extended-scala-forward-def-test-O1YRXs ()
  (ar-test-point-min
      "def foo(p: Seq[String], q: Seq[Int]): Map[Int, String] = ???"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (extended-scala-forward-def-or-class)
    (should (looking-back "???" (line-beginning-position)))
    ))

(ert-deftest extended-scala-forward-def-test-S8VNtl ()
  (ar-test-point-min
      "def foo(p: Seq[String], q: Seq[Int]): Map[Int, String] =
  ???"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (extended-scala-forward-def)
    (should (looking-back "???" (line-beginning-position)))
    ))

(ert-deftest extended-scala-forward-def-test-XNuwtY ()
  (ar-test-point-min
      "def foo(p: Seq[String], q: Seq[Int]): Map[Int, String] =
  ???"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (extended-scala-forward-def-or-class)
    (should (looking-back "???" (line-beginning-position)))
    ))



(ert-deftest extended-scala-forward-def-test-yVEnBi ()
  (ar-test-point-min
      "def aktualisieren(p: D4, x: Double): D4 = p match { case (min, max, sum, length) =>
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
"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (extended-scala-forward-def)
    (should (eq (char-before) ?}))
    ))

(ert-deftest extended-scala-forward-def-test-wweans ()
  (ar-test-point-min
      "def aktualisieren(p: D4, x: Double): D4 = p match { case (min, max, sum, length) =>
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
"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (extended-scala-forward-def)
    (should (eq (char-before) ?}))
    ))

(ert-deftest extended-scala-forward-def-test-uRKqXt ()
  (ar-test-point-min
      "// some comment
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
"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (extended-scala-forward-def)
    (should (eq (char-before) ?}))
    ))

(ert-deftest extended-scala-forward-def-test-CqlQP0 ()
  (ar-test-point-min
      "def aktualisieren(p: D4, x: Double): D4 = p match { case (min, max, sum, length) =>
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
"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (extended-scala-forward-def)
    (should (eq (char-before) ?}))
    ))

(ert-deftest extended-scala-forward-def-test-YcRxyu ()
  (ar-test-point-min
      "def aktualisieren(p: D4, x: Double): D4 = p match { case (min, max, sum, length) =>
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
"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (search-forward "}")
    (extended-scala-forward-def)
    (should (eq (char-before) ?}))
    ))

(ert-deftest extended-scala-forward-def-test-g2JtZe ()
  (ar-test-point-min
      "type D4 = (Double, Double, Double, Int)
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
"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (extended-scala-forward-def)
    (should (eq (char-before) ?}))
    ))

(ert-deftest extended-scala-forward-def-test-d2KZi1 ()
  (ar-test-point-min
"object LargestTree {
  def largestTree(a: Seq[List[Int]]): Seq[List[Int]] = {
    a.map{ k=>k.sortBy(k=>(-k))}.map{ k => k.take(3) }
  }
  def main(args: Array[String]) {
    val expected = Seq(List(50, 30, 10), List(100), List(200, 20, 2))
    val result =  this.largestTree(Seq(List(50, 30, 10), List(100), List(200, 20, 2)))
    assert(result == expected)
    if (result == expected) println(\"solution2.1.7.10_object_main result: %s\n\".format(result))

  }
}"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (extended-scala-forward-def)
    (should (eq (char-before) ?}))
    ))

(ert-deftest extended-scala-forward-def-test-bZp3fF ()
  (ar-test-point-min
"object LargestTree {
  def largestTree(a: Seq[List[Int]]): Seq[List[Int]] = {
    a.map{ k=>k.sortBy(k=>(-k))}.map{ k => k.take(3) }
  }
  def main(args: Array[String]) {
    val expected = Seq(List(50, 30, 10), List(100), List(200, 20, 2))
    val result =  this.largestTree(Seq(List(50, 30, 10), List(100), List(200, 20, 2)))
    assert(result == expected)
    if (result == expected) println(\"solution2.1.7.10_object_main result: %s\n\".format(result))

  }
}"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (search-forward " main")
    (extended-scala-forward-def)
    (should (eq (char-before) ?}))
    (forward-char -1)
    (should-not (bolp))
    ))

(ert-deftest extended-scala-forward-def-or-class-test-CB1v8d ()
  (ar-test-point-min
      "// some comment
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
}"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (search-forward "expected")
    (extended-scala-forward-def-or-class)
    (should (eq (char-before) ?}))
    (forward-char -1)
    (should-not (bolp))))

(ert-deftest extended-scala-forward-def-or-class-test-9Au9zk ()
  (ar-test-point-min
      "// some comment
case class Rectangle(width: Int, height: Int) {
  val area = width * height
  val neu = \"asf\"
}
"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (extended-scala-forward-def-or-class)
    (should (eq (char-before) ?}))
    ))

(ert-deftest extended-scala-forward-def-or-class-test-8S6M05 ()
  (ar-test-point-min
      "// some comment
case class Rectangle(width: Int, height: Int) {
  val area = width * height
  val neu = \"asf\"
}
"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (extended-scala-forward-def-or-class)
    (should (eq (char-before) ?}))
    ))

(ert-deftest extended-scala-forward-def-or-class-test-5vQnkh ()
  (ar-test-point-min
"// some comment
trait Pet {
    def speak = println(\"Yo\")     // concrete implementation of a speak method
    def comeToMaster(): Unit      // abstract
}"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (extended-scala-forward-def-or-class)
    (should (eq (char-before) ?}))
    ))

(ert-deftest extended-scala-forward-def-or-class-test-45R3LH ()
  (ar-test-point-min
"// some comment
trait Pet {
    def speak = println(\"Yo\")     // concrete implementation of a speak method
    def comeToMaster(): Unit      // abstract
}"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (search-forward "def")
    (extended-scala-forward-def-or-class)
    (should (eq (char-before) ?\)))
    ))

(ert-deftest extended-scala-forward-def-or-class-test-nALcPi ()
  (ar-test
"// some comment
trait Pet {
    def speak = println(\"Yo\")     //  concrete implementation of a speak method
    def comeToMaster(): Unit      // abstract
}
"
    'scala-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "//" nil t 2)
    (extended-scala-forward-def-or-class)
    (back-to-indentation)
    (should (looking-at "def comeToMaster"))
    ))

(ert-deftest extended-scala-forward-def-or-class-test-L3Ki5e ()
  (ar-test-point-min
"val expected =  (1.0,3.0,2.0)

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

"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (extended-scala-forward-def-or-class)
    (should (looking-back "expected)" (line-beginning-position)))
    ))

(ert-deftest extended-scala-forward-def-or-class-test-r8Auha ()
  (ar-test-point-min
"object LargestTree {
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
"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (extended-scala-forward-def-or-class)
    (back-to-indentation)
    (should (eq (char-after) ?}))
    (should (bolp))
    ))


(ert-deftest extended-scala-forward-def-or-class-test-lGhLdK ()
  (ar-test-point-min
"object LargestTree {
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
"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (end-of-line) 
    (extended-scala-forward-def-or-class)
    (back-to-indentation)
    (should (eq (char-after) ?}))
    (should (bolp))
    ))

(ert-deftest extended-scala-forward-def-or-class-test-RZEaEC ()
  (ar-test-point-min
"object LargestTree {
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
"
    'scala-mode
    ar-debug-p
    (goto-char (point-min))
    (search-forward "largest")
    (extended-scala-forward-def-or-class)
    (back-to-indentation)
    (should (eq (char-after) ?}))
    (should-not (eq (char-before) 20))
    ))






(provide 'extended-scala-forward-tests)
;;; extended-scala-forward-tests.el ends here
