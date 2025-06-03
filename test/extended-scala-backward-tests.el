;;; extended-scala-backward-tests.el --- Test scala-mode navigation  -*- lexical-binding: t -*-

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

(ert-deftest extended-scala-scala-backward-test-ocYQwh ()
  (ar-test
      "def foo(p: Seq[String], q: Seq[Int]): Map[Int, String] = ???"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (ar-backward-def)
    (should (looking-at "def"))
    ))

(ert-deftest extended-scala-scala-backward-test-S8VNtl ()
  (ar-test
      "def foo(p: Seq[String], q: Seq[Int]): Map[Int, String] =
  ???"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (ar-backward-def)
    (should (looking-at "def"))
    ))

(ert-deftest extended-scala-scala-backward-test-wweans ()
  (ar-test
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
    'ar-verbose-p
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (ar-backward-def)
    (should (looking-at "def multiLeftFoldInt"))
    ))

(ert-deftest extended-scala-scala-backward-test-YcRxyu ()
  (ar-test
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
    'ar-verbose-p
    (goto-char (point-max))
    (search-backward "length")
    (ar-backward-def)
    (should (looking-at "def multiLeftFoldInt"))
    ))

(ert-deftest extended-scala-scala-backward-test-g2JtZe ()
  (ar-test
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
    'ar-verbose-p
    (goto-char (point-max))
    (search-backward "def")
    (beginning-of-line)
    (ar-backward-def)
    (should (looking-at "def aktualisieren"))
    ))

(ert-deftest extended-scala-scala-backward-test-d2KZi1 ()
  (ar-test
"object LargestTree {
  def largestTree(a: Seq[List[Int]]): Seq[List[Int]] = {
    a.map{ k=>k.sortBy(k=>(-k))}.map{ k => k.take(3) }
  }
  def main(args: Array[String]) {
    val expected = Seq(List(50, 30, 10), List(100), List(200, 20, 2))
    val result =  this.largestTree(Seq(List(50, 30, 10), List(100), List(200, 20, 2)))
    assert(result == expected)
    if (result == expected) println(\"solution2.1.7.10_object_main result: %s\".format(result))
  }
}"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (search-backward "result")
    (ar-backward-def)
    (should (looking-at "def main"))
    ))

(ert-deftest extended-scala-scala-backward-test-bZp3fF ()
  (ar-test
"object LargestTree {
  def largestTree(a: Seq[List[Int]]): Seq[List[Int]] = {
    a.map{ k=>k.sortBy(k=>(-k))}.map{ k => k.take(3) }
  }
  def main(args: Array[String]) {
    val expected = Seq(List(50, 30, 10), List(100), List(200, 20, 2))
    val result =  this.largestTree(Seq(List(50, 30, 10), List(100), List(200, 20, 2)))
    assert(result == expected)
    if (result == expected) println(\"solution2.1.7.10_object_main result: %s\".format(result))

  }
}"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (search-backward "largest")
    (ar-backward-def)
    (should (looking-at "def main"))
    ))

(ert-deftest extended-scala-scala-backward-test-CB1v8d ()
  (ar-test
      "// some comment
object LargestTree {
  def largestTree(a: Seq[List[Int]]): Seq[List[Int]] = {
    a.map{ k=>k.sortBy(k=>(-k))}.map{ k => k.take(3) }
  }
  def main(args: Array[String]) {
    val expected = Seq(List(50, 30, 10), List(100), List(200, 20, 2))
    val result =  this.largestTree(Seq(List(50, 30, 10), List(100), List(200, 20, 2)))
    assert(result == expected)
    if (result == expected) println(\"solution2.1.7.10_object_main result: %s\".format(result))

  }
}"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (search-backward "def largest")
    (beginning-of-line)
    (ar-backward-def-or-class)
    (should (looking-at "object"))))

(ert-deftest extended-scala-scala-backward-test-9Au9zk ()
  (ar-test
      "// some comment
case class Rectangle(width: Int, height: Int) {
  val area = width * height
  val neu = \"asf\"
}
"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (ar-backward-def-or-class)
    (should (looking-at "case "))))

(ert-deftest extended-scala-scala-backward-test-HjAcmj ()
  (ar-test
"// some comment
trait Pet {
    def speak = println(\"Yo\")     // concrete implementation of a speak method
    def comeToMaster(): Unit      // abstract
}"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (ar-backward-def-or-class)
    (should (looking-at "trait "))))

(ert-deftest extended-scala-scala-backward-test-yYjS3s ()
  (ar-test
"// some comment
trait Pet {
    def speak = println(\"Yo\")     // concrete implementation of a speak method
    def comeToMaster(): Unit      // abstract
}"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (ar-backward-class)
    (should (looking-at "trait "))))

(ert-deftest extended-scala-scala-backward-test-BZ3aQM ()
  (ar-test
"// some comment
def multiLeftFoldInt(a: Seq[Double]): (Double, Double, Double) = {
  val init: D4 = (Double.PositiveInfinity, Double.NegativeInfinity, 0.0, 0)
  val (min, max, sum, length) = a.foldLeft(init)(aktualisieren)
  (min, max, sum/length)
}
"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (search-backward "min")
    (ar-backward-def)
    (should (looking-at "def "))))

(ert-deftest extended-scala-scala-backward-test-YNcqcU ()
  (ar-test
"// some comment
def multiLeftFoldInt(a: Seq[Double]): (Double, Double, Double) = {
  val init: D4 = (Double.PositiveInfinity, Double.NegativeInfinity, 0.0, 0)
  val (min, max, sum, length) = a.foldLeft(init)(aktualisieren)
  (min, max, sum/length)
}
"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (search-backward "min")
    (ar-backward-def-or-class)
    ;; (sit-for 1)
    (should (looking-at "def "))))

(ert-deftest extended-scala-scala-backward-test-y9WuQ6 ()
  (ar-test
"// some comment
def multiLeftFoldInt(a: Seq[Double]): (Double, Double, Double) = {
  val init: D4 = (Double.PositiveInfinity, Double.NegativeInfinity, 0.0, 0)
  val (min, max, sum, length) = a.foldLeft(init)(aktualisieren)
  (min, max, sum/length)
}
"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (search-backward "D4")
    (ar-backward-def-or-class)
    (should (looking-at "def "))))

(ert-deftest extended-scala-scala-backward-test-MHFSaE ()
  (ar-test
"// some comment
def multiLeftFoldInt(a: Seq[Double]): (Double, Double, Double) = {
  val init: D4 = (Double.PositiveInfinity, Double.NegativeInfinity, 0.0, 0)
  val (min, max, sum, length) = a.foldLeft(init)(aktualisieren)
  (min, max, sum/length)
}
"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (search-backward "min")
    (if (featurep 'extended-scala-scala-mode)
        (funcall 'ar-backward-def)
      (funcall 'scala-syntax:beginning-of-definition))
    (should (looking-at "def "))))

(ert-deftest extended-scala-scala-backward-test-eznI8K ()
  (ar-test
      "import scala.annotation.tailrec

@tailrec def lengthT(s: Seq[Int], res: Int): Int =
  if (s.isEmpty) res
  else lengthT(s.tail, res + 1)
"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (search-backward "lengthT")
    (if (featurep 'extended-scala-scala-mode)
        (funcall 'ar-backward-def)
      (funcall 'scala-syntax:beginning-of-definition))
    (should (looking-at "def "))))

(ert-deftest extended-scala-scala-backward-test-W1eux3 ()
  (ar-test
"import scala.annotation.tailrec

def length[A](xs: Seq[A]): Int = {
  @tailrec def lengthT(s: Seq[A], res: Int): Int = {
    if (s.isEmpty) res
    else lengthT(s.tail, res + 1)
  }
  lengthT(xs, 0)
}"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    ;; (forward-char -1)
    (ar-backward-def-or-class)
    (should (looking-at "def length" t))))

(ert-deftest extended-scala-scala-backward-test-PFzcBf ()
  (ar-test
      "def toPairs[A](xs: Seq[A], default: A): Seq[(A, A)] = {
  type Acc = (Seq[(A, A)], Seq[A])
    // Type alias, for brevity.
  def init: Acc = (Seq(), Seq())
  def updater(acc: Acc, x: A): Acc = acc match {
    case (result, Seq())
        => (result, Seq(x))
    case (result, Seq(prev)) => (result :+ ((prev, x)), Seq())
  }
  val (result, holdover) = xs.foldLeft(init)(updater)
    holdover match {
      // May need to append the last element to the result.
      case Seq()
          => result
      case Seq(x)
          => result :+ ((x, default))
    }
}"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (ar-backward-def-or-class)
    (should (looking-at "def toPairs" t))))

(ert-deftest extended-scala-scala-backward-test-9vHP8b ()
  (ar-test
      "def toPairs[A](xs: Seq[A], default: A): Seq[(A, A)] = {
  type Acc = (Seq[(A, A)], Seq[A])
    // Type alias, for brevity.
  def init: Acc = (Seq(), Seq())
  def updater(acc: Acc, x: A): Acc = acc match {
    case (result, Seq())
        => (result, Seq(x))
    case (result, Seq(prev)) => (result :+ ((prev, x)), Seq())
  }
  val (result, holdover) = xs.foldLeft(init)(updater)
    holdover match {
      // May need to append the last element to the result.
      case Seq()
          => result
      case Seq(x)
          => result :+ ((x, default))
    }
}"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (search-backward "case" nil t 3)
    (beginning-of-line)
    (ar-backward-def-or-class)
    (should (looking-at "def updater" t))))

(ert-deftest extended-scala-scala-backward-test-ZB5JkS ()
  (ar-test
      "def toPairs[A](xs: Seq[A], default: A): Seq[(A, A)] = {
  type Acc = (Seq[(A, A)], Seq[A])
    // Type alias, for brevity.
  def init: Acc = (Seq(), Seq())
  def updater(acc: Acc, x: A): Acc = acc match {
    case (result, Seq())
        => (result, Seq(x))
    case (result, Seq(prev)) => (result :+ ((prev, x)), Seq())
  }
  val (result, holdover) = xs.foldLeft(init)(updater)
    holdover match {
      // May need to append the last element to the result.
      case Seq()
          => result
      case Seq(x)
          => result :+ ((x, default))
    }
}
"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (beginning-of-line)
    (search-backward "Seq(prev)")
    (end-of-line)
    (ar-backward-def-or-class)
    (should (looking-at "def updater" t))))

(ert-deftest extended-scala-scala-backward-test-tGkIa5 ()
  (ar-test
      "// some comment
object LargestTree {
  def largestTree(a: Seq[List[Int]]): Seq[List[Int]] = {
    a.map{ k=>k.sortBy(k=>(-k))}.map{ k => k.take(3) }
  }
  def main(args: Array[String]) {
    val expected = Seq(List(50, 30, 10), List(100), List(200, 20, 2))
    val result =  this.largestTree(Seq(List(50, 30, 10), List(100), List(200, 20, 2)))
    assert(result == expected)
    if (result == expected) println(\"solution2.1.7.10_object_main result: %s\".format(result))

  }
}"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (search-backward "def main")
    (beginning-of-line)
    (ar-backward-class)
    (should (looking-at "object"))))

(ert-deftest extended-scala-scala-backward-test-CUuiEB ()
  (ar-test
      "// some comment
object LargestTree {
  def largestTree(a: Seq[List[Int]]): Seq[List[Int]] = {
    a.map{ k=>k.sortBy(k=>(-k))}.map{ k => k.take(3) }
  }
  def main(args: Array[String]) {
    val expected = Seq(List(50, 30, 10), List(100), List(200, 20, 2))
    val result =  this.largestTree(Seq(List(50, 30, 10), List(100), List(200, 20, 2)))
    assert(result == expected)
    if (result == expected) println(\"solution2.1.7.10_object_main result: %s\".format(result))

  }
}"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (search-backward "def main")
    (ar-backward-class)
    (should (looking-at "object"))))

(ert-deftest extended-scala-scala-backward-test-iBzf8i ()
  (ar-test
"object LargestTree {
  def largestTree(a: Seq[List[Int]]): Seq[List[Int]] = {
    a.map{ k=>k.sortBy(k=>(-k))}.map{ k => k.take(3) }
  }
  def main(args: Array[String]) {
    val expected = Seq(List(50, 30, 10), List(100), List(200, 20, 2))
    val result =  this.largestTree(Seq(List(50, 30, 10), List(100), List(200, 20, 2)))
    assert(result == expected)
    if (result == expected) println(\"solution2.1.7.10_object_main result: %s\".format(result))

  }
}"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (search-backward "def main")
    (skip-chars-backward " \t\r\n\f")
    (forward-char -1)
    (ar-backward-statement)
    (should (looking-at "a.map"))))

(ert-deftest extended-scala-scala-backward-test-nE4BBt ()
  (ar-test
"object LargestTree {
  def largestTree(a: Seq[List[Int]]): Seq[List[Int]] = {
    a.map{ k=>k.sortBy(k=>(-k))}.map{ k => k.take(3) }
    // a.map{ k=>k.sortBy(k=>(-k))}.map{ k => k.take(3) }
    // a.map{ k=>k.sortBy(k=>(-k))}.map{ k => k.take(3) }
  }
  def main(args: Array[String]) {
    val expected = Seq(List(50, 30, 10), List(100), List(200, 20, 2))
    val result =  this.largestTree(Seq(List(50, 30, 10), List(100), List(200, 20, 2)))
    assert(result == expected)
    if (result == expected) println(\"solution2.1.7.10_object_main result: %s\".format(result))

  }
}"
    'scala-mode
    'ar-verbose-p
    (goto-char (point-max))
    (search-backward "def main")
    (skip-chars-backward " \t\r\n\f")
    (beginning-of-line)
    ;; (forward-char -1)
    (ar-backward-statement)
    (should (looking-at "a.map"))))

(provide 'extended-scala-backward-tests)
;;; extended-scala-backward-tests.el ends here
