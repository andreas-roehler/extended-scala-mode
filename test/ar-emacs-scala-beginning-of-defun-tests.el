;;; ar-emacs-scala-beginning-of-defun-tests.el --- Test scala-mode navigation  -*- lexical-binding: t -*-

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

(require 'ar-emacs-scala-mode-setup-tests)

(setq-local beginning-of-defun-function #'ar-scala-backward-def-or-class)

(ert-deftest ar-emacs-scala-beginning-of-defun-test-ocYQwh ()
  (ar-test
      "def foo(p: Seq[String], q: Seq[Int]): Map[Int, String] = ???"
    'scala-mode
    ar-debug-p
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (beginning-of-defun)
    (should (looking-at "def"))
    ))

(ert-deftest ar-emacs-scala-beginning-of-defun-test-S8VNtl ()
  (ar-test
      "def foo(p: Seq[String], q: Seq[Int]): Map[Int, String] =
  ???"
    'scala-mode
    ar-debug-p
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (beginning-of-defun)
    (should (looking-at "def"))
    ))

(ert-deftest ar-emacs-scala-beginning-of-defun-test-wweans ()
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
    ar-debug-p
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (beginning-of-defun)
    (should (looking-at "def multiLeftFoldInt"))
    ))

(ert-deftest ar-emacs-scala-beginning-of-defun-test-YcRxyu ()
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
    ar-debug-p
    (goto-char (point-max))
    (search-backward "length")
    (beginning-of-defun)
    (message "Reached: %s" (buffer-substring-no-properties (point) (line-end-position)))
    (should (looking-at "def multiLeftFoldInt"))
    ))

(ert-deftest ar-emacs-scala-beginning-of-defun-test-g2JtZe ()
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
    ar-debug-p
    (goto-char (point-max))
    (search-backward "def")
    (beginning-of-line)
    (beginning-of-defun)
    (should (looking-at "def aktualisieren"))
    ))

(ert-deftest ar-emacs-scala-beginning-of-defun-test-d2KZi1 ()
  (ar-test
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
    (goto-char (point-max))
    (search-backward "result")
    (beginning-of-defun)
    (message "Reached: %s" (buffer-substring-no-properties (point) (line-end-position)))
    (should (looking-at "def main"))
    ))

(ert-deftest ar-emacs-scala-beginning-of-defun-test-bZp3fF ()
  (ar-test
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
    (goto-char (point-max))
    (search-backward "largest")
    (beginning-of-defun)
    (message "Reached: %s" (buffer-substring-no-properties (point) (line-end-position)))
    (should (looking-at "def main"))
    ))

(ert-deftest ar-emacs-scala-beginning-of-defun-test-CB1v8d ()
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
    if (result == expected) println(\"solution2.1.7.10_object_main result: %s\n\".format(result))

  }
}"
    'scala-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "def largest")
    (beginning-of-line)
    (beginning-of-defun)
    (should (looking-at "object"))))

(ert-deftest ar-emacs-scala-beginning-of-defun-test-9Au9zk ()
  (ar-test
      "// some comment
case class Rectangle(width: Int, height: Int) {
  val area = width * height
  val neu = \"asf\"
}
"
    'scala-mode
    ar-debug-p
    (goto-char (point-max))
    (beginning-of-defun)
    (should (looking-at "case "))))

(ert-deftest ar-emacs-scala-beginning-of-defun-test-HjAcmj ()
  (ar-test
"// some comment
trait Pet {
    def speak = println(\"Yo\")     // concrete implementation of a speak method
    def comeToMaster(): Unit      // abstract
}"
    'scala-mode
    ar-debug-p
    (goto-char (point-max))
    (beginning-of-defun)
    (should (looking-at "trait "))))

(ert-deftest ar-emacs-scala-beginning-of-defun-test-yYjS3s ()
  (ar-test
"// some comment
trait Pet {
    def speak = println(\"Yo\")     // concrete implementation of a speak method
    def comeToMaster(): Unit      // abstract
}"
    'scala-mode
    ar-debug-p
    (goto-char (point-max))
    (beginning-of-defun)
    (should (looking-at "trait "))))

(ert-deftest ar-emacs-scala-beginning-of-defun-test-BZ3aQM ()
  (ar-test
"// some comment
def multiLeftFoldInt(a: Seq[Double]): (Double, Double, Double) = {
  val init: D4 = (Double.PositiveInfinity, Double.NegativeInfinity, 0.0, 0)
  val (min, max, sum, length) = a.foldLeft(init)(aktualisieren)
  (min, max, sum/length)
}
"
    'scala-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "min")
    (beginning-of-defun)
    (message "Reached: %s" (buffer-substring-no-properties (point) (line-end-position))) 
    (should (looking-at "def "))))

(ert-deftest ar-emacs-scala-beginning-of-defun-test-YNcqcU ()
  (ar-test
"// some comment
def multiLeftFoldInt(a: Seq[Double]): (Double, Double, Double) = {
  val init: D4 = (Double.PositiveInfinity, Double.NegativeInfinity, 0.0, 0)
  val (min, max, sum, length) = a.foldLeft(init)(aktualisieren)
  (min, max, sum/length)
}
"
    'scala-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "min")
    (beginning-of-defun)
    (message "Reached: %s" (buffer-substring-no-properties (point) (line-end-position)))
    (should (looking-at "def "))))

(ert-deftest ar-emacs-scala-beginning-of-defun-test-y9WuQ6 ()
  (ar-test
"// some comment
def multiLeftFoldInt(a: Seq[Double]): (Double, Double, Double) = {
  val init: D4 = (Double.PositiveInfinity, Double.NegativeInfinity, 0.0, 0)
  val (min, max, sum, length) = a.foldLeft(init)(aktualisieren)
  (min, max, sum/length)
}
"
    'scala-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "D4")
    (beginning-of-defun)
    (should (looking-at "def "))))

(ert-deftest ar-emacs-scala-beginning-of-defun-test-MHFSaE ()
  (ar-test
"// some comment
def multiLeftFoldInt(a: Seq[Double]): (Double, Double, Double) = {
  val init: D4 = (Double.PositiveInfinity, Double.NegativeInfinity, 0.0, 0)
  val (min, max, sum, length) = a.foldLeft(init)(aktualisieren)
  (min, max, sum/length)
}
"
    'scala-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "min")
    (beginning-of-defun)
    (message "Reached: %s" (buffer-substring-no-properties (point) (line-end-position)))
    (should (looking-at "def "))))

(ert-deftest ar-emacs-scala-beginning-of-defun-test-eznI8K ()
  (ar-test
      "import scala.annotation.tailrec

@tailrec def lengthT(s: Seq[Int], res: Int): Int =
  if (s.isEmpty) res
  else lengthT(s.tail, res + 1)
"
    'scala-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "lengthT")
    (beginning-of-defun)
    (message "Reached: %s" (buffer-substring-no-properties (point) (line-end-position)))
    (should (looking-at "@tailrec def "))))

(ert-deftest ar-emacs-scala-beginning-of-defun-test-W1eux3 ()
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
    ar-debug-p
    (goto-char (point-max))
    ;; (forward-char -1)
    (beginning-of-defun)
    (should (looking-at "def length\\[A]"))))

(provide 'ar-emacs-scala-beginning-of-defun-tests)
;;; ar-emacs-scala-beginning-of-defun-tests.el ends here
