package com.mgummelt.economics

object Utils {
  /** Inverse function */
  def inv[A, B](f: A => B): B => A = _

  def first[A](p: A => Boolean): A = _
}

/** Random Variable */
class RV[A]

/** 0 <= probability <= 1 */
abstract class Probability extends Double