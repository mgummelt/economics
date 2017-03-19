package com.mgummelt.economics

object Utils {
  /** Inverse function */
  def inv[A, B](f: A => B): B => A = _

  /** Returns an instance of A that matches the given predicate. */
  def first[A](p: A => Boolean): A = _

  /** Returns an instance of A that matches the given predicate. */
  def all[A](p: A => Boolean): Set[A] = _
}

/** Random Variable */
class RV[A]