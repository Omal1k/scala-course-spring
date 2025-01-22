package kse.unit1.challenge

import scala.annotation.tailrec

object arithmetic:

  type Number = Long

  val increment: Number => Number =
    value => value + 1

  val decrement: Number => Number =
    value => value - 1

  val isZero: Number => Boolean =
    value => value == 0

  val isNonNegative: Number => Boolean =
    value => value >= 0

  val abs: Number => Number =
    value =>
      if isNonNegative(value) then value
      else -value

  def addition(left: Number, right: Number): Number =
    // require(left >= 0, "Left must be non-negative")
    // require(right >= 0, "Right must be non-negative")

    @tailrec
    def saver(left: Number, right: Number): Number =
      if isZero(left) then right
      else if isZero(right) then left
      else if (isNonNegative(left) && !isNonNegative(right)) || (!isNonNegative(left) && !isNonNegative(right))
      then saver(decrement(left), increment(right))
      else saver(increment(left), decrement(right))

    saver(left, right)

  def multiplication(left: Number, right: Number): Number =
    // require(left >= 0, "Left must be non-negative")
    // require(right >= 0, "Right must be non-negative")

    @tailrec
    def saver(left: Number, right: Number, acc: Number): Number =
      if isZero(right) then acc
      else if !isNonNegative(left) && isNonNegative(right)
      then saver(left, decrement(right), addition(acc, left))
      else if isNonNegative(left) && !isNonNegative(right)
      then saver(right, decrement(left), addition(acc, right))
      else if !isNonNegative(left) && !isNonNegative(right)
      then saver(abs(left), decrement(abs(right)), addition(acc, abs(left)))
      else saver(left, decrement(right), addition(acc, left))

    saver(left, right, 0)

  def power(base: Number, p: Number): Number =
    require(p >= 0, "Power must be non-negative")
    require(base != 0 || p != 0, "0^0 is undefined")

    @tailrec
    def saver(base: Number, p: Number, acc: Number): Number =
      if isZero(p) then acc
      else saver(base, decrement(p), multiplication(acc, base))

    saver(base, p, 1)
