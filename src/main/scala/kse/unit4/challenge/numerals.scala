package kse.unit4.challenge

import scala.annotation.targetName

object numerals:

  trait Numeral:

    def isZero: Boolean

    def predecessor: Numeral

    def successor: Numeral = Successor(this)

    @targetName("greater than")
    infix def >(that: Numeral): Boolean

    @targetName("greater or equal to")
    infix def >=(that: Numeral): Boolean = (this == that) || (this > that)

    @targetName("less than")
    infix def <(that: Numeral): Boolean = !(this >= that)

    @targetName("less or equal to")
    infix def <=(that: Numeral): Boolean = !(this > that)

    @targetName("addition")
    infix def +(that: Numeral): Numeral

    // Optional
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral

    def toInt: Int

    override def toString: String = s"Nat($predecessor)"

  type Zero = Zero.type

  object Zero extends Numeral:

    def isZero: Boolean = true

    def predecessor: Numeral = Zero

    @targetName("greater than")
    infix def >(that: Numeral): Boolean = false

    @targetName("addition")
    infix def +(that: Numeral): Numeral = that

    // Optional
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral = that match
      case Zero => this
      case _    => Zero

    def toInt: Int = 0

    override def toString: String = "Zero"

    override def equals(obj: Any): Boolean = obj match
      case value: Zero.type => true
      case _                => false

    override def hashCode(): Int = 0

  object Successor:
    def unapply(successor: Successor): Option[Numeral] = Option(successor.predecessor)

  class Successor(n: Numeral) extends Numeral:

    def isZero: Boolean = false

    def predecessor: Numeral = n

    @targetName("greater than")
    infix def >(that: Numeral): Boolean = that match
      case Zero         => true
      case s: Successor => n > s.predecessor

    @targetName("addition")
    infix def +(that: Numeral): Numeral = Successor(n + that)

    // Optional
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral = that match
      case Zero         => this
      case s: Successor => this.predecessor - s.predecessor

    def toInt: Int = 1 + n.toInt

    override def toString: String = s"Successor(${predecessor.toString}"

    override def equals(obj: Any): Boolean = obj match
      case s: Successor => this.predecessor.equals(s.predecessor)
      case _            => false

    override def hashCode(): Int = toInt.hashCode() + 1
