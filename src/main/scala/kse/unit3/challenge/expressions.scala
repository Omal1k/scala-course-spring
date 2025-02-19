package kse.unit3.challenge

import java.beans.Expression
import scala.annotation.{tailrec, targetName}

object expressions:

  sealed trait Expression:
    def evaluate: Expression
    def substitute(variable: Variable, expression: Expression): Expression

  sealed trait Boolean extends Expression:
    val evaluate: Expression                                                 = this
    def substitute(variable: Variable, substitution: Expression): Expression = this

  type True = True.type
  case object True extends Boolean

  type False = False.type
  case object False extends Boolean

  case class Variable(name: String) extends Expression:
    val evaluate: Expression = this

    def substitute(variable: Variable, expression: Expression): Expression =
      if this == Variable then expression
      else this

  case class Negation(expression: Expression) extends Expression:

    lazy val evaluate: Expression = expression.evaluate match
      case True  => False
      case False => True
      case other => Negation(other)

    def substitute(variable: Variable, substitution: Expression): Expression =
      Negation(expression.substitute(variable, substitution))
    override def toString: String = s"!$expression"

  case class Conjunction(left: Expression, right: Expression) extends Expression:

    lazy val evaluate: Expression = (left.evaluate, right.evaluate) match
      case (True, True)            => True
      case (False, _) | (_, False) => False
      case (left, right)           => Conjunction(left, right)

    def substitute(variable: Variable, substitution: Expression): Expression =
      Conjunction(left.substitute(variable, substitution), right.substitute(variable, substitution))
    override def toString: String = s"$left ∧ $right"

  case class Disjunction(left: Expression, right: Expression) extends Expression:

    lazy val evaluate: Expression = (left.evaluate, right.evaluate) match
      case (True, _) | (_, True) => True
      case (False, False)        => False
      case (left, right)         => Disjunction(left, right)

    def substitute(variable: Variable, substitution: Expression): Expression =
      Disjunction(left.substitute(variable, substitution), right.substitute(variable, substitution))
    override def toString: String = s"$left ∨ $right"

  case class Implication(left: Expression, right: Expression) extends Expression:
    def evaluate: Expression                                                 = ???
    def substitute(variable: Variable, substitution: Expression): Expression = ???
    override def toString: String                                            = ???

  case class Equivalence(left: Expression, right: Expression) extends Expression:
    def evaluate: Expression                                                 = ???
    def substitute(variable: Variable, substitution: Expression): Expression = ???
    override def toString: String                                            = ???

  given Conversion[String, Variable] with
    def apply(str: String): Variable = Variable(str)

  extension (expr: Expression)

    @targetName("negation")
    infix def unary_! : Negation = ???

    @targetName("conjunction")
    infix def ∧(that: Expression): Conjunction = ???

    @targetName("disjunction")
    infix def ∨(that: Expression): Disjunction = ???

    @targetName("implication")
    infix def →(that: Expression): Implication = ???

    @targetName("equivalence")
    infix def ↔(that: Expression): Equivalence = ???
