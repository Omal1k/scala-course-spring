package kse.unit3.challenge

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
      if this == variable then expression
      else this

  case class Negation(expression: Expression) extends Expression:

    lazy val evaluate: Expression =
      expression.evaluate match
        case True  => False
        case False => True
        case other => Negation(other.evaluate)

    def substitute(variable: Variable, substitution: Expression): Expression =
      Negation(expression.substitute(variable, substitution))
    override def toString: String = s"!$expression"

  case class Conjunction(left: Expression, right: Expression) extends Expression:

    lazy val evaluate: Expression =
      (left.evaluate, right.evaluate) match
        case (True, expr)            => expr
        case (expr, True)            => expr
        case (False, _) | (_, False) => False
        case (left, right)           => Conjunction(left, right)

    def substitute(variable: Variable, substitution: Expression): Expression =
      Conjunction(left.substitute(variable, substitution), right.substitute(variable, substitution))
    override def toString: String = s"($left ∧ $right)"

  case class Disjunction(left: Expression, right: Expression) extends Expression:

    lazy val evaluate: Expression =
      (left.evaluate, right.evaluate) match
        case (True, _) | (_, True) => True
        case (False, expr)         => expr
        case (expr, False)         => expr
        case (left, right)         => Disjunction(left, right)

    def substitute(variable: Variable, substitution: Expression): Expression =
      Disjunction(left.substitute(variable, substitution), right.substitute(variable, substitution))
    override def toString: String = s"($left ∨ $right)"

  case class Implication(left: Expression, right: Expression) extends Expression:
    lazy val evaluate: Expression = Disjunction(Negation(left).evaluate, right.evaluate).evaluate

    def substitute(variable: Variable, substitution: Expression): Expression =
      Implication(left.substitute(variable, substitution), right.substitute(variable, substitution))
    override def toString: String = s"($left → $right)"

  case class Equivalence(left: Expression, right: Expression) extends Expression:
    lazy val evaluate: Expression = Conjunction(Implication(left, right).evaluate, Implication(right, left).evaluate).evaluate

    def substitute(variable: Variable, substitution: Expression): Expression =
      Equivalence(left.substitute(variable, substitution), right.substitute(variable, substitution))
    override def toString: String = s"($left ↔ $right)"

  given Conversion[String, Variable] with
    def apply(str: String): Variable = Variable(str)

  extension (expr: Expression)

    @targetName("negation")
    infix def unary_! : Negation = Negation(expr)

    @targetName("conjunction")
    infix def ∧(that: Expression): Conjunction = Conjunction(expr, that)

    @targetName("disjunction")
    infix def ∨(that: Expression): Disjunction = Disjunction(expr, that)

    @targetName("implication")
    infix def →(that: Expression): Implication = Implication(expr, that)

    @targetName("equivalence")
    infix def ↔(that: Expression): Equivalence = Equivalence(expr, that)
