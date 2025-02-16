package kse.unit2.challenge

import scala.annotation.{tailrec, targetName}

object booleans:

  case object True

  case object False

  type True    = True.type
  type False   = False.type
  type Boolean = True | False

  val negation: Boolean => Boolean =
    case True  => False
    case False => True

  val conjunction: (Boolean, => Boolean) => Boolean =
    (left, right) =>
      left match
        case False => False
        case True  => right

  val disjunction: (Boolean, => Boolean) => Boolean =
    (left, right) =>
      left match
        case True  => True
        case False => right

  val implication: (Boolean, => Boolean) => Boolean =
    (left, right) => disjunction(negation(left), right)

  val equivalence: (Boolean, => Boolean) => Boolean =
    case (left, right) => conjunction(implication(left, right), implication(right, left))

  extension (value: Boolean)

    @targetName("negation")
    infix def unary_! : Boolean = negation(value)

    @targetName("conjunction")
    infix def ∧(that: => Boolean): Boolean = conjunction(value, that)

    @targetName("disjunction")
    infix def ∨(that: => Boolean): Boolean = disjunction(value, that)

    @targetName("implication")
    infix def →(that: => Boolean): Boolean = implication(value, that)

    @targetName("equivalence")
    infix def ↔(that: => Boolean): Boolean = equivalence(value, that)

  def fold(operation: (Boolean, => Boolean) => Boolean, unit: Boolean)(list: List[Boolean]): Boolean =
    @tailrec
    def foldReq(list: List[Boolean], acc: Boolean): Boolean =
      list match
        case Nil          => acc
        case head :: tail => foldReq(tail, operation(head, acc))

    foldReq(list, unit)

  val conjunctionOfElements: List[Boolean] => Boolean = fold(conjunction, True)
  val disjunctionOfElements: List[Boolean] => Boolean = fold(disjunction, False)

  extension (booleans: List[Boolean])
    infix def conjunction: Boolean = conjunctionOfElements(booleans)
    infix def disjunction: Boolean = disjunctionOfElements(booleans)
