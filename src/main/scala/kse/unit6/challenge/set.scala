package kse.unit6.challenge

import kse.unit6.challenge.order.{Order, *, given}
import scala.annotation.targetName

object set:

  trait Set[+A]:

    infix def forAll(predicate: A => Boolean): Boolean

    infix def exists(predicate: A => Boolean): Boolean

    infix def contains[B >: A: Order](x: B): Boolean

    infix def include[B >: A: Order](x: B): Set[B]

    // Optional from the Unit 5. If you haven't implement it in Unit 5 then skip it
    //    infix def remove[B >: A: Order](x: B): Set[B]

    @targetName("union")
    infix def ∪[B >: A: Order](that: Set[B]): Set[B]

    @targetName("intersection")
    infix def ∩[B >: A: Order](that: Set[B]): Set[B]

    // Optional from the Unit 5. If you haven't implement it in Unit 5 then skip it
    //    @targetName("difference")
    //    infix def \[B >: A: Order](that: Set[B]): Set[B]

    // Optional from the Unit 5. If you haven't implement it in Unit 5 then skip it
    //    @targetName("symmetric difference")
    //    infix def ∆[B >: A: Order](that: Set[B]): Set[B] = ???

  end Set

  type Empty = Empty.type

  // TODO: Remind about type system
  case object Empty extends Set[Nothing]:

    infix def forAll(predicate: Nothing => Boolean): Boolean = true

    infix def exists(predicate: Nothing => Boolean): Boolean = false

    infix def contains[B: Order](x: B): Boolean = false

    infix def include[B: Order](x: B): Set[B] = NonEmpty(Empty, x, Empty)

    // Optional from the Unit 5. If you haven't implement it in Unit 5 then skip it
    //    infix def remove[B: Order](x: B): Set[B] = ???

    @targetName("union")
    infix def ∪[B: Order](that: Set[B]): Set[B] = that

    @targetName("intersection")
    infix def ∩[B: Order](that: Set[B]): Set[B] = Empty

    // Optional from the Unit 5. If you haven't implement it in Unit 5 then skip it
    //    @targetName("difference")
    //    infix def \[B: Order](that: Set[B]): Set[B] = ???

    override def toString: String = "[*]"

    override def equals(obj: Any): Boolean =
      obj match
        case _: Empty.type => true
        case _             => false

  end Empty

  case class NonEmpty[A](left: Set[A], element: A, right: Set[A]) extends Set[A]:

    infix def forAll(predicate: A => Boolean): Boolean =
      left.forAll(predicate) && predicate(element) && right.forAll(predicate)

    infix def exists(predicate: A => Boolean): Boolean =
      left.exists(predicate) || predicate(element) || right.exists(predicate)

    infix def contains[B >: A: Order](x: B): Boolean =
      if x == element then true
      else if x > element then right.contains(x)
      else left.contains(x)

    infix def include[B >: A: Order](x: B): Set[B] =
      if x == element then this
      else if x > element then NonEmpty(left, element, right.include(x))
      else NonEmpty(left.include(x), element, right)

    // Optional from the Unit 5. If you haven't implement it in Unit 5 then skip it
    //    infix def remove[B >: A: Order](x: B): Set[B] = ???

    @targetName("union")
    infix def ∪[B >: A: Order](that: Set[B]): Set[B] = left ∪ (right ∪ that.include(element))

    @targetName("intersection")
    infix def ∩[B >: A: Order](that: Set[B]): Set[B] =
      if that.contains(element) then NonEmpty(left ∩ that, element, right ∩ that)
      else (left ∩ that) ∪ (right ∩ that)

    // Optional from the Unit 5. If you haven't implement it in Unit 5 then skip it
    //    @targetName("difference")
    //    infix def \[B >: A: Order](that: Set[B]): Set[B] = ???

    override def toString: String = s"[$left - [$element] - $right]"

    override def equals(obj: Any): Boolean =
      obj match
        case s @ NonEmpty(left, v, right) => s.forAll(element => this.exists(_ == element))
        case _                            => false

  end NonEmpty
