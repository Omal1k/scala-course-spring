package kse.unit4.challenge

import kse.unit4.challenge.generators.given
import kse.unit4.challenge.numerals.*
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean, throws}
import org.scalacheck.Test.Parameters

object NumeralsSpecification extends Properties("Numerals"):

  override def overrideParameters(p: Parameters): Parameters =
    p.withMinSuccessfulTests(50).withMaxDiscardRatio(100)

  property("Zero < value") = forAll: (value: Numeral) =>
    (Zero < value) == !value.isZero

  property("succesor(n) == n+1") = forAll: (value: Numeral) =>
    value.successor == value + Successor(Zero)

  property("predecessor(successor(value)) == value") = forAll: (value: Numeral) =>
    value.successor.predecessor == value

  property("value + 0 == value") = forAll: (value: Numeral) =>
    value + Zero == value

  property("0 + value == value") = forAll: (value: Numeral) =>
    Zero + value == value

  property("left + right == right + left") = forAll: (left: Numeral, right: Numeral) =>
    left + right == right + left

  property("a + (b + c) == (a + b) + c") = forAll: (a: Numeral, b: Numeral, c: Numeral) =>
    (a + (b + c)) == ((a + b) + c)

  property("reflexivity") = forAll: (left: Numeral, right: Numeral) =>
    (left == right) == (left == right)

  property("symmetry") = forAll: (left: Numeral, right: Numeral) =>
    (left == right) == (right == left)

  property("transitivity") = forAll: (a: Numeral, b: Numeral, c: Numeral) =>
    (!((a == b) && (b == c))) || (a == c)

  property("value - zero == value") = forAll: (value: Numeral) =>
    (value - Zero) == value

  property("value - value == zero") = forAll: (value: Numeral) =>
    (value - value) == Zero

  property("a - b not negative") = forAll: (a: Numeral, b: Numeral) =>
    (a - b).isZero || (a >= b)
end NumeralsSpecification
