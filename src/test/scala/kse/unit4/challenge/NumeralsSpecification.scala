package kse.unit4.challenge

import kse.unit4.challenge.generators.given
import kse.unit4.challenge.numerals.*
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean, throws}
import org.scalacheck.Test.Parameters

object NumeralsSpecification extends Properties("Numerals"):

  override def overrideParameters(p: Parameters): Parameters =
    p.withMinSuccessfulTests(50).withMaxDiscardRatio(100)

  property("Zero == Zero") = (Zero == Zero)

  property("Zero < value") = forAll: (value: Numeral) =>
    if value.isZero then (Zero < value) == false
    else (Zero < value) == true

  property("succesor(n) == n+1") = forAll: (value: Numeral) =>
    value.successor.toInt == value.toInt + 1

  property("value + 0 == value") = forAll: (value: Numeral) =>
    value + Zero == value

  property("left + right == right + left") = forAll: (left: Numeral, right: Numeral) =>
    left + right == right + left

  property("a+(b+c) == (a+b)+c") = forAll: (a: Numeral, b: Numeral, c: Numeral) =>
    (a + (b + c)).toInt == ((a + b) + c).toInt

  property("reflexivity") = forAll: (left: Numeral, right: Numeral) =>
    (left.toInt == right.toInt) == (left == right)

  property("left - right == left - right") = forAll: (left: Numeral, right: Numeral) =>
    try (left.toInt - right.toInt) == (left - right).toInt
    catch case e: UnsupportedOperationException => true

  property("value - zero == value") = forAll: (value: Numeral) =>
    (value - Zero) == value
end NumeralsSpecification
