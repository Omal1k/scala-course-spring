package kse.unit2.challenge

import kse.unit2.challenge.booleans.*

object BooleanTest:

  def main(args: Array[String]): Unit =
    println("Testing Boolean operations:")

    // Negation
    println(s"!True  = ${!True}")  // Expected: False
    println(s"!False = ${!False}") // Expected: True

    // Conjunction (AND)
    println(s"True ∧ True   = ${True ∧ True}")   // Expected: True
    println(s"True ∧ False  = ${True ∧ False}")  // Expected: False
    println(s"False ∧ True  = ${False ∧ True}")  // Expected: False
    println(s"False ∧ False = ${False ∧ False}") // Expected: False

    // Short-circuit check for conjunction
    try {
      println(s"False ∧ (throw new Exception('Should not be thrown')) = ${False ∧ (throw new Exception("Should not be thrown"))}")
    } catch {
      case e: Exception => println(s"Short-circuiting test passed (Exception not thrown): ${e.getMessage}")
    }

    // Disjunction (OR)
    println(s"True ∨ True   = ${True ∨ True}")   // Expected: True
    println(s"True ∨ False  = ${True ∨ False}")  // Expected: True
    println(s"False ∨ True  = ${False ∨ True}")  // Expected: True
    println(s"False ∨ False = ${False ∨ False}") // Expected: False

    // Implication
    println(s"True → True   = ${True → True}") // Expected: True
    println(s"True → False  = ${True → False}") // Expected: False
    println(s"False → True  = ${False → True}") // Expected: True
    println(s"False → False = ${False → False}") // Expected: True

    // Equivalence (IFF)
    println(s"True ↔ True   = ${True ↔ True}")   // Expected: True
    println(s"True ↔ False  = ${True ↔ False}")  // Expected: False
    println(s"False ↔ True  = ${False ↔ True}")  // Expected: False
    println(s"False ↔ False = ${False ↔ False}") // Expected: True

    // Conjunction and disjunction over lists
    val list1 = List(True, True, False, True)
    val list2 = List(False, False, False)

    println(s"should be False, but = ${conjunctionOfElements(list1)}")
