package spellnumbers

import org.scalatest.{FunSpec, FunSuite, Matchers}

class NumbersSpellerTests extends FunSuite with Matchers with NumbersSpeller {
  test("spell 0") {
    spell(0) should be("zero")
  }

  test("spell 1") {
    spell(1) should be("one")
  }

  test("spell 105") {
    spell(105) should be("one hundred and five")
  }

  test("spell 4567") {
    spell(4567) should be("four thousand, five hundred and sixty seven")
  }

  test("spell 1000000") {
    spell(1000000) should be ("one million")
  }

  test("spell 56945781") {
    spell(56945781) should be ("fifty six million, nine hundred and forty five thousand, seven hundred and eighty one")
  }

  test("throws an exception when number is too big") {
    assertThrows[IllegalArgumentException] {
      spell(1000000000)
    }
  }
}
