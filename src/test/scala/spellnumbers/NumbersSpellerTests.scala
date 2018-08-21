package spellnumbers

import org.scalatest.{FunSpec, FunSuite, Matchers}

class NumbersSpellerTests extends FunSuite with Matchers with NumbersSpeller {
  test("zero") {
    spell(0) should be("zero")
  }

  test("spell 1") {
    spell(1) should be("one")
  }

  test("spell 10") {
    spell(10) should be("ten")
  }

  test("spell 101") {
    spell(101) should be("one hundred and one")
  }

  test("spell 1001") {
    spell(1001) should be("one thousand and one")
  }

  test("spell 1101") {
    spell(1101) should be("one thousand, one hundred and one")
  }

  test("spell 1000000") {
    spell(1000000) should be ("one million")
  }

  test("spell 1000001") {
    spell(1000001) should be ("one million and one")
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
