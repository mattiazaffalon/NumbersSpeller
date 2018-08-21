package spellnumbers

object Main extends NumbersSpeller {
  def main(args: Array[String]) = {
    assert(args.size == 1, "Call the program with the integer number to be spelled")

    println(spell(args(0).toInt))
  }
}
