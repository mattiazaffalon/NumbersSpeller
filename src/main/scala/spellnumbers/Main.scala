package spellnumbers

object Main extends NumbersSpeller {
  def main(args: Array[String]) = {
    assert(args.size == 1)

    println(spell(args(0).toInt))
  }
}
