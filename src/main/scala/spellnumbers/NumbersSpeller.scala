package spellnumbers

trait NumbersSpeller {

  val MAX_SPELLABLE = 999999999

  case class GroupingNumber(number: Int, name: Option[String])

  private val groupingNumbers =
    GroupingNumber(1000000, Some("million")) ::
    GroupingNumber(1000, Some("thousand")) ::
      GroupingNumber(100, Some("hundred")) ::
      GroupingNumber(1, None) :: Nil

  private val zeroTo9  = (0 to 9).zip("zero" :: "one" :: "two" :: "three" :: "four" :: "five" :: "six" :: "seven" :: "eight" :: "nine" :: Nil).toMap

  private val tenTo19 = (10 to 19).zip("ten" :: "eleven" :: "twelve" :: "thirteen" :: "fourteen" :: "fifteen" :: "sixteen" :: "seventeen" :: "eighteen" :: "nineteen" :: Nil).toMap

  private val dozens = (20 to 90 by 10).zip("twenty" :: "thirty" :: "forty" :: "fifty" :: "sixty" :: "seventy" :: "eighty" :: "ninety" :: Nil).toMap

  private val betweenDozens = (for ((number, name) <- dozens; i <- 1 to 9) yield (number + i, name + " " + zeroTo9.get(i).get)).toMap

  private val directNames = zeroTo9 ++ tenTo19 ++ dozens ++ betweenDozens

  private def joinNumbers(ns: Seq[String]): String = {
    ns.zipWithIndex.foldLeft("")((z, n) => {
      val number = n._1
      val idx = n._2

      val sep: Option[String] = if (z == "") None
      else if (idx == ns.size - 1 && !number.contains("and")) Some(" and ")
      else Some(", ")

      z + sep.getOrElse("") + number
    })
  }

  def spell(number: Int): String = {

    def spellFromLeft(number: Int, groupIdx: Int): List[String] = {

      if (number == 0 || groupIdx == 3)
        directNames.get(number).get :: Nil
      else {
        val groupNum = groupingNumbers(groupIdx)

        val howMany: Option[String] = number / groupNum.number match {
          case q if q > 0 => Some(joinNumbers(spellFromLeft(q, 2)) + groupNum.name.map(" " + _).getOrElse(""))
          case _ => None
        }

        val reminder: Option[String] = number % groupNum.number match {
          case r if r > 0 => Some(joinNumbers(spellFromLeft(r, groupIdx + 1)))
          case _ => None
        }

        howMany match {
          case Some(h) => reminder match {
            case Some(r) => h :: r :: Nil
            case _ => h :: Nil
          }
          case _ => reminder match {
            case Some(r) => r :: Nil
            case _ => Nil
          }
        }
      }
    }

    if (number > MAX_SPELLABLE) throw new IllegalArgumentException(s"Max number spellable is $MAX_SPELLABLE")

    joinNumbers(spellFromLeft(number, 0))
  }
}
