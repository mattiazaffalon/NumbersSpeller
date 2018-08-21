package spellnumbers

trait NumbersSpeller {

  val MAX_SPELLABLE = 999999999

  case class GroupingNumber(number: Int, name: Option[String], tailSeparator: Option[String])

  private val groupingNumbers =
    GroupingNumber(1000000, Some("million"), Some(", ")) ::
    GroupingNumber(1000, Some("thousand"), Some(", ")) ::
      GroupingNumber(100, Some("hundred"), Some(" and ")) ::
      GroupingNumber(1, None, None) :: Nil

  private val zeroTo9 = (0 to 9).zip("zero" :: "one" :: "two" :: "three" :: "four" :: "five" :: "six" :: "seven" :: "eight" :: "nine" :: Nil).toMap

  private val tenTo19 = (10 to 19).zip("ten" :: "eleven" :: "twelve" :: "thirteen" :: "fourteen" :: "fifteen" :: "sixteen" :: "seventeen" :: "eighteen" :: "nineteen" :: Nil).toMap

  private val dozens = (20 to 90 by 10).zip("twenty" :: "thirty" :: "forty" :: "fifty" :: "sixty" :: "seventy" :: "eighty" :: "ninety" :: Nil)

  private val twentyTo99 = for ((number, name) <- dozens; i <- 0 to 9) yield (number + i, name + " " + zeroTo9.get(i).get)

  private val directNames = zeroTo9 ++ tenTo19 ++ twentyTo99

  def spell(number: Int): String = {

    def spellFromLeft(number: Int, groupIdx: Int): Option[String] = {

      if (number == 0 || groupIdx == 3)
        Some(directNames(number))
      else {
        val groupNum = groupingNumbers(groupIdx)

        val howMany = number / groupNum.number match {
          case q if q > 0 => for (num <- spellFromLeft(q, 2); name <- groupNum.name) yield (num + " " + name)
          case _ => None
        }

        val reminder = number % groupNum.number match {
          case r if r > 0 => spellFromLeft(r, groupIdx + 1)
          case _ => None
        }

        howMany match {
          case Some(h) => {
            reminder match {
              case Some(r) => Some(h + groupNum.tailSeparator.get + r)
              case None => howMany
            }
          }
          case _ => reminder
        }
      }
    }

    if (number > MAX_SPELLABLE) throw new IllegalArgumentException(s"Max number spellable is $MAX_SPELLABLE")

    spellFromLeft(number, 0).get
  }
}
