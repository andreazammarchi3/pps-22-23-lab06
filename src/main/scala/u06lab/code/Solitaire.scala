package u06lab.code

import javax.swing.text.Position

object Solitaire extends App:
  private type Position = (Int, Int)
  private type Solution = List[Position]
  private def placeMarks(width: Int, height: Int)(init: Position): Iterable[Solution] =
    def _placeMarks(n: Int): Iterable[Solution] = n match
      case 1 => Seq(List(init)).view
      case _ =>
        for
          solution <- _placeMarks(n - 1)
          i <- 0 until width
          j <- 0 until height
          newPos = (i, j)
          if !solution.contains(newPos)
          if isSafe(newPos, solution.head)
        yield newPos :: solution

    _placeMarks(width * height)

  private def isSafe(p1: Position, p2: Position): Boolean =
    (math.abs(p1._1 - p2._1) == 2 && math.abs(p1._2 - p2._2) == 2)
      || (math.abs(p1._1 - p2._1) == 3 && math.abs(p1._2 - p2._2) == 0)
      || (math.abs(p1._1 - p2._1) == 0 && math.abs(p1._2 - p2._2) == 3)

  private def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
                    number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  private val size = (5, 7)
  private val center: Position = (size._1 / 2, size._2 / 2)
  private val solutions = placeMarks(size._1, size._2)(center)
  for s <- solutions
    do println(render(solution = s, width = size._1, height = size._2) + "\n")
