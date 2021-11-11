package sudoku

class Sudoku(numbers: Int*):
  
  def digitAt(pos: Vec2): Option[Int] =
    Some(numbers(pos.x + pos.y * 9)).filter(_ != 0)
  
  override def toString: String =
    numbers.map(n => if (n == 0) "*" else n)
      .grouped(27).map(
      _.grouped(9).map(
        _.grouped(3).map(
          _.mkString(" ")
        ).mkString(" | ")
      ).mkString("\n")
    ).mkString("\n", "\n------+-------+------\n", "\n")