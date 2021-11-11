package sudoku

case class Vec2(x: Int, y: Int):
  def + (v: Vec2) = Vec2(x + v.x, y + v.y)
  
  def * (s: Int) = Vec2(x * s, y * s)

object Vec2:
  
  val positions: Set[Vec2] = for
    x <- Set.range(0, 9)
    y <- Set.range(0, 9)
  yield Vec2(x, y)
  
  val regions = for
    x <- Set.range(0, 3)
    y <- Set.range(0, 3)
  yield Vec2(x, y)
  
  def col(x: Int) = Set.range(0, 9).map(Vec2(x, _))
  
  def row(y: Int) = Set.range(0, 9).map(Vec2(_, y))
