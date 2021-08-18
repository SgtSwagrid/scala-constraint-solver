package solver

import Solver.*

object SudokuSolver:
  
  def solve(sudoku: Sudoku): LazyList[Sudoku] =
  
    val vars = Vec2.positions.map { pos => pos -> Variable (
      pos.toString,
      sudoku.digitAt(pos).map(Set(_)).getOrElse(Set.range(1, 9+1))
    )}.toMap
    
    val rows = Set.range(0, 9).map(y => Different(Vec2.row(y).map(vars)))
    val cols = Set.range(0, 9).map(x => Different(Vec2.col(x).map(vars)))
    val boxes = Vec2.regions.map(a => Different(Vec2.regions.map(b => vars(a*3 + b))))
    
    val solutions: Solutions[Int] =
      Problem(vars.values.toSet, rows ++ cols ++ boxes).solve
    
    solutions.map { soln =>
      val numbers =
        for (y <- 0 until 9; x <- 0 until 9)
        yield soln(vars(Vec2(x, y)))
      Sudoku(numbers*)
    }
    
  class Sudoku(numbers: Int*):
    
    def digitAt(pos: Vec2): Option[Int] =
      Some(numbers(pos.x + pos.y * 9)).filter(_ != 0)
    
    override def toString: String =
      numbers.map(n => if (n == 0) "*" else n)
        .grouped(27).map (
          _.grouped(9).map (
            _.grouped(3).map (
              _.mkString(" ")
            ).mkString(" | ")
          ).mkString("\n")
        ).mkString("\n", "\n------+-------+------\n", "\n")
  
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