// Core Part about finding a single tour for a board using the
// Warnsdorf Rule
//==============================================================

object M4b {
// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary functions, feel free to 
// implement them, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  if (x._1 < 0 || x._1 >= dim || x._2 < 0 || x._2 >= dim) false
  else if (path.contains(x)) false
  else true
}

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val moves = List((x._1+1, x._2+2), (x._1+2, x._2+1), (x._1+2, x._2-1), (x._1+1, x._2-2), (x._1-1, x._2-2), (x._1-2, x._2-1), (x._1-2, x._2+1), (x._1-1, x._2+2))
  val legal = moves.filter(is_legal(dim, path, _))
  legal
}

def smaller_than(x1: Pos, x2: Pos, dim: Int, path: Path): Boolean =
  legal_moves(dim, path, x1).size < legal_moves(dim, path, x2).size


//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.


def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val legalMoves = legal_moves(dim, path, x)
  legalMoves.sortWith(smaller_than(_, _, dim, path))
}


//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 


def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {
  if (xs.isEmpty) None
  else {
    val x = xs.head
    val y = f(x)
    if (y.isDefined) y
    else first(xs.tail, f)
  }
}

def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
  if ((dim * dim == path.length) && (legal_moves(dim, List(path.head), path.head).contains(path.last))) Some(path)
  else {
    val onward_moves = ordered_moves(dim, path, path.last)
    first(onward_moves, (x: Pos) => first_closed_tour_heuristics(dim, path ::: List(x)))
  }
}


//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristics(dim: Int, path: Path) : Option[Path] =  {
  if (dim * dim == path.length) Some(path)
  else {
    val onward_moves = ordered_moves(dim, path, path.last)
    first(onward_moves, (x: Pos) => first_tour_heuristics(dim, path ::: List(x)))
  }
}



}
