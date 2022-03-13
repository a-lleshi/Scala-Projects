// Finding a single tour on a "mega" board
//=========================================

object M4c {

// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala                                   !!! 
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
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


def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val legalMoves = legal_moves(dim, path, x)
  legalMoves.sortWith(smaller_than(_, _, dim, path))
}

//(9) Implement a function that searches for a 
//    you have to be careful to write a tail-recursive version as this 
//    function will be called with dimensions of up to 70 * 70
//    and starting field (0, 0). It has to produce a solution within
//    30 seconds.


def tour_on_mega_board(dim: Int, path: Path) : Option[Path] = {
  if (path.size == dim*dim) Some(path)
  else {
    val onward_moves = ordered_moves(dim, path, path.last)
    tour_on_mega_board(dim, path ::: List(onward_moves.head))
  }
}

}
