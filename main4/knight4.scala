// Part 4 about finding a single tour on "mutilated" chessboards
//==============================================================

object M4d { 

// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala or knight3.scala                  !!! 
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

type Pos = (Int, Int)
type Path = List[Pos]

def print_board(dim: Int, path: Path): Unit = {
  println()
  for (i <- 0 until dim) {
    for (j <- 0 until dim) {
      print(f"${path.reverse.indexOf((i, j))}%4.0f ")
    }
    println()
  } 
}

// ADD YOUR CODE BELOW
//======================

// (10)
def one_tour_pred(dim: Int, path: Path, n: Int, pred: Pos => Boolean): Option[Path] = {
    if (path.size == n) return Some(path) 
    val legal_moves = legal_moves(dim, path, path.head).filter(pred) 
    legal_moves.foreach { move =>
        val result = one_tour_pred(dim, move :: path, n, pred)
        if (result.isDefined) return result 
    }
    None 
}


}





// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
