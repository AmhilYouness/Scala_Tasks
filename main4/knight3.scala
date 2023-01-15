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

// ADD YOUR CODE BELOW
//======================


//(9) 
def tour_on_mega_board(dim: Int, path: Path) : Option[Path] = {
    def tour_on_mega_board_helper(path: Path, steps: Int): Option[Path] = {
        if (steps == dim * dim) return Some(path)
        if (path.length == 0) return None
        val nextMoves = ordered_moves(dim, path, path.head).filterNot(path.contains)
        nextMoves.foreach { move => 
            val res = tour_on_mega_board_helper(move :: path, steps + 1)
            if (res.isDefined) return res
        }
        None
    }
    tour_on_mega_board_helper(path, 0)
}





// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
