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

// ADD YOUR CODE BELOW
//======================

//(1) 
def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
    val (a, b) = x
    if (a >= 0 && a < dim && b >= 0 && b < dim && !path.contains(x)) true else false
}


//(2) 
def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val legal_moves = List((1,2),(2,1),(2,-1),(1,-2),(-1,-2),(-2,-1),(-2,1),(-1,2))
    legal_moves.map{ case (dx, dy) => ( x._1 + dx , x._2 + dy) }.filter(move => is_legal(dim, path, move))
}

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {
    if (xs.isEmpty) None
    else {
        val res = f(xs.head)
        if (res.isDefined) res
        else first(xs.tail, f)
    }
}

// (3) 
def count_tours(dim: Int, path: Path) : Int = {
    if(path.length == dim*dim && path.distinct.length == dim*dim) 1
    else legal_moves(dim,path,path.head).map(move => count_tours(dim, move :: path)).sum
}

def enum_tours(dim: Int, path: Path) : List[Path] = {
    if(path.length == dim*dim && path.distinct.length == dim*dim) List(path)
    else legal_moves(dim,path,path.head).flatMap(move => enum_tours(dim, move :: path))
}

//(6) 
def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    legal_moves(dim, path, x).sortBy(p => legal_moves(dim, p :: path, p).length)
}


//(7) 
def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
    first(ordered_moves(dim, path, path.head), p => {
        val res = enum_tours(dim,p :: path)
        if (res.nonEmpty) Some(res.head) else None
    })
}


//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristics(dim: Int, path: Path) : Option[Path] =  {
    first(ordered_moves(dim, path, path.head), p => {
        val res = enum_tours(dim, p :: path)
        if (res.nonEmpty) Some(res.head) else None
    })
}



}





// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
