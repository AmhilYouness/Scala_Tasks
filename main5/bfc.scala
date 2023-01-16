// Main Part 5 about a "Compiler" for the Brainf*** language
//============================================================


object M5b {

// !!! Copy any function you need from file bf.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


// DEBUGGING INFORMATION FOR COMPILERS!!!
//
// Compiler, even real ones, are fiendishly difficult to get
// to produce correct code. One way to debug them is to run
// example programs ``unoptimised''; and then optimised. Does
// the optimised version still produce the same result?


// for timing purposes
def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}


type Mem = Map[Int, Int]

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================

// (6) 
// def jtable(pg: String) : Map[Int, Int] = ???


def jtable(pg: String): Map[Int, Int] = {
    var table = Map[Int, Int]()
    var stack = List[Int]()
    for (i <- 0 until pg.length) {
        if (pg(i) == '[') stack = i :: stack
        else if (pg(i) == ']') {
            val start = stack.head
            stack = stack.tail
            table += (i -> (start + 1))
            table += (start -> (i + 1))
        }
    }
    table
}


// testcase
//
// jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")
// =>  Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)


def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Map[Int, Int]): Map[Int, Int] = {
    if (pc < 0 || pc >= pg.length) return mem
    val (newPc, newMp, newMem) = pg(pc) match {
        case '>' => (pc + 1, mp + 1, mem)
        case '<' => (pc + 1, mp - 1, mem)
        case '+' => (pc + 1, mp, mem + (mp -> (mem.getOrElse(mp, 0) + 1)))
        case '-' => (pc + 1, mp, mem + (mp -> (mem.getOrElse(mp, 0) - 1)))
        case '.' => (pc + 1, mp, mem)
        case ',' => (pc + 1, mp, mem)
        case '[' => if (mem.getOrElse(mp, 0) == 0) (tb(pc), mp, mem) else (pc + 1, mp, mem)
        case ']' => if (mem.getOrElse(mp, 0) != 0) (tb(pc), mp, mem) else (pc + 1, mp, mem)
    }
    compute2(pg, tb, newPc, newMp, newMem)
}

def run2(pg: String, m: Map[Int, Int] = Map()): Map[Int, Int] = {
    val jumpTable = jtable(pg)
    compute2(pg, jumpTable, 0, 0, m)
}


// testcases
// time_needed(1, run2(load_bff("benchmark.bf")))
// time_needed(1, run2(load_bff("sierpinski.bf")))



// (7) 

import scala.util.matching.Regex

def optimise(s: String): String = {
    val deadCode = "[^<>+-.,\\[\\]]".r
    val loop = "\\[\\s*-\\s*\\]".r
    deadCode.replaceAllIn(s, "") // remove dead code
        .replaceAll(loop.toString, "0") // replace [ - ] with 0
}

def compute3(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = ???

def run3(pg: String, m: Mem = Map()) = ???


// testcases
//
// optimise(load_bff("benchmark.bf"))          // should have inserted 0's
// optimise(load_bff("mandelbrot.bf")).length  // => 11203
// 
// time_needed(1, run3(load_bff("benchmark.bf")))



// (8)  
def combine(s: String) : String = ???

// testcase
// combine(load_bff("benchmark.bf"))

def compute4(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = ???

// should call first optimise and then combine on the input string
//
def run4(pg: String, m: Mem = Map()) = ???


// testcases
// combine(optimise(load_bff("benchmark.bf"))) // => """>A+B[<A+M>A-A]<A[[....."""

// testcases (they should now run much faster)
// time_needed(1, run4(load_bff("benchmark.bf")))
// time_needed(1, run4(load_bff("sierpinski.bf"))) 
// time_needed(1, run4(load_bff("mandelbrot.bf")))


}





// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
