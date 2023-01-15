// Main Part 5 about an Interpreter for 
// the Brainf*** language
//==============================================


object M5a {

// representation of BF memory 

type Mem = Map[Int, Int]

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================

// (1)
def load_bff(name: String) : String = {
    try {
        val file = Source.fromFile(name)
        val content = file.mkString
        file.close()
        content
    } catch {
        case _: Exception => ""
    }
}

// (2) 

def sread(mem: Mem, mp: Int) : Int = mem.getOrElse(mp, 0)

def write(mem: Mem, mp: Int, v: Int) : Mem = mem + (mp -> v)

// (3) 

def jumpRight(prog: String, pc: Int, level: Int): Int = {
    var currentLevel = level
    var currentPc = pc
    while (currentPc < prog.length) {
        if (prog(currentPc) == '[') currentLevel += 1
        else if (prog(currentPc) == ']') {
            if (currentLevel == 0) return currentPc + 1
            else currentLevel -= 1
        }
        currentPc += 1
    }
    -1
}

def jumpLeft(prog: String, pc: Int, level: Int): Int = {
    var currentLevel = level
    var currentPc = pc
    while (currentPc >= 0) {
        if (prog(currentPc) == ']') currentLevel += 1
        else if (prog(currentPc) == '[') {
            if (currentLevel == 0) return currentPc + 1
            else currentLevel -= 1
        }
        currentPc -= 1
    }
    -1
}



// testcases
//jumpRight("""--[..+>--],>,++""", 3, 0)         // => 10
//jumpLeft("""--[..+>--],>,++""", 8, 0)          // => 3
//jumpRight("""--[..[+>]--],>,++""", 3, 0)       // => 12
//jumpRight("""--[..[[-]+>[.]]--],>,++""", 3, 0) // => 18
//jumpRight("""--[..[[-]+>[.]]--,>,++""", 3, 0)  // => 22 (outside)
//jumpLeft("""[******]***""", 7, 0)              // => -1 (outside)



// (4) 

def compute(prog: String, pc: Int, mp: Int, mem: Map[Int, Int]): Map[Int, Int] = {
    if (pc < 0 || pc >= prog.length) return mem
    val (newPc, newMp, newMem) = prog(pc) match {
        case '>' => (pc + 1, mp + 1, mem)
        case '<' => (pc + 1, mp - 1, mem)
        case '+' => (pc + 1, mp, mem + (mp -> (mem.getOrElse(mp, 0) + 1)))
        case '-' => (pc + 1, mp, mem + (mp -> (mem.getOrElse(mp, 0) - 1)))
        case '.' => (pc + 1, mp, mem)
        case ',' => (pc + 1, mp, mem)
        case '[' => if (mem.getOrElse(mp, 0) == 0) (jumpRight(prog, pc + 1, 0), mp, mem) else (pc + 1, mp, mem)
        case ']' => if (mem.getOrElse(mp, 0) != 0) (jumpLeft(prog, pc - 1, 0), mp, mem) else (pc + 1, mp, mem)
    }
    compute(prog, newPc, newMp, newMem)
}

def run(prog: String, m: Map[Int, Int] = Map()) = compute(prog, 0, 0, m)


// (5)
def generate(msg: List[Char]): String = {
    val sb = new StringBuilder
    for (c <- msg) {
        val asciiCode = c.toInt
        for (i <- 1 to asciiCode) sb.append("+")
        sb.append(".[-]")
    }
    sb.toString
}



// some sample bf-programs collected from the Internet
//=====================================================


// some contrived (small) programs
//---------------------------------

// clears the 0-cell
//run("[-]", Map(0 -> 100))    // Map will be 0 -> 0

// moves content of the 0-cell to 1-cell
//run("[->+<]", Map(0 -> 10))  // Map will be 0 -> 0, 1 -> 10


// copies content of the 0-cell to 2-cell and 4-cell
//run("[>>+>>+<<<<-]", Map(0 -> 42))    // Map(0 -> 0, 2 -> 42, 4 -> 42)


// prints out numbers 0 to 9
//run("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")


// some more "useful" programs
//-----------------------------

// hello world program 1
//run("""++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++
//       ..+++.>>.<-.<.+++.------.--------.>>+.>++.""")

// hello world program 2
//run("""++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>+
//       +.<<+++++++++++++++.>.+++.------.--------.>+.>.""")

// hello world program 3
//run("""+++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..
//       +++.>-.------------.<++++++++.--------.+++.------.--------.>+.""")

 
// draws the Sierpinski triangle
//run(load_bff("sierpinski.bf"))


//fibonacci numbers below 100
//run("""+++++++++++
//      >+>>>>++++++++++++++++++++++++++++++++++++++++++++
//      >++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>
//      +<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-
//      <-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<
//      -]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]
//      >[<<+>>[-]]<<<<<<<]>>>>>[+++++++++++++++++++++++++
//      +++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++
//      ++++++++++++++++++++++++++++++++++++++++++++.[-]<<
//      <<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<
//      [-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]""")

//outputs the square numbers up to 10000
//run("""++++[>+++++<-]>[<+++++>-]+<+[>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+
//       >>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]
//       <<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]""")


// calculates 2 to the power of 6 
//(example from a C-to-BF compiler at https://github.com/elikaski/BF-it)
//run(""">>[-]>[-]++>[-]++++++><<<>>>>[-]+><>[-]<<[-]>[>+<<+>-]>[<+>-]
//       <><[-]>[-]<<<[>>+>+<<<-]>>>[<<<+>>>-][-]><<>>[-]>[-]<<<[>>[-]
//       <[>+>+<<-]>[<+>-]+>[[-]<-<->>]<<<-]>>[<<+>>-]<<[[-]>[-]<<[>+>
//       +<<-]>>[<<+>>-][-]>[-]<<<<<[>>>>+>+<<<<<-]>>>>>[<<<<<+>>>>>-]
//       <<>>[-]>[-]<<<[>>>+<<<-]>>>[<<[<+>>+<-]>[<+>-]>-]<<<>[-]<<[-]
//       >[>+<<+>-]>[<+>-]<><[-]>[-]<<<[>>+>+<<<-]>>>-[<<<+>>>-]<[-]>[-]
//       <<<[>>+>+<<<-]>>>[<<<+>>>-][-]><<>>[-]>[-]<<<[>>[-]<[>+>+<<-]>
//       [<+>-]+>[[-]<-<->>]<<<-]>>[<<+>>-]<<][-]>[-]<<[>+>+<<-]>>[<<+>
//       >-]<<<<<[-]>>>>[<<<<+>>>>-]<<<<><>[-]<<[-]>[>+<<+>-]>[<+>-]<>
//       <[-]>[-]>[-]<<<[>>+>+<<<-]>>>[<<<+>>>-]<<>>[-]>[-]>[-]>[-]>[-]>
//       [-]>[-]>[-]>[-]>[-]<<<<<<<<<<>>++++++++++<<[->+>-[>+>>]>[+[-<+
//       >]>+>>]<<<<<<]>>[-]>>>++++++++++<[->-[>+>>]>[+[-<+>]>+>>]<<<<<
//       ]>[-]>>[>++++++[-<++++++++>]<.<<+>+>[-]]<[<[->-<]++++++[->++++
//       ++++<]>.[-]]<<++++++[-<++++++++>]<.[-]<<[-<+>]<<><<<""")



// a Mandelbrot set generator in brainf*** written by Erik Bosman
// (http://esoteric.sange.fi/brainfuck/utils/mandelbrot/)
//
//run(load_bff("mandelbrot.bf"))


// a benchmark program (counts down from 'Z' to 'A')
//
//run(load_bff("benchmark.bf"))

// calculates the Collatz series for numbers from 1 to 30
//
//run(load_bff("collatz.bf"))

}





// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
