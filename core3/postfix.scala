// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

object C3a {

// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
		"-" -> 1,
		"*" -> 2,
		"/" -> 2)

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// ADD YOUR CODE BELOW
//======================


// (1) 
def is_op(op: String) : Boolean = ops.contains(op)

def prec(op1: String, op2: String) : Boolean = precs(op1) >= precs(op2)

def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = toks match {
  case Nil => st.reverse ++ out
  case h :: t => h match {
    case "+" | "-" | "*" | "/" =>
      val popped = st.takeWhile(is_op)
      syard(t, st.drop(popped.length), popped ++ out)
    case "(" =>
      syard(t, h :: st, out)
    case ")" =>
      val popped = st.takeWhile(_ != "(")
      syard(t, st.drop(popped.length + 1), popped ++ out)
    case _ =>
      syard(t, st, out :+ h)
  }
}



// test cases
//syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
//syard(split("10 + 12 * 33"))       // 10 12 33 * +
//syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
//syard(split("5 + 7 / 2"))          // 5 7 2 / +
//syard(split("5 * 7 / 2"))          // 5 7 * 2 /
//syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +

//syard(split("3 + 4 + 5"))           // 3 4 + 5 +
//syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
//syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
//syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +

 
// (2) 
def compute(toks: Toks, st: List[Int] = Nil) : Int ={
  if (toks.isEmpty) return st.head
  
  val h = toks.head
  
  if (!is_op(h)) {
    compute(toks.tail, h.toInt::st)
  } else {
    val b = st.head
    val a = st.tail.head
    
    val res = h match {
      case "+" => a + b
      case "-" => a - b
      case "*" => a * b
      case "/" => a / b
    }
    
    compute(toks.tail, res::st.tail.tail)
  }
}





// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15

}



// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.

