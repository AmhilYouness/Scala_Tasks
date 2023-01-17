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
def prec(op1: String, op2: String) : Boolean = precs(op1) <= precs(op2)






def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = toks match {
    case x :: xs => {
        if (is_op(x)) {
            st match {
                case s :: ss if s == "(" => {
                    syard(xs, x :: st, out)
                    }
                case s :: ss if s == ")" => {
                    val index = st.indexOf("(")
                    syard(xs, st.drop(index + 1), out:::st.take(index))
                    }
                case s :: ss if (prec(s, x)) =>{
                    syard(toks, ss, out ::: s :: Nil)
                    }
                case s :: ss if (prec(s, x) == false) =>{
                    syard(xs,x::st,out)
                    }
                case _ => {
                    syard(xs, x :: Nil, out)
                    }
            }
        }
        else if(x.forall(_.isDigit)){
            syard(xs,st, out ::: x :: Nil)
            }
        else if(x == "("){
            syard(xs, x :: st, out)
            }
        else if(x == ")"){
            val index = st.indexOf("(")
            syard(xs, st.drop(index + 1), out:::st.take(index))
            }
        else{
            println(x)
            out ::: x :: Nil
            }
    }
    case _ => {
        out ::: st
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
def compute(toks: Toks) : Int = {
  var st = List[Int]()
  for (tok <- toks) {
    if (is_op(tok)) {
      var op2 = st.head
      st = st.tail
      var op1 = st.head
      st = st.tail
      tok match {
        case "+" => st = (op1 + op2) :: st
        case "-" => st = (op1 - op2) :: st
        case "*" => st = (op1 * op2) :: st
        case "/" => st = (op1 / op2) :: st
      }
    } else {
      st = tok.toInt :: st
    }
  }
  st.head
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

