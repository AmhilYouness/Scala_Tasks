// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

object C3b {


// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// left- and right-associativity
abstract class Assoc
case object LA extends Assoc
case object RA extends Assoc


// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

// ADD YOUR CODE BELOW
//======================

// (1) 
def is_op(op: String) : Boolean = ops.contains(op)
def prec(op1: String, op2: String) : Boolean = precs(op1) <= precs(op2)



// (3) 
def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks =  {
    var outval:Toks = out
    var stval: Toks = st
    toks match {
    case Nil => stval.reverse ::: outval
    case h :: t if !ops.contains(h) => syard(t, stval, outval ::: List(h))
    case h :: t if h == "(" => syard(t, h :: stval, outval)
    case h :: t if h == ")" => 
      var op = stval.head
      var new_stval = stval.tail
      while (op != "(") {
        outval = outval ::: List(op)
        op = new_stval.head
        new_stval = new_stval.tail
      }
      syard(t, new_stval, outval)
    case h :: t if ops.contains(h) => 
      while (stval.nonEmpty && ops.contains(stval.head) && (prec(stval.head, h) || assoc(h) == RA && prec(stval.head, h) == true )) {
        outval = outval ::: List(stval.head)
        stval = stval.tail
      }
      syard(t, h :: stval, outval)
    }
}



// test cases
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


// (4)
def compute(toks: Toks, st: List[Int] = Nil) : Int = {
    var stval : List[Int] = st
    for (tok <- toks) {
        if (ops.contains(tok)) {
        var op2 = stval.head
        stval = stval.tail
        var op1 = stval.head
        stval = stval.tail
        tok match {
            case "+" => stval = (op1 + op2) :: stval
            case "-" => stval = (op1 - op2) :: stval
            case "*" => stval = (op1 * op2) :: stval
            case "/" => stval = (op1 / op2) :: stval
            case "^" => stval = math.pow(op1, op2).toInt :: stval
        }
        } else {
        stval = tok.toInt :: stval
        }
    }
    stval.head
}



// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

}





// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
