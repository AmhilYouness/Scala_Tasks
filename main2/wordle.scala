// Main Part 3 about Evil Wordle
//===============================


object M2 { 

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================


//(1)
def get_wordle_list(url: String) : List[String] =  {
    val source = Try(Source.fromURL(url))
    source.toOption match {
        case Some(s) => s.getLines().toList
        case None => List.empty
    }
}

// val secrets = get_wordle_list("https://nms.kcl.ac.uk/christian.urban/wordle.txt")
// secrets.length // => 12972
// secrets.filter(_.length != 5) // => Nil

//(2)
def removeN[A](xs: List[A], elem: A, n: Int) : List[A] = {
    var i = n
    xs.foldLeft(List[A]())((acc, x) => {
        if (x == elem && i > 0) {
            i -= 1
            acc
        } else {
            acc :+ x
        }
    })
}


// removeN(List(1,2,3,2,1), 3, 1)  // => List(1, 2, 2, 1)
// removeN(List(1,2,3,2,1), 2, 1)  // => List(1, 3, 2, 1)
// removeN(List(1,2,3,2,1), 1, 1)  // => List(2, 3, 2, 1)
// removeN(List(1,2,3,2,1), 0, 2)  // => List(1, 2, 3, 2, 1)

// (3)
abstract class Tip
case object Absent extends Tip
case object Present extends Tip
case object Correct extends Tip

def pool(secret: String, word: String) : List[Char] = {
    secret.filter(c => !word.contains(c)).toList
}

def aux(secret: List[Char], word: List[Char], pool: List[Char]) : List[Tip] = (secret, word) match {
    case (Nil, Nil) => Nil
    case (Nil, _) => word.map(_ => Absent)
    case (_, Nil) => secret.map(_ => Absent)
    case (s :: secretTail, w :: wordTail) =>
        if (s == w && pool.contains(w)) {
            Present :: aux(secretTail, wordTail, pool.filterNot(_ == w))
        } else if (s == w) {
            Correct :: aux(secretTail, wordTail, pool.filterNot(_ == w))
        } else if (pool.contains(w)) {
            Present :: aux(secretTail, wordTail, pool.filterNot(_ == w))
        } else {
            Absent :: aux(secretTail, wordTail, pool)
        }
}

def score(secret: String, word: String) : List[Tip] = {
    aux(secret.toList, word.toList, pool(secret, word))
}
:


// score("chess", "caves") // => List(Correct, Absent, Absent, Present, Correct)
// score("doses", "slide") // => List(Present, Absent, Absent, Present, Present)
// score("chess", "swiss") // => List(Absent, Absent, Absent, Correct, Correct)
// score("chess", "eexss") // => List(Present, Absent, Absent, Correct, Correct)

// (4)
def eval(t: Tip) : Int = ???

def iscore(secret: String, word: String) : Int = ???

//iscore("chess", "caves") // => 21
//iscore("chess", "swiss") // => 20

// (5)
def lowest(secrets: List[String], word: String, current: Int, acc: List[String]) : List[String] = ???

def evil(secrets: List[String], word: String) = ???


//evil(secrets, "stent").length
//evil(secrets, "hexes").length
//evil(secrets, "horse").length
//evil(secrets, "hoise").length
//evil(secrets, "house").length

// (6)
def frequencies(secrets: List[String]) : Map[Char, Double] = ???

// (7)
def rank(frqs: Map[Char, Double], s: String) = ???

def ranked_evil(secrets: List[String], word: String) = ???


}






// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.