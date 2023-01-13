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




def pool(secret: String, word: String): List[Char] = {
    val minLength = math.min(secret.length, word.length)
    (0 until minLength).filter(i => secret(i) != word(i)).map(i => secret(i)).toList
}





def aux(secret: List[Char], word: List[Char], pool: List[Char]) : List[Tip] = (secret, word) match {
    case (Nil, Nil) => Nil
    case (Nil, _) => word.map(_ => Absent)
    case (_, Nil) => secret.map(_ => Absent)
    case (s :: secretTail, w :: wordTail) =>
        if (s == w) {
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



// score("chess", "caves") // => List(Correct, Absent, Absent, Present, Correct)
// score("doses", "slide") // => List(Present, Absent, Absent, Present, Present)
// score("chess", "swiss") // => List(Absent, Absent, Absent, Correct, Correct)
// score("chess", "eexss") // => List(Present, Absent, Absent, Correct, Correct)

// (4)
def eval(t: Tip) : Int ={
    t match {
        case Absent => 0
        case Present => 1
        case Correct => 10
    }
}

def iscore(secret: String, word: String) : Int = {
    score(secret,word).map(eval).sum
}
//iscore("chess", "caves") // => 21
//iscore("chess", "swiss") // => 20

// (5)
def lowest(secrets: List[String], word: String, current: Int, acc: List[String]) : List[String] =  {
    if (secrets.isEmpty) {
        acc
    } else {
        val secret = secrets.head
        val score = iscore(secret, word)
        if (score < current) {
            lowest(secrets.tail, word, score, List(secret))
        } else if (score == current) {
            lowest(secrets.tail, word, score, secret :: acc)
        } else {
            lowest(secrets.tail, word, current, acc)
        }
    }
}

def evil(secrets: List[String], word: String) = {
    lowest(secrets, word, Int.MaxValue, List())
}


//evil(secrets, "stent").length
//evil(secrets, "hexes").length
//evil(secrets, "horse").length
//evil(secrets, "hoise").length
//evil(secrets, "house").length

// (6)
def frequencies(secrets: List[String]) : Map[Char, Double] = {
    val allLetters = secrets.mkString.toLowerCase.toList
    val numLetters = allLetters.length
    val letterCount = allLetters.groupBy(identity).mapValues(_.size.toDouble)
    letterCount.mapValues(count =>1 -  count / numLetters).toMap
}

// (7)
def rank(frqs: Map[Char, Double], s: String) = {
    s.toLowerCase.map(frqs.getOrElse(_, 0.0)).sum
}

def ranked_evil(secrets: List[String], word: String) =  {
    val frqs = frequencies(secrets)
    evil(secrets,word).sortBy(s => rank(frqs, s)).reverse.take(1)
}


}






// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
