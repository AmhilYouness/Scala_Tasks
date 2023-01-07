// Core Part 1 about the 3n+1 conjecture
//============================================

object C1 {

// ADD YOUR CODE BELOW
//======================


//(1) 
def collatz(n: Long) : Long = {
  if (n == 1) 0 
  else if (n % 2 == 0) 1 + collatz(n / 2) 
  else 1 + collatz(3 * n + 1) 
}



//(2) 
def collatz_max(bnd: Long): (Long, Long) = {

  var max_steps = 0L
  var number = 0L
  for (i <- 1L to bnd) {
    val steps = collatz(i)
    if (steps > max_steps) {
      max_steps = steps
      number = i
    }
  }

  (max_steps, number)
}










//(3)
def is_pow_of_two(n: Long) : Boolean = {
  if (n == 0) false
  else (n & (n - 1)) == 0
}

def is_hard(n: Long) : Boolean = {
  is_pow_of_two(3 * n + 1)
}



def last_odd(n: Long): Long = {
  if(n == 0) 0
  else {
    if (n % 2 == 0) last_odd(n/2)
    else{
      if (is_hard(n)) n
      else last_odd(n*3 + 1)
    }

  }     
}


}



// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
