// Main Part 1 about a really dumb investment strategy
//===================================================

object M1 {

//two test portfolios

val blchip_portfolio = List("GOOG", "AAPL", "MSFT", "IBM", "FB", "AMZN", "BIDU")
val rstate_portfolio = List("PLD", "PSA", "AMT", "AIV", "AVB", "BXP", "CCI", 
                            "DLR", "EQIX", "EQR", "ESS", "EXR", "FRT", "HCP") 

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================


// (1) 
def get_january_data(symbol: String, year: Int) : List[String] = {
    val file = Source.fromFile(symbol + ".csv")
    val rows = file.getLines().toList
    val filteredRows = rows.filter {
        case row =>
            Try {
                val date = row.split(",")(0)
                date.startsWith(year.toString)
            }.getOrElse(false)
    }
    file.close()
    filteredRows.map {
        case row => 
            val cells = row.split(",")
            s"${cells(0)},${cells(1)}"
    }
}


// (2) 
def get_first_price(symbol: String, year: Int) : Option[Double] = {
    val januaryData = get_january_data(symbol, year)
    if (januaryData.isEmpty) {
        None
    } else {
        val firstPrice = januaryData.head.split(",")(1)
        Try(firstPrice.toDouble).toOption
    }
}


// (3) 
def get_prices(portfolio: List[String], years: Range) : List[List[Option[Double]]] = ???



// (4) 
def get_delta(price_old: Option[Double], price_new: Option[Double]) : Option[Double] = ???



// (5) 
def get_deltas(data: List[List[Option[Double]]]) :  List[List[Option[Double]]] = ???

// (6) 
def yearly_yield(data: List[List[Option[Double]]], balance: Long, index: Int) : Long = ???


// (7) 
def compound_yield(data: List[List[Option[Double]]], balance: Long, index: Int) : Long = ???

def investment(portfolio: List[String], years: Range, start_balance: Long) : Long = ???




//Test cases for the two portfolios given above

//println("Real data: " + investment(rstate_portfolio, 1978 to 2019, 100))
//println("Blue data: " + investment(blchip_portfolio, 1978 to 2019, 100))


}




// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
