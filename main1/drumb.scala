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
def get_prices(portfolio: List[String], years: Range) : List[List[Option[Double]]] = {
    years.map { year =>
        portfolio.map { symbol =>
            get_first_price(symbol, year)
        }
    }.toList
}



// (4) 
def get_delta(price_old: Option[Double], price_new: Option[Double]) : Option[Double] = {
    (price_old, price_new) match {
        case (Some(p_old), Some(p_new)) => Some((p_new - p_old) / p_old)
        case _ => None
    }
}



// (5) 
def get_deltas(data: List[List[Option[Double]]]) :  List[List[Option[Double]]] = {
    val num_years = data.size
    data.zipWithIndex.map { case (prices, i) =>
        prices.map { price_old =>
            if (i + 1 < num_years) {
                get_delta(price_old, data(i + 1)(prices.indexOf(price_old)))
            } else {
                None
            }
        }
    }
}

// (6) 
def yearly_yield(data: List[List[Option[Double]]], balance: Long, index: Int) : Long = {
    if (index >= data.size) {
        balance
    } else {
        val changes = data(index)
        val num_companies = changes.size
        val invested = balance / num_companies
        val profit = changes.filter(_.isDefined).map(_.get).map(x => (x * invested).toLong).sum
        balance + profit
    }
}


// (7) 
def compound_yield(data: List[List[Option[Double]]], balance: Long, index: Int) : Long = {
    if (index >= data.size) {
        balance
    } else {
        val new_balance = yearly_yield(data, balance, index)
        compound_yield(data, new_balance, index + 1)
    }
}

def investment(portfolio: List[String], years: Range, start_balance: Long) : Long =  {
    val data = get_deltas(get_prices(portfolio, years.start to years.end))
    compound_yield(data, start_balance, 0)
}




//Test cases for the two portfolios given above

//println("Real data: " + investment(rstate_portfolio, 1978 to 2019, 100))
//println("Blue data: " + investment(blchip_portfolio, 1978 to 2019, 100))


}




// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
