#' Lag OHLC Time Series
#'
#' Create a lagged series from OHLC price data.
#'
#' @param TS An xts time series containing Open, High, Low and Close prices
#' @param k Number of periods to lag (default = 1)
#'
#' @details
#' This function shifts the series k-periods down, prepending NAs to the front of the series.
#'
#' @return
#' An xts object containing lagged OHLC columns. The original column names are
#' appended with \code{'.Lag.k'}, where \code{k} is the number of periods lagged.
#' The returned series maintains the number of observations of the original.
#'
#' @note
#' This function uses the Lag function from the \pkg{stats} package.
#'
#' @seealso \code{\link[stats]{lag}}, \code{\link{LagOC}}
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO")
#' LagOHLC(YHOO) # Lag OHLC series by one period
#' LagOHLC(YHOO, k = 1:3) # Lag OHLC series by one, two and three periods
#' # this will return a 12 columns xts object
#' }
#'
#' @export
LagOHLC <- function(TS, k = 1) {
  if (!quantmod::is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  result <- cbind(
    stats::lag(quantmod::Op(TS), k),
    stats::lag(quantmod::Hi(TS), k),
    stats::lag(quantmod::Lo(TS), k),
    stats::lag(quantmod::Cl(TS), k)
  )
  colnames(result) <- c(
    paste(colnames(quantmod::Op(TS)), k, sep = ".Lag."),
    paste(colnames(quantmod::Hi(TS)), k, sep = ".Lag."),
    paste(colnames(quantmod::Lo(TS)), k, sep = ".Lag."),
    paste(colnames(quantmod::Cl(TS)), k, sep = ".Lag.")
  )
  return(result)
}
