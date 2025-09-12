#' Lag Open/Close Time Series
#'
#' Create a lagged series from Open and Close price data.
#'
#' @param TS An xts time series containing Open and Close prices
#' @param k Number of periods to lag (default = 1)
#'
#' @details
#' This function shifts the series k-periods down, prepending NAs to the front of the series.
#'
#' @return
#' An xts object containing lagged Open and Close columns. The original column names are
#' appended with \code{'.Lag.k'}, where \code{k} is the number of periods lagged.
#' The returned series maintains the number of observations of the original.
#'
#' @note
#' This function uses the Lag function from the \pkg{stats} package.
#'
#' @seealso \code{\link[stats]{lag}}, \code{\link{LagOHLC}}
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO")
#' LagOC(YHOO) # Lag OC series by one period
#' LagOC(YHOO, k = 1:3) # Lag OC series by one, two and three periods
#' # this will return a 6 columns xts object
#' }
#'
#' @export
LagOC <- function(TS, k = 1) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  result <- cbind(stats::lag(quantmod::Op(TS), k), stats::lag(quantmod::Cl(TS), k))
  colnames(result) <- c(
    paste(colnames(quantmod::Op(TS)), k, sep = ".Lag."),
    paste(colnames(quantmod::Cl(TS)), k, sep = ".Lag.")
  )
  return(result)
}
