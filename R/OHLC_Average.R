#' Calculate OHLC Average
#'
#' Calculates the average value of Open, High, Low, and Close prices.
#'
#' @param TS An xts time series containing OHLC prices
#'
#' @details
#' The OHLC Average is calculated as: (Open + High + Low + Close) / 4
#'
#' @return
#' An xts object containing a single column:
#' \itemize{
#' \item \code{OHLC_Average}: The average of Open, High, Low, and Close prices
#' }
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' head(OHLC_Average(YHOO))
#' }
#'
#' @export
OHLC_Average <- function(TS) {
  if (!quantmod::is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  Av <- xts::as.xts(apply(cbind(quantmod::Op(TS), quantmod::Hi(TS), quantmod::Lo(TS), quantmod::Cl(TS)), 1, mean))
  colnames(Av) <- c("OHLC_Average")
  return(xts::reclass(Av, TS))
}
