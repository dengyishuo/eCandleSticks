#' N Consecutive Long Black Candle Bodies
#'
#' Identifies sequences of N consecutive long black candle bodies in an OHLC price series.
#'
#' @param TS xts Time Series containing Open and Close prices
#' @param N Number of consecutive candles to test. Default is 2.
#' @param n Number of preceding candles to calculate median candle body length. Default is 20.
#' @param threshold Minimum candle body length in relation to the median candle body length
#' of \code{n} preceding candles. Default is 1.
#'
#' @details
#' This function identifies when the current candle is the N-th consecutive long black candle body.
#' A candle body is considered "long" if its length is greater than or equal to the threshold
#' times the median candle body length of the preceding n candles.
#'
#' @return
#' A xts object containing the column:
#' \itemize{
#' \item {N}LongBlackCandleBodies: TRUE if current candle is the \code{N}-th consecutive long black candle body
#' }
#'
#' @author Andreas Voellenklee
#'
#' @seealso
#' \code{\link{CSPLongCandleBody}}
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' CSPNLongBlackCandleBodies(YHOO, N = 4, n = 50, threshold = 1.2)
#' }
#'
#' @export
#' @importFrom xts reclass xtsAttributes as.xts
#' @importFrom stats lag
CSPNLongBlackCandleBodies <- function(TS, N = 2, n = 20, threshold = 1) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  if (N < 1) {
    stop("N has to be a integer >= 1")
  }
  LBCB <- CSPLongCandleBody(TS, n = n, threshold = threshold)[, 2] # LongBlackCandleBody
  result <- xts::reclass(xts::as.xts(apply(stats::lag(LBCB, k = 0:(N - 1)), 1, all)), TS)
  colnames(result) <- paste(N, "LongBlackCandleBodies", sep = "")
  xts::xtsAttributes(result) <- list(bars = N)
  return(result)
}
