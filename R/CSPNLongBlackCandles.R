#' N Consecutive Long Black Candles
#'
#' Identifies sequences of N consecutive long black candles in an OHLC price series.
#'
#' @param TS xts Time Series containing OHLC prices
#' @param N Number of consecutive candles to test. Default is 2.
#' @param n Number of preceding candles to calculate median candle length. Default is 20.
#' @param threshold Minimum candle length in relation to the median candle length
#' of \code{n} preceding candles. Default is 1.
#'
#' @details
#' This function identifies when the current candle is the N-th consecutive long black candle.
#' A candle is considered "long" if its length is greater than or equal to the threshold
#' times the median candle length of the preceding n candles.
#'
#' @return
#' A xts object containing the column:
#' \itemize{
#' \item {N}LongBlackCandles: TRUE if current candle is the \code{N}-th consecutive long black candle
#' }
#'
#' @author Andreas Voellenklee
#'
#' @seealso
#' \code{\link{CSPLongCandle}}
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' CSPNLongBlackCandles(YHOO, N = 3)
#' }
#'
#' @export
#' @importFrom quantmod is.OHLC
#' @importFrom xts reclass xtsAttributes as.xts
#' @importFrom stats lag
CSPNLongBlackCandles <- function(TS, N = 2, n = 20, threshold = 1) {
  if (!quantmod::is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  if (N < 1) {
    stop("N has to be a integer >= 1")
  }
  LBC <- CSPLongCandle(TS, n = n, threshold = threshold)[, 2] # LongBlackCandle
  result <- xts::reclass(xts::as.xts(apply(stats::lag(LBC, k = 0:(N - 1)), 1, all)), TS)
  colnames(result) <- paste(N, "LongBlackCandles", sep = "")
  xts::xtsAttributes(result) <- list(bars = N)
  return(result)
}
