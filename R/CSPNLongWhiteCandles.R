#' N Consecutive Long White Candles
#'
#' Identifies sequences of N consecutive long white candles in an OHLC price series.
#'
#' @param TS xts Time Series containing OHLC prices
#' @param N Number of consecutive candles to test. Default is 2.
#' @param n Number of preceding candles to calculate median candle length. Default is 20.
#' @param threshold Minimum candle length in relation to the median candle length
#' of \code{n} preceding candles. Default is 1.
#'
#' @details
#' This function identifies when the current candle is the N-th consecutive long white candle.
#' A candle is considered "long" if its length is greater than or equal to the threshold
#' times the median candle length of the preceding n candles.
#'
#' @return
#' A xts object containing the column:
#' \itemize{
#' \item {N}LongWhiteCandles: TRUE if current candle is the \code{N}-th consecutive long white candle
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
#' CSPNLongWhiteCandles(YHOO)
#' }
#'
#' @export
#' @importFrom quantmod is.OHLC
#' @importFrom xts reclass xtsAttributes as.xts
#' @importFrom stats lag
CSPNLongWhiteCandles <- function(TS, N = 2, n = 20, threshold = 1) {
  if (!quantmod::is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  if (N < 1) {
    stop("N has to be a integer >= 1")
  }
  LWC <- CSPLongCandle(TS, n = n, threshold = threshold)[, 1] # LongWhiteCandle
  result <- xts::reclass(xts::as.xts(apply(stats::lag(LWC, k = 0:(N - 1)), 1, all)), TS)
  colnames(result) <- paste(N, "LongWhiteCandles", sep = "")
  xts::xtsAttributes(result) <- list(bars = N)
  return(result)
}
