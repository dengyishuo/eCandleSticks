#' N Consecutive Long White Candle Bodies
#'
#' Identifies sequences of N consecutive long white candle bodies in an OHLC price series.
#'
#' @param TS xts Time Series containing Open and Close prices
#' @param N Number of consecutive candles to test. Default is 2.
#' @param n Number of preceding candles to calculate median candle body length. Default is 20.
#' @param threshold Minimum candle body length in relation to the median candle body length
#' of \code{n} preceding candles. Default is 1.
#'
#' @details
#' This function identifies when the current candle is the N-th consecutive long white candle body.
#' A candle body is considered "long" if its length is greater than or equal to the threshold
#' times the median candle body length of the preceding n candles.
#'
#' @return
#' A xts object containing the column:
#' \itemize{
#' \item {N}LongWhiteCandleBodies: TRUE if current candle is the \code{N}-th consecutive long white candle body
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
#' CSPNLongWhiteCandleBodies(YHOO)
#' }
#'
#' @export
#' @importFrom xts reclass xtsAttributes as.xts
#' @importFrom stats lag
CSPNLongWhiteCandleBodies <- function(TS, N = 2, n = 20, threshold = 1) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  if (N < 1) {
    stop("N has to be a integer >= 1")
  }
  LWCB <- CSPLongCandleBody(TS, n = n, threshold = threshold)[, 1] # LongWhiteCandleBody
  result <- xts::reclass(xts::as.xts(apply(stats::lag(LWCB, k = 0:(N - 1)), 1, all)), TS)
  colnames(result) <- paste(N, "LongWhiteCandleBodies", sep = "")
  xts::xtsAttributes(result) <- list(bars = N)
  return(result)
}
