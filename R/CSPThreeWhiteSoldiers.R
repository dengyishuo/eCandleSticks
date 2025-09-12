#' Three White Soldiers Candlestick Pattern
#'
#' Identifies Three White Soldiers patterns in an Open/Close price series.
#' This is a three-candle bullish reversal pattern that occurs during a downtrend.
#'
#' @param TS xts Time Series containing Open and Close prices
#' @param strict If FALSE, use less strict conditions to detect pattern. Default is TRUE.
#' @param n Number of preceding candles to calculate median candle length. Default is 20.
#' @param minbodysizeMedian Minimum candle length in relation to the median candle length
#' of \code{n} preceding candles. Default is 1.
#'
#' @details
#' Number of candle lines: \bold{3}
#'
#' The Three White Soldiers pattern is characterized by three long white candlesticks
#' stepping upward like a staircase.
#'
#' In strict mode (default):
#' \itemize{
#' \item The opening of each day is lower than the previous close but higher than the previous open
#' \item Each candle must have a long body relative to the median of previous candles
#' \item The pattern appears during a downtrend
#' }
#'
#' When strict = FALSE, candle 2 and/or 3 of the formation may open higher than the
#' previous day's close, thus forming a gap.
#'
#' @return
#' A xts object containing the column:
#' \itemize{
#' \item ThreeWhiteSoldiers: TRUE if Three White Soldiers pattern detected
#' }
#'
#' @references
#' The following sites were used to code/document this indicator:
#' \itemize{
#' \item \url{http://www.candlesticker.com/Bullish.asp}
#' \item \url{http://www.candlesticker.com/Bearish.asp}
#' }
#'
#' @note
#' The function filters patterns that look like three white soldiers, without considering
#' the current trend direction. If only patterns in downtrends should be filtered,
#' an external trend detection function must be used. See examples.
#'
#' @seealso
#' \code{\link{CSPThreeBlackCrows}}
#' \code{\link{CSPNLongWhiteCandles}}
#' \code{\link{CSPNLongWhiteCandleBodies}}
#' \code{\link{CSPThreeLineStrike}}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' CSPThreeWhiteSoldiers(YHOO)
#' CSPThreeWhiteSoldiers(YHOO, strict = FALSE)
#'
#' # Filter for three white soldiers that occur in downtrends
#' ThreeWhiteSoldiers <- CSPThreeWhiteSoldiers(YHOO) &
#'   TrendDetectionChannel(lag(YHOO, k = 3))[, "DownTrend"]
#'
#' # How often does that occur?
#' colSums(ThreeWhiteSoldiers, na.rm = TRUE)
#' }
#'
#' @export
#' @importFrom quantmod Op Cl is.OHLC
#' @importFrom xts reclass xtsAttributes
#' @importFrom stats lag
CSPThreeWhiteSoldiers <- function(TS, strict = TRUE, n = 20, minbodysizeMedian = 1) {
  if (!quantmod::is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }

  THREELWCB <- CSPNLongWhiteCandleBodies(TS, N = 3, n = n, threshold = minbodysizeMedian)

  lagged_oc <- do.call(merge, lapply(0:2, function(k) {
    merge(stats::lag(quantmod::Op(TS), -k), stats::lag(quantmod::Cl(TS), -k))
  }))

  colnames(lagged_oc) <- c("Op.L0", "Cl.L0", "Op.L1", "Cl.L1", "Op.L2", "Cl.L2")

  result <- xts::reclass(
    THREELWCB[, 1] &
      lagged_oc$Op.L0 > lagged_oc$Op.L1 &
      lagged_oc$Op.L1 > lagged_oc$Op.L2 &
      lagged_oc$Cl.L0 > lagged_oc$Cl.L1 &
      lagged_oc$Cl.L1 > lagged_oc$Cl.L2,
    TS
  )

  if (strict) {
    result <- result &
      lagged_oc$Op.L0 <= lagged_oc$Cl.L1 &
      lagged_oc$Op.L1 <= lagged_oc$Cl.L2
  }

  colnames(result) <- "ThreeWhiteSoldiers"
  xts::xtsAttributes(result) <- list(bars = 3)
  return(result)
}
