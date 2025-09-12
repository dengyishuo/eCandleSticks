#' Current Trend Detection using Exponential Moving Average
#'
#' Uses the EMA of \code{n} periods to determine whether a price series
#' is in uptrend or downtrend.
#'
#' @param TS xts Time Series containing OHLC prices
#' @param n Number of periods for the EMA to average over. Default is 20.
#'
#' @details
#' This function assumes that a price series is in uptrend (downtrend) when a period's
#' price closes above (below) the exponential moving average of \code{n} periods.
#' EMA gives more weight to recent prices, making it more responsive to new information
#' compared to SMA.
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item UpTrend: TRUE if uptrend detected (Close > EMA(n))
#' \item NoTrend: TRUE if sideward trend detected (Close == EMA(n))
#' \item DownTrend: TRUE if downtrend detected (Close < EMA(n))
#' \item Trend: +1 for uptrend, 0 for sideward trend, -1 for downtrend
#' }
#'
#' @author Your Name
#'
#' @seealso
#' \code{\link{TrendDetectionSMA}}, \code{\link{TrendDetectionChannel}}
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#'
#' # Create chart of YAHOO
#' chartSeries(YHOO, subset = "last 1 year", TA = NULL)
#'
#' # Visualize the result of trend detection in a indicator box
#' addTA(TrendDetectionEMA(YHOO)[, 4])
#'
#' # Filter YHOO for Hammer Candlestick Patterns that occur in downtrends
#' Hammer <- CSPHammer(YHOO) & TrendDetectionEMA(YHOO)[, "DownTrend"]
#'
#' # How frequent are these hammers?
#' colSums(Hammer, na.rm = TRUE)
#' }
#'
#' @export
#' @importFrom TTR EMA
#' @importFrom quantmod Cl
TrendDetectionEMA <- function(TS, n = 20) {
  if (!has.Cl(TS)) {
    stop("Price series must contain Close prices")
  }

  Close <- quantmod::Cl(TS)
  CloseToEMA <- Close / TTR::EMA(Close, n = n)

  UpTrend <- CloseToEMA[, 1] > 1
  NoTrend <- CloseToEMA[, 1] == 1
  DownTrend <- CloseToEMA[, 1] < 1
  Trend <- UpTrend + DownTrend * (-1)

  result <- cbind(UpTrend, NoTrend, DownTrend, Trend)
  colnames(result) <- c("UpTrend", "NoTrend", "DownTrend", "Trend")

  return(result)
}
