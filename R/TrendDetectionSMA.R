#' Current Trend Detection using Simple Moving Average
#'
#' Uses the SMA of \code{n} periods to determine whether a price series
#' is in uptrend or downtrend.
#'
#' @param TS xts Time Series containing OHLC prices
#' @param n Number of periods for the SMA to average over. Default is 20.
#'
#' @details
#' This function assumes that a price series is in uptrend (downtrend) when a period's
#' price closes above (below) the simple moving average of \code{n} periods.
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item UpTrend: TRUE if uptrend detected (Close > SMA(n))
#' \item NoTrend: TRUE if sideward trend detected (Close == SMA(n))
#' \item DownTrend: TRUE if downtrend detected (Close < SMA(n))
#' \item Trend: +1 for uptrend, 0 for sideward trend, -1 for downtrend
#' }
#'
#' @author Andreas Voellenklee
#'
#' @seealso
#' \code{\link{TrendDetectionChannel}}
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#'
#' # Create chart of YAHOO
#' chartSeries(YHOO, subset = "last 1 year", TA = NULL)
#'
#' # Visualize the result of trend detection in a indicator box
#' addTA(TrendDetectionSMA(YHOO)[, 4])
#'
#' # Filter YHOO for Hammer Candlestick Patterns that occur in downtrends
#' Hammer <- CSPHammer(YHOO) & TrendDetectionSMA(YHOO)[, "DownTrend"]
#'
#' # How frequent are these hammers?
#' colSums(Hammer, na.rm = TRUE)
#' }
#'
#' @export
#' @importFrom TTR SMA
#' @importFrom quantmod Cl
TrendDetectionSMA <- function(TS, n = 20) {
  if (!has.Cl(TS)) {
    stop("Price series must contain Close prices")
  }
  Close <- Cl(TS)
  CloseToSMA <- Close / SMA(Close, n = n)
  UpTrend <- CloseToSMA[, 1] > 1
  NoTrend <- CloseToSMA[, 1] == 1
  DownTrend <- CloseToSMA[, 1] < 1
  Trend <- UpTrend + DownTrend * (-1)
  result <- cbind(UpTrend, NoTrend, DownTrend, Trend)
  colnames(result) <- c("UpTrend", "NoTrend", "DownTrend", "Trend")
  return(result)
}
