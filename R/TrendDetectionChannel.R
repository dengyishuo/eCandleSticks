#' Current Trend Detection using Donchian Channel
#'
#' Uses the Donchian Channel of \code{n} periods to determine whether a price series
#' is in uptrend, sideward trend or downtrend.
#'
#' @param TS xts Time Series containing OHLC prices
#' @param n Number of periods to calculate the high and low band of the Donchian Channel. Default is 20.
#' @param DCSector Sector of Donchian Channel to determine uptrend/downtrend. Default is 1/3.
#'
#' @details
#' This function assumes that a price series is in uptrend when a period's price closes
#' above the upper third of the Donchian Channel of \code{n} periods. If the price closes
#' below the lower third of the channel, a downtrend is detected. If the price closes
#' within the middle third of the channel, a sideward trend is detected.
#'
#' The parameter DCSector can be used to widen/narrow the threshold of up/downtrend detection.
#' For example, a value of 0.25 anticipates the price for an uptrend to be above the highest
#' quarter of the Donchian channel.
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item UpTrend: TRUE if uptrend detected
#' \item NoTrend: TRUE if sideward trend detected
#' \item DownTrend: TRUE if downtrend detected
#' \item Trend: +1 for uptrend, 0 for sideward trend, -1 for downtrend
#' }
#'
#' @author Andreas Voellenklee
#'
#' @seealso
#' \code{\link{TrendDetectionSMA}}
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#'
#' # Create chart of YAHOO
#' chartSeries(YHOO, subset = "last 1 year", TA = NULL)
#'
#' # Visualize the result of trend detection in a indicator box
#' addTA(TrendDetectionChannel(YHOO)[, 4])
#'
#' # Filter YHOO for Hammer Candlestick Patterns that occur in downtrends
#' Hammer <- CSPHammer(YHOO) & TrendDetectionChannel(YHOO)[, "DownTrend"]
#'
#' # How frequent are these hammers?
#' colSums(Hammer, na.rm = TRUE)
#' }
#'
#' @export
#' @importFrom TTR DonchianChannel
#' @importFrom quantmod Cl
TrendDetectionChannel <- function(TS, n = 20, DCSector = 1 / 3) {
  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  Channel <- DonchianChannel2(cbind(Hi(TS), Lo(TS)), n = n)
  UpTrend <- Cl(TS) > Lo(Channel) + (Hi(Channel) - Lo(Channel)) * (1 - DCSector)
  DownTrend <- Cl(TS) < Lo(Channel) + (Hi(Channel) - Lo(Channel)) * DCSector
  NoTrend <- !(UpTrend | DownTrend)
  Trend <- UpTrend + DownTrend * (-1)
  result <- cbind(UpTrend, NoTrend, DownTrend, Trend)
  colnames(result) <- c("UpTrend", "NoTrend", "DownTrend", "Trend")
  return(result)
}
