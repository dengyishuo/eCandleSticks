#' Three Outside Up/Down Candlestick Pattern
#'
#' Identifies Three Outside Up and Three Outside Down patterns in an Open/Close price series.
#' These are three-candle reversal patterns that signal potential trend changes.
#'
#' @param TS xts Time Series containing Open and Close Prices
#'
#' @details
#' Number of candle lines: \bold{3}
#'
#' \strong{Three Outside Up:}
#' \itemize{
#' \item The market is in a downtrend
#' \item The first two days form a Bullish Engulfing pattern
#' \item The third day confirms the reversal with a white candlestick closing with a new high for the last three days
#' \item This pattern signals a potential bullish reversal in a downtrend
#' }
#'
#' \strong{Three Outside Down:}
#' \itemize{
#' \item The market is in an uptrend
#' \item The first two days form a Bearish Engulfing pattern
#' \item The third day confirms the reversal with a black candlestick closing with a new low for the last three days
#' \item This pattern signals a potential bearish reversal in an uptrend
#' }
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item ThreeOutsideUp: TRUE if Three Outside Up pattern detected
#' \item ThreeOutsideDown: TRUE if Three Outside Down pattern detected
#' }
#'
#' @references
#' The following sites were used to code/document this candlestick pattern:
#' \itemize{
#' \item \url{http://www.candlesticker.com/Bullish.asp}
#' \item \url{http://www.candlesticker.com/Bearish.asp}
#' }
#'
#' @note
#' The function filters patterns that look like three outside up/down, without considering
#' the current trend direction. If only patterns in specific trends should be filtered,
#' an external trend detection function must be used. See examples.
#'
#' @seealso
#' \code{\link{CSPEngulfing}}
#' \code{\link{CSPThreeInside}}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("AAPL", adjust = TRUE)
#' CSPThreeOutside(AAPL)
#'
#' # Filter three outside up in downtrends
#' CSPThreeOutside(AAPL)[, "ThreeOutsideUp"] &
#'   TrendDetectionChannel(lag(AAPL, k = 3))[, "DownTrend"]
#' }
#'
#' @export
#' @importFrom quantmod Op Cl
#' @importFrom xts reclass xtsAttributes
#' @importFrom stats lag
CSPThreeOutside <- function(TS) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  LAGTS1 <- LagOHLC(TS, k = 1) # second candle
  LAGENGULFING <- CSPEngulfing(LAGTS1)
  TOUP <- reclass(LAGENGULFING[, 1] & # bullish engulfing
    Cl(TS) > Op(TS) & # 3rd candle is white
    Cl(TS) > Cl(LAGTS1), TS) # 3rd candle closes above 2nd candle
  TODOWN <- reclass(LAGENGULFING[, 2] & # bearish engulfing
    Cl(TS) < Op(TS) & # 3rd candle is black
    Cl(TS) < Cl(LAGTS1), TS) # 3rd candle closes below 2nd candle
  result <- cbind(TOUP, TODOWN)
  colnames(result) <- c("ThreeOutsideUp", "ThreeOutsideDown")

  xts::xtsAttributes(result) <- list(bars = 3)

  return(result)
}
