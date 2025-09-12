#' Morning/Evening Star Candlestick Pattern
#'
#' Identifies Morning and Evening Star patterns in an OHLC price series.
#' These are three-candle reversal patterns that signal potential trend changes.
#'
#' @param TS xts Time Series containing OHLC prices
#' @param n Number of preceding candles to calculate median candle length. Default is 20.
#' @param minbodysizeMedian Minimum candle length in relation to the median candle length
#' of the formation's \emph{first} candle. Default is 1.
#' @param maxbodysizeMedian Maximum candle length in relation to the median candle length
#' of the formation's \emph{second} candle. Default is 1.
#'
#' @details
#' Number of candle lines: \bold{3}
#'
#' \strong{Morning Star:}
#' \itemize{
#' \item The market is in a downtrend
#' \item First candle: long black body
#' \item Second candle: small real body of either color with a gap down
#' \item Third candle: white candle that closes above the middle of the first candle's body
#' \item This pattern signals a potential bullish reversal
#' }
#'
#' \strong{Evening Star:}
#' \itemize{
#' \item The market is in an uptrend
#' \item First candle: long white body
#' \item Second candle: small real body of either color with a gap up
#' \item Third candle: black candle that closes below the middle of the first candle's body
#' \item This pattern signals a potential bearish reversal
#' }
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item MorningStar: TRUE if Morning Star pattern detected
#' \item EveningStar: TRUE if Evening Star pattern detected
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
#' The function filters patterns that look like morning/evening stars, without considering
#' the current trend direction. If only patterns in specific trends should be filtered,
#' an external trend detection function must be used. See examples.
#'
#' @seealso
#' \code{\link{CSPLongCandleBody}}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("AAPL", adjust = TRUE)
#' CSPStar(AAPL)
#'
#' # Allow only a small second candle body
#' CSPStar(YHOO, maxbodysizeMedian = 0.5)
#'
#' # Filter for morning stars that occur in downtrends
#' MorningStar <- CSPStar(YHOO)[, "MorningStar"] &
#'   TrendDetectionChannel(lag(YHOO, k = 3))[, "DownTrend"]
#' }
#'
#' @export
#' @importFrom quantmod Op Cl
#' @importFrom xts reclass xtsAttributes
CSPStar <- function(TS, n = 20, minbodysizeMedian = 1, maxbodysizeMedian = 1) {
  if (!(has.Op(TS) && has.Cl(TS))) {
    stop("Price series must contain Open and Close.")
  }

  LAG2TS <- LagOC(TS, k = 2)
  LAG1TS <- LagOC(TS, k = 1)
  LCB2 <- CSPLongCandleBody(LAG2TS, n = n, threshold = minbodysizeMedian) # first candle body is longer than average
  GAP1 <- CSPGap(LAG1TS, ignoreShadows = TRUE) # gap between the first and second candle
  SCB1 <- CSPShortCandleBody(LAG1TS, n = n, threshold = maxbodysizeMedian) # second candle body is shorter than average

  MorningStar <- xts::reclass(
    LCB2[, 2] & # 1st candle: long black candle body
      GAP1[, 2] & # gap down from 1st to 2nd candle
      (SCB1[, 1] | SCB1[, 2]) & # 2nd candle: short black or white candle body
      quantmod::Cl(TS) > quantmod::Op(TS) & # 3rd candle is white
      quantmod::Cl(TS) > (quantmod::Op(LAG2TS) + quantmod::Cl(LAG2TS)) / 2, # 3rd candle closes above middle of 1st candle body
    TS
  )

  EveningStar <- xts::reclass(
    LCB2[, 1] & # 1st candle: long white candle body
      GAP1[, 1] & # gap up from 1st to 2nd candle
      (SCB1[, 1] | SCB1[, 2]) & # 2nd candle: short black or white candle body
      quantmod::Cl(TS) < quantmod::Op(TS) & # third candle is black
      quantmod::Cl(TS) < (quantmod::Op(LAG2TS) + quantmod::Cl(LAG2TS)) / 2, # 3rd candle closes below middle of 1st candle body
    TS
  )

  result <- cbind(MorningStar, EveningStar)
  colnames(result) <- c("MorningStar", "EveningStar")
  xts::xtsAttributes(result) <- list(bars = 3)
  return(result)
}
