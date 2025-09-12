#' Piercing Candlestick Pattern
#'
#' Identifies Piercing Patterns in an Open/Close price series.
#' This is a two-candle bullish reversal pattern that occurs during a downtrend.
#'
#' @param TS xts Time Series containing Open and Close Prices
#' @param n Number of preceding candles to calculate median candle body length. Default is 20.
#' @param minbodysizeMedian Minimum body length relative to the median of the past \code{n} body sizes. Default is 1.
#'
#' @details
#' Number of candle lines: \bold{2}
#'
#' A Piercing Pattern consists of:
#' \enumerate{
#' \item A long black candlestick (first candle)
#' \item A gap lower opening on the next day
#' \item A white candlestick that closes more than halfway into the prior black candlestick's real body
#' \item The close of the second candle is still below the open of the first candle
#' }
#'
#' This pattern is considered a bullish reversal signal when it appears during a downtrend.
#'
#' @return
#' A xts object containing the column:
#' \itemize{
#' \item PiercingPattern: TRUE if Piercing Pattern detected
#' }
#'
#' @references
#' The following sites were used to code/document this indicator:
#' \itemize{
#' \item \url{http://www.candlesticker.com/Bullish.asp}
#' \item \url{http://www.onlinetradingconcepts.com/TechnicalAnalysis/Candlesticks/PiercingPattern.html}
#' }
#'
#' @note
#' The function filters patterns that look like piercing patterns, without considering
#' the current trend direction. If only patterns in downtrends should be filtered,
#' an external trend detection function must be used. See examples.
#'
#' @seealso
#' The counterpart of this pattern is \code{\link{CSPDarkCloudCover}}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' CSPPiercingPattern(YHOO)
#'
#' # Filter piercing patterns that occur in downtrends.
#' # The lag of 2 periods of the time series for trend detection
#' # ensures that the downtrend is active before the
#' # piercing pattern occurs.
#' CSPPiercingPattern(YHOO) &
#'   TrendDetectionChannel(lag(YHOO, k = 2))[, "DownTrend"]
#' }
#'
#' @export
#' @importFrom quantmod Op Cl
#' @importFrom xts reclass xtsAttributes
CSPPiercingPattern <- function(TS, n = 20, minbodysizeMedian = 1) {
  if (!(has.Op(TS) && has.Cl(TS))) {
    stop("Price series must contain Open and Close.")
  }

  LAGTS <- LagOC(TS, k = 1)
  LongCandleBody <- CSPLongCandleBody(LAGTS, n = n, threshold = minbodysizeMedian)

  PiercingPattern <- xts::reclass(
    LongCandleBody[, "LongBlackCandleBody"] & # first candle is black and longer than median of past n candles
      quantmod::Op(TS) < quantmod::Cl(LAGTS) & # second candle opens lower than close of 1st candle
      quantmod::Cl(TS) >= (quantmod::Op(LAGTS) + quantmod::Cl(LAGTS)) / 2 & # second candle closes at or higher than half of 1st candles' body
      quantmod::Cl(TS) < quantmod::Op(LAGTS), # close of second candle is lower than open of 1st candle
    TS
  )

  colnames(PiercingPattern) <- c("PiercingPattern")
  xts::xtsAttributes(PiercingPattern) <- list(bars = 2)
  return(PiercingPattern)
}
