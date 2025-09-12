#' Dark Cloud Cover Candlestick Pattern
#'
#' Identifies Dark Cloud Cover patterns in an Open/Close price series.
#' This is a two-candle bearish reversal pattern that occurs during an uptrend.
#'
#' @param TS xts Time Series containing Open and Close prices
#' @param n Number of preceding candles to calculate median candle body length. Default is 20.
#' @param minbodysizeMedian Minimum body length relative to the median of the past \code{n} body sizes. Default is 1.
#'
#' @details
#' Number of candle lines: \bold{2}
#'
#' A Dark Cloud Cover pattern consists of:
#' \enumerate{
#' \item A long white candlestick (body length > median of past n candles)
#' \item A gap higher opening on the next day
#' \item A black candlestick that closes more than halfway into the prior white candlestick's body
#' \item The close of the second candle is still above the open of the first candle
#' }
#'
#' This pattern is considered a bearish reversal signal when it appears during an uptrend.
#'
#' @return
#' A xts object containing the column:
#' \itemize{
#' \item DarkCloudCover: TRUE if Dark Cloud Cover pattern detected
#' }
#'
#' @references
#' The following sites were used to code/document this indicator:
#' \itemize{
#' \item \url{http://www.candlesticker.com/Bearish.asp}
#' \item \url{http://www.onlinetradingconcepts.com/TechnicalAnalysis/Candlesticks/DarkCloudCover.html}
#' }
#'
#' @note
#' The function filters patterns that look like dark cloud covers, without considering
#' the current trend direction. If only patterns in uptrends should be filtered,
#' an external trend detection function must be used. See examples.
#'
#' @seealso
#' The counterpart of this pattern is \code{\link{CSPPiercingPattern}}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' CSPDarkCloudCover(YHOO)
#'
#' # Filter dark cloud covers that occur in uptrends.
#' # The lag of 2 periods of the time series for trend detection
#' # ensures that the uptrend is active before the
#' # dark cloud cover occurs.
#' CSPDarkCloudCover(YHOO) &
#'   TrendDetectionChannel(lag(YHOO, k = 2))[, "UpTrend"]
#' }
#'
#' @export
#' @importFrom quantmod Op Cl
#' @importFrom xts reclass xtsAttributes
CSPDarkCloudCover <- function(TS, n = 20, minbodysizeMedian = 1) {
  if (!(has.Op(TS) && has.Cl(TS))) {
    stop("Price series must contain Open and Close.")
  }

  LAGTS <- LagOC(TS, k = 1)
  LongCandleBody <- CSPLongCandleBody(LAGTS, n = n, threshold = minbodysizeMedian)

  DarkCloudCover <- xts::reclass(
    LongCandleBody[, "LongWhiteCandleBody"] & # first candle is white and longer than median of past n candles
      quantmod::Op(TS) > quantmod::Cl(LAGTS) & # second candle opens higher than close of 1st candle
      (quantmod::Op(LAGTS) + quantmod::Cl(LAGTS)) / 2 >= quantmod::Cl(TS) & # second candle closes at or below half of 1st candles' body
      quantmod::Cl(TS) > quantmod::Op(LAGTS), # close of second candle is higher than open of 1st candle
    TS
  )

  colnames(DarkCloudCover) <- c("DarkCloudCover")
  xts::xtsAttributes(DarkCloudCover) <- list(bars = 2)
  return(DarkCloudCover)
}
