#' Dark Cloud Cover Candlestick Pattern
#'
#' Identifies Dark Cloud Cover patterns in an Open/Close price series.
#' This is a two-candle bearish reversal pattern that occurs during an uptrend.
#'
#' @param x xts Time Series containing Open and Close prices
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
#' The counterpart of this pattern is \code{\link{addCSPPiercingPattern}}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' addCSPDarkCloudCover(YHOO)
#'
#' # Filter dark cloud covers that occur in uptrends.
#' # The lag of 2 periods of the time series for trend detection
#' # ensures that the uptrend is active before the
#' # dark cloud cover occurs.
#' addCSPDarkCloudCover(YHOO) &
#'   TrendDetectionChannel(lag(YHOO, k = 2))[, "UpTrend"]
#' }
#'
#' @importFrom tibble as_tibble
#' @importFrom zoo index
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#' @family pattern-2bar
#' @family pattern-bear
#' @export
#' @importFrom quantmod Op Cl
#' @importFrom xts reclass xtsAttributes
addCSPDarkCloudCover <- function(x, n = 20, minbodysizeMedian = 1,
                              output = c("xts", "tibble", "data.frame")) {
  # ── accept data.frame / tibble input ─────────────────────────────────────
  if (!xts::is.xts(x)) {
    nms <- tolower(colnames(x))
    date_col  <- colnames(x)[nms %in% c("date", "time", "index")][1]
    open_col  <- colnames(x)[nms == "open"][1]
    high_col  <- colnames(x)[nms == "high"][1]
    low_col   <- colnames(x)[nms == "low"][1]
    close_col <- colnames(x)[nms == "close"][1]
    if (any(is.na(c(date_col, open_col, high_col, low_col, close_col))))
      stop("x must contain open/high/low/close columns or be an xts OHLC object.")
    mat <- as.matrix(x[, c(open_col, high_col, low_col, close_col)])
    colnames(mat) <- c("Open", "High", "Low", "Close")
    x <- xts::xts(mat, order.by = as.Date(x[[date_col]]))
  }
  TS <- x

  if (!(has.Op(TS) && has.Cl(TS))) {
    stop("Price series must contain Open and Close.")
  }

  LAGTS <- LagOC(TS, k = 1)
  LongCandleBody <- addCSPLongCandleBody(LAGTS, n = n, threshold = minbodysizeMedian)

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
