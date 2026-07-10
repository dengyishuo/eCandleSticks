#' Harami Candlestick Pattern
#'
#' Identifies Bullish and Bearish Harami Patterns in an OHLC price series.
#' Harami is a two-candle reversal pattern that signals potential trend changes.
#'
#' @param x xts Time Series containing Open and Close Prices
#' @param n Number of preceding candles to calculate median candle body length. Default is 20.
#' @param minbodysizeMedian Minimum body length of the first candle (mother candle)
#' relative to the median of the past \code{n} body sizes. Default is 1.
#'
#' @details
#' Number of candle lines: \bold{2}
#'
#' \strong{Bullish Harami Pattern:}
#' \itemize{
#' \item A large black candlestick (mother candle) is followed by a smaller white candlestick
#' \item The body of the second candle is completely within the vertical range of the first candle's body
#' \item The pattern appears during a downtrend and signals a potential reversal upward
#' }
#'
#' \strong{Bearish Harami Pattern:}
#' \itemize{
#' \item A large white candlestick (mother candle) is followed by a smaller black candlestick
#' \item The body of the second candle is completely within the vertical range of the first candle's body
#' \item The pattern appears during an uptrend and signals a potential reversal downward
#' }
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item Bull.Harami: TRUE if bullish harami pattern detected
#' \item Bear.Harami: TRUE if bearish harami pattern detected
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
#' The function filters patterns that look like haramis, without considering
#' the current trend direction. If only patterns in specific trends should be
#' filtered, an external trend detection function must be used. See examples.
#'
#' @seealso
#' \code{\link{addCSPLongCandleBody}}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' addCSPHarami(YHOO)
#'
#' # Filter for bullish harami that occur in downtrends
#' BullHarami <- addCSPHarami(YHOO)[, "Bull.Harami"] &
#'   TrendDetectionChannel(lag(YHOO, k = 2))[, "DownTrend"]
#' }
#'
#' @importFrom tibble as_tibble
#' @importFrom zoo index
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#' @family pattern-2bar
#' @family pattern-bull
#' @family pattern-bear
#' @export
#' @importFrom quantmod Op Cl
#' @importFrom xts reclass xtsAttributes
addCSPHarami <- function(x, n = 20, minbodysizeMedian = 1,
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

  BullHarami <- xts::reclass(
    LongCandleBody[, 2] & # body of mother candle is black and longer than average
      quantmod::Cl(TS) > quantmod::Op(TS) & # second candle is white
      quantmod::Op(LAGTS) > quantmod::Cl(TS) & quantmod::Cl(LAGTS) < quantmod::Op(TS), # second body is within first body
    TS
  )

  BearHarami <- xts::reclass(
    LongCandleBody[, 1] & # body of mother candle is white and longer than average
      quantmod::Cl(TS) < quantmod::Op(TS) & # second candle is black
      quantmod::Op(LAGTS) < quantmod::Cl(TS) & quantmod::Cl(LAGTS) > quantmod::Op(TS), # second body is within first body
    TS
  )

  result <- cbind(BullHarami, BearHarami)
  colnames(result) <- c("Bull.Harami", "Bear.Harami")
  xts::xtsAttributes(result) <- list(bars = 2)

  # ── output format ────────────────────────────────────────────────────────
  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df

}
