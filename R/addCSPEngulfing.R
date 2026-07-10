#' Engulfing Candlestick Pattern
#'
#' Identifies Bullish and Bearish Engulfing Patterns in an Open/Close price series.
#'
#' @param x xts Time Series containing Open and Close prices
#'
#' @details
#' Number of candle lines: \bold{2}
#'
#' \strong{Bullish Engulfing Pattern:}
#' \itemize{
#' \item Prior trend: \bold{down}
#' \item Characterized by a large white real body engulfing a preceding small black real body
#' \item The white body does not necessarily engulf the shadows of the black body but totally engulfs the body itself
#' \item Appears during a downtrend
#' }
#'
#' \strong{Bearish Engulfing Pattern:}
#' \itemize{
#' \item Prior trend: \bold{up}
#' \item Characterized by a large black real body engulfing a preceding small white real body
#' \item The black body does not necessarily engulf the shadows of the white body but totally engulfs the body itself
#' \item Appears during an uptrend
#' }
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item Bull.Engulfing: TRUE if bullish engulfing pattern detected
#' \item Bear.Engulfing: TRUE if bearish engulfing pattern detected
#' }
#'
#' @references
#' The following sites were used to code/document this candlestick pattern:
#' \itemize{
#' \item \url{http://thepatternsite.com/BullEngulfing.html}
#' \item \url{http://www.onlinetradingconcepts.com/TechnicalAnalysis/Candlesticks/BullishEngulfing.html}
#' \item \url{http://www.candlesticker.com/Bullish.asp}
#' \item \url{http://www.candlesticker.com/Bearish.asp}
#' }
#'
#' @note
#' The function filters patterns that look like engulfing patterns, without considering
#' the current trend direction. If only patterns in uptrends/downtrends should be filtered,
#' an external trend detection function must be used. See examples.
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' addCSPEngulfing(YHOO)
#'
#' # Filter bearish engulfing patterns that occur in uptrends
#' addCSPEngulfing(YHOO)[, "Bear.Engulfing"] &
#'   TrendDetectionChannel(YHOO)[, "UpTrend"]
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
addCSPEngulfing <- function(x,
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

  BullEngulfing <- xts::reclass(
    quantmod::Op(LAGTS) > quantmod::Cl(LAGTS) & # First candle is black
      quantmod::Cl(TS) > quantmod::Op(TS) & # Second candle is white
      quantmod::Cl(LAGTS) >= quantmod::Op(TS) & # Second candle open <= first candle close
      quantmod::Cl(TS) >= quantmod::Op(LAGTS), # Second candle close >= first candle open
    TS
  )

  BearEngulfing <- xts::reclass(
    quantmod::Cl(LAGTS) > quantmod::Op(LAGTS) & # First candle is white
      quantmod::Op(TS) > quantmod::Cl(TS) & # Second candle is black
      quantmod::Op(LAGTS) >= quantmod::Cl(TS) & # Second candle close <= first candle open
      quantmod::Op(TS) >= quantmod::Cl(LAGTS), # Second candle open >= first candle close
    TS
  )

  result <- cbind(BullEngulfing, BearEngulfing)
  colnames(result) <- c("Bull.Engulfing", "Bear.Engulfing")
  xts::xtsAttributes(result) <- list(bars = 2)

  # ── output format ────────────────────────────────────────────────────────
  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df

}
