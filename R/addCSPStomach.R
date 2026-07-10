#' Above/Below The Stomach Candlestick Pattern
#'
#' Identifies Above/Below The Stomach patterns in an Open/Close price series.
#' These are two-candle reversal patterns that signal potential trend changes.
#'
#' @param x xts Time Series containing Open and Close Prices
#'
#' @details
#' Number of candle lines: \bold{2}
#'
#' \strong{Above The Stomach:}
#' \itemize{
#' \item Prior trend: \bold{down}
#' \item A black candle followed by a white candle
#' \item The opening price of the second day must be at or above the midpoint of the first candle's body
#' \item This pattern signals a potential bullish reversal in a downtrend
#' }
#'
#' \strong{Below The Stomach:}
#' \itemize{
#' \item Prior trend: \bold{up}
#' \item A white candle followed by a black candle
#' \item The opening price of the second day must be at or below the midpoint of the first candle's body
#' \item This pattern signals a potential bearish reversal in an uptrend
#' }
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item AboveTheStomach: TRUE if Above The Stomach pattern detected
#' \item BelowTheStomach: TRUE if Below The Stomach pattern detected
#' }
#'
#' @references
#' The following sites were used to code/document this indicator:
#' \itemize{
#' \item \url{http://www.thepatternsite.com/AboveStomach.html}
#' \item \url{http://www.thepatternsite.com/BelowStomach.html}
#' }
#'
#' @note
#' The function filters patterns that look like above/below the Stomach, without considering
#' the current trend direction. If only patterns in specific trends should be filtered,
#' an external trend detection function must be used. See examples.
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' addCSPStomach(YHOO)
#'
#' # Filter above the Stomach pattern in downtrends
#' addCSPStomach(YHOO)[, "AboveTheStomach"] &
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
#' @importFrom xts reclass xtsAttributes as.xts
addCSPStomach <- function(x,
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

  #  Above the Stomach pattern:

  #    Previous candle is bearish (red)

  #  Current candle is bullish (green)

  #  Current candle's open is above the midpoint of previous candle

  AboveTheStomach <- xts::reclass(
    quantmod::Op(LAGTS) > quantmod::Cl(LAGTS) & # Previous candle is bearish
      quantmod::Cl(TS) > quantmod::Op(TS) & # Current candle is bullish
      quantmod::Op(TS) >= ((quantmod::Op(LAGTS) + quantmod::Cl(LAGTS)) / 2), # Current open above previous midpoint
    TS
  )

  # Below the Stomach pattern:

  # Previous candle is bullish (green)

  # Current candle is bearish (red)

  # Current candle's open is below the midpoint of previous candle

  BelowTheStomach <- xts::reclass(
    quantmod::Cl(LAGTS) > quantmod::Op(LAGTS) & # Previous candle is bullish
      quantmod::Op(TS) > quantmod::Cl(TS) & # Current candle is bearish
      quantmod::Op(TS) <= ((quantmod::Op(LAGTS) + quantmod::Cl(LAGTS)) / 2), # Current open below previous midpoint
    TS
  )

  # Combine results

  result <- xts::as.xts(cbind(AboveTheStomach, BelowTheStomach))
  colnames(result) <- c("AboveTheStomach", "BelowTheStomach")
  xts::xtsAttributes(result) <- list(bars = 2)

  # ── output format ────────────────────────────────────────────────────────
  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df

}
