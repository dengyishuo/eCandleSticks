#' Three Inside Up/Down Candlestick Pattern
#'
#' Identifies Three Inside Up and Three Inside Down patterns in an OHLC price series.
#' These are three-candle reversal patterns that signal potential trend changes.
#'
#' @param x xts Time Series containing Open, High, Low and Close Prices
#'
#' @details
#' Number of candle lines: \bold{3}
#'
#' \strong{Three Inside Up:}
#' \itemize{
#' \item The market is in a downtrend
#' \item The first two days form a Bullish Harami pattern
#' \item The third day confirms the reversal with a white candlestick closing with a new high for the last three days
#' \item This pattern signals a potential bullish reversal in a downtrend
#' }
#'
#' \strong{Three Inside Down:}
#' \itemize{
#' \item The market is in an uptrend
#' \item The first two days form a Bearish Harami pattern
#' \item The third day confirms the reversal with a black candlestick closing with a new low for the last three days
#' \item This pattern signals a potential bearish reversal in an uptrend
#' }
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item ThreeInsideUp: TRUE if Three Inside Up pattern detected
#' \item ThreeInsideDown: TRUE if Three Inside Down pattern detected
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
#' The function filters patterns that look like three inside up/down, without considering
#' the current trend direction. If only patterns in specific trends should be filtered,
#' an external trend detection function must be used. See examples.
#'
#' @seealso
#' \code{\link{addCSPHarami}}
#' \code{\link{addCSPThreeOutside}}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' addCSPThreeInside(YHOO)
#'
#' # Filter three inside up in downtrends
#' addCSPThreeInside(YHOO)[, "ThreeInsideUp"] &
#'   TrendDetectionChannel(lag(YHOO, k = 3))[, "DownTrend"]
#' }
#'
#' @importFrom tibble as_tibble
#' @importFrom zoo index
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#' @family pattern-3bar
#' @family pattern-bull
#' @family pattern-bear
#' @export
#' @importFrom quantmod Op Cl
#' @importFrom xts reclass xtsAttributes
#' @importFrom stats lag
addCSPThreeInside <- function(x,
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

  if (!(has.Op(TS) && has.Hi(TS) && has.Lo(TS) && has.Cl(TS))) {
    stop("Price series must contain Open, High, Low and Close.")
  }

  LAGTS2 <- stats::lag(quantmod::Op(TS), k = 2) # first candle of formation
  LAGTS1 <- LagOHLC(TS, k = 1) # second candle
  LAGHARAMI <- addCSPHarami(LAGTS1)

  TIUP <- xts::reclass(
    LAGHARAMI[, 1] & # bullish harami
      quantmod::Cl(TS) > quantmod::Op(TS) & # 3rd candle is white
      quantmod::Cl(TS) > quantmod::Cl(LAGTS1) & # close of 3rd candle greater than close of 2nd candle
      quantmod::Cl(TS) > LAGTS2, # close of 3rd candle greater than open of 1st candle
    TS
  )

  TIDOWN <- xts::reclass(
    LAGHARAMI[, 2] & # bearish harami
      quantmod::Cl(TS) < quantmod::Op(TS) & # 3rd candle is black
      quantmod::Cl(TS) < quantmod::Cl(LAGTS1) & # close of 3rd candle lower than close of 2nd candle
      quantmod::Cl(TS) < LAGTS2, # close of 3rd candle lower than open of 1st candle
    TS
  )

  result <- cbind(TIUP, TIDOWN)
  colnames(result) <- c("ThreeInsideUp", "ThreeInsideDown")
  xts::xtsAttributes(result) <- list(bars = 3)

  # ── output format ────────────────────────────────────────────────────────
  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df

}
