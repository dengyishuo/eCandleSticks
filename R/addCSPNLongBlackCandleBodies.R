#' N Consecutive Long Black Candle Bodies
#'
#' Identifies sequences of N consecutive long black candle bodies in an OHLC price series.
#'
#' @param x xts Time Series containing Open and Close prices
#' @param N Number of consecutive candles to test. Default is 2.
#' @param n Number of preceding candles to calculate median candle body length. Default is 20.
#' @param threshold Minimum candle body length in relation to the median candle body length
#' of \code{n} preceding candles. Default is 1.
#'
#' @details
#' This function identifies when the current candle is the N-th consecutive long black candle body.
#' A candle body is considered "long" if its length is greater than or equal to the threshold
#' times the median candle body length of the preceding n candles.
#'
#' @return
#' A xts object containing the column:
#' \itemize{
#' \item {N}LongBlackCandleBodies: TRUE if current candle is the \code{N}-th consecutive long black candle body
#' }
#'
#' @author Andreas Voellenklee
#'
#' @seealso
#' \code{\link{addCSPLongCandleBody}}
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' addCSPNLongBlackCandleBodies(YHOO, N = 4, n = 50, threshold = 1.2)
#' }
#'
#' @importFrom tibble as_tibble
#' @importFrom zoo index
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#' @family pattern-nbar
#' @family pattern-bear
#' @export
#' @importFrom xts reclass xtsAttributes as.xts
#' @importFrom stats lag
addCSPNLongBlackCandleBodies <- function(x, N = 2, n = 20, threshold = 1,
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

  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  if (N < 1) {
    stop("N has to be a integer >= 1")
  }
  LBCB <- addCSPLongCandleBody(TS, n = n, threshold = threshold)[, 2] # LongBlackCandleBody
  result <- xts::reclass(xts::as.xts(apply(stats::lag(LBCB, k = 0:(N - 1)), 1, all)), TS)
  colnames(result) <- paste(N, "LongBlackCandleBodies", sep = "")
  xts::xtsAttributes(result) <- list(bars = N)

  # ── output format ────────────────────────────────────────────────────────
  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df

}
