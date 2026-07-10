#' On-Neck Candlestick Pattern
#'
#' Identifies On-Neck pattern: black candle then white closing at Bar 1 low.
#'
#' @param x xts or data.frame with OHLC price data
#' @param tolerance Tolerance as ratio of candle length. Default 0.05.
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#'
#' @details
#' Number of candle lines: \bold{2}
#'
#' @return
#' A xts object containing the column(s):
#' \itemize{
#' \item OnNeck: TRUE if pattern detected
#' }
#'
#' @family pattern-2bar
#' @family pattern-bear
#' @importFrom xts reclass xtsAttributes `xtsAttributes<-`
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom zoo index
#' @importFrom tibble as_tibble
#' @export
addCSPOnNeck <- function(x, tolerance = 0.05, output = c("xts", "tibble", "data.frame")) {
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

  LAGTS  <- LagOHLC(TS, k = 1)
  Op1 <- quantmod::Op(LAGTS)
  Hi1 <- quantmod::Hi(LAGTS)
  Lo1 <- quantmod::Lo(LAGTS)
  Cl1 <- quantmod::Cl(LAGTS)
  Op0 <- quantmod::Op(TS)
  Cl0 <- quantmod::Cl(TS)
  CL1 <- Hi1 - Lo1

  OnNeck <- xts::reclass(
    Op1 > Cl1 &
      Op0 < Lo1 &
      Cl0 > Op0 &
      abs(Cl0 - Lo1) <= CL1 * tolerance,
    TS)

  result <- OnNeck

  colnames(result) <- c("OnNeck")
  xts::xtsAttributes(result) <- list(bars = 2)

  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df
}
