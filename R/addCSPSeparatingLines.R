#' Separating Lines Candlestick Pattern
#'
#' Identifies Separating Lines: two opposite candles opening at the same price.
#'
#' @param x xts or data.frame with OHLC price data
#' @param n Periods for average. Default 20.
#' @param tolerance Open tolerance. Default 0.05.
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#'
#' @details
#' Number of candle lines: \bold{2}
#'
#' @return
#' A xts object containing the column(s):
#' \itemize{
#' \item BullSeparatingLines: TRUE if pattern detected
#' \item BearSeparatingLines: TRUE if pattern detected
#' }
#'
#' @family pattern-2bar
#' @family pattern-bull
#' @family pattern-bear
#' @importFrom xts reclass xtsAttributes `xtsAttributes<-`
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom zoo rollmean index
#' @importFrom tibble as_tibble
#' @export
addCSPSeparatingLines <- function(x, n = 20, tolerance = 0.05, output = c("xts", "tibble", "data.frame")) {
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

  LAGTS <- LagOC(TS, k = 1)
  Op1 <- quantmod::Op(LAGTS)
  Cl1 <- quantmod::Cl(LAGTS)
  Op0 <- quantmod::Op(TS)
  Cl0 <- quantmod::Cl(TS)
  CL  <- quantmod::Hi(TS) - quantmod::Lo(TS)
  avg_CL <- zoo::rollmean(CL, k = n, fill = NA, align = "right")

  BullSeparatingLines <- xts::reclass(
    Op1 > Cl1 &
      Cl0 > Op0 &
      abs(Op0 - Op1) <= avg_CL * tolerance,
    TS)

  BearSeparatingLines <- xts::reclass(
    Cl1 > Op1 &
      Op0 > Cl0 &
      abs(Op0 - Op1) <= avg_CL * tolerance,
    TS)

  result <- cbind(BullSeparatingLines, BearSeparatingLines)

  colnames(result) <- c("BullSeparatingLines", "BearSeparatingLines")
  xts::xtsAttributes(result) <- list(bars = 2)

  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df
}
