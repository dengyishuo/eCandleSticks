#' Stick Sandwich Candlestick Pattern
#'
#' Identifies Stick Sandwich: black, white, black with outer blacks closing at same level.
#'
#' @param x xts or data.frame with OHLC price data
#' @param n Periods for average. Default 20.
#' @param tolerance Close tolerance. Default 0.05.
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#'
#' @details
#' Number of candle lines: \bold{3}
#'
#' @return
#' A xts object containing the column(s):
#' \itemize{
#' \item StickSandwich: TRUE if pattern detected
#' }
#'
#' @family pattern-3bar
#' @family pattern-bull
#' @importFrom xts reclass xtsAttributes `xtsAttributes<-`
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom stats lag
#' @importFrom zoo rollmean index
#' @importFrom tibble as_tibble
#' @export
addCSPStickSandwich <- function(x, n = 20, tolerance = 0.05, output = c("xts", "tibble", "data.frame")) {
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

  Op2 <- stats::lag(quantmod::Op(TS), -2)
  Cl2 <- stats::lag(quantmod::Cl(TS), -2)
  Op1 <- stats::lag(quantmod::Op(TS), -1)
  Cl1 <- stats::lag(quantmod::Cl(TS), -1)
  Op0 <- quantmod::Op(TS)
  Cl0 <- quantmod::Cl(TS)
  CL  <- quantmod::Hi(TS) - quantmod::Lo(TS)
  avg_CL <- zoo::rollmean(CL, k = n, fill = NA, align = "right")

  StickSandwich <- xts::reclass(
    Op2 > Cl2 &
      Cl1 > Op1 &
      Op1 > Cl2 &
      Op0 > Cl0 &
      abs(Cl0 - Cl2) <= avg_CL * tolerance,
    TS)

  result <- StickSandwich

  colnames(result) <- c("StickSandwich")
  xts::xtsAttributes(result) <- list(bars = 3)

  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df
}
