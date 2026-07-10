#' Stalled Pattern Candlestick
#'
#' Identifies Stalled Pattern: three white candles, last is small, opening near prior close.
#'
#' @param x xts or data.frame with OHLC price data
#' @param smallBodyRatio Max ratio of last body to prior. Default 0.5.
#' @param maxOpenGap Max open distance from prior close. Default 0.5.
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#'
#' @details
#' Number of candle lines: \bold{3}
#'
#' @return
#' A xts object containing the column(s):
#' \itemize{
#' \item StalledPattern: TRUE if pattern detected
#' }
#'
#' @family pattern-3bar
#' @family pattern-bear
#' @importFrom xts reclass xtsAttributes `xtsAttributes<-`
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom stats lag
#' @importFrom zoo index
#' @importFrom tibble as_tibble
#' @export
addCSPStalledPattern <- function(x, smallBodyRatio = 0.5, maxOpenGap = 0.5, output = c("xts", "tibble", "data.frame")) {
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
  Hi0 <- quantmod::Hi(TS)
  Cl0 <- quantmod::Cl(TS)

  BL2 <- Cl2 - Op2
  BL1 <- Cl1 - Op1
  BL0 <- Cl0 - Op0

  StalledPattern <- xts::reclass(
    Cl2 > Op2 & Cl1 > Op1 & Cl0 > Op0 &
      Cl1 > Cl2 & Cl0 > Cl1 &
      BL0 <= BL1 * smallBodyRatio &
      Op0 >= Cl1 - BL1 * maxOpenGap,
    TS)

  result <- StalledPattern

  colnames(result) <- c("StalledPattern")
  xts::xtsAttributes(result) <- list(bars = 3)

  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df
}
