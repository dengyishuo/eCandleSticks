#' Identical Three Crows Candlestick Pattern
#'
#' Identifies Identical Three Crows: three black candles each opening at the prior close.
#'
#' @param x xts or data.frame with OHLC price data
#' @param n Periods for average candle length. Default 20.
#' @param tolerance Open tolerance. Default 0.1.
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#'
#' @details
#' Number of candle lines: \bold{3}
#'
#' @return
#' A xts object containing the column(s):
#' \itemize{
#' \item Identical3Crows: TRUE if pattern detected
#' }
#'
#' @family pattern-3bar
#' @family pattern-bear
#' @importFrom xts reclass xtsAttributes `xtsAttributes<-`
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom stats lag
#' @importFrom zoo rollmean index
#' @importFrom tibble as_tibble
#' @export
addCSPIdentical3Crows <- function(x, n = 20, tolerance = 0.1, output = c("xts", "tibble", "data.frame")) {
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
  Hi0 <- quantmod::Hi(TS)
  CL  <- Hi0 - quantmod::Lo(TS)
  avg_CL <- zoo::rollmean(CL, k = n, fill = NA, align = "right")

  Identical3Crows <- xts::reclass(
    Op2 > Cl2 & Op1 > Cl1 & Op0 > Cl0 &
      abs(Op1 - Cl2) <= avg_CL * tolerance &
      abs(Op0 - Cl1) <= avg_CL * tolerance,
    TS)

  result <- Identical3Crows

  colnames(result) <- c("Identical3Crows")
  xts::xtsAttributes(result) <- list(bars = 3)

  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df
}
