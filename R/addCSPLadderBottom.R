#' Ladder Bottom Candlestick Pattern
#'
#' Identifies Ladder Bottom: three declining blacks, black with long upper shadow, white gap up.
#'
#' @param x xts or data.frame with OHLC price data
#' @param minuppershadow Minimum upper shadow ratio of Bar 4. Default 0.3.
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#'
#' @details
#' Number of candle lines: \bold{5}
#'
#' @return
#' A xts object containing the column(s):
#' \itemize{
#' \item LadderBottom: TRUE if pattern detected
#' }
#'
#' @family pattern-5bar
#' @family pattern-bull
#' @importFrom xts reclass xtsAttributes `xtsAttributes<-`
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom stats lag
#' @importFrom zoo index
#' @importFrom tibble as_tibble
#' @export
addCSPLadderBottom <- function(x, minuppershadow = 0.3, output = c("xts", "tibble", "data.frame")) {
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

  Op4 <- stats::lag(quantmod::Op(TS), -4)
  Cl4 <- stats::lag(quantmod::Cl(TS), -4)
  Op3 <- stats::lag(quantmod::Op(TS), -3)
  Cl3 <- stats::lag(quantmod::Cl(TS), -3)
  Op2 <- stats::lag(quantmod::Op(TS), -2)
  Cl2 <- stats::lag(quantmod::Cl(TS), -2)
  Op1 <- stats::lag(quantmod::Op(TS), -1)
  Hi1 <- stats::lag(quantmod::Hi(TS), -1)
  Cl1 <- stats::lag(quantmod::Cl(TS), -1)
  Op0 <- quantmod::Op(TS)
  Cl0 <- quantmod::Cl(TS)
  CL1 <- Hi1 - stats::lag(quantmod::Lo(TS), -1)

  LadderBottom <- xts::reclass(
    Op4 > Cl4 & Op3 > Cl3 & Op2 > Cl2 &
      Cl3 < Cl4 & Cl2 < Cl3 &
      Op1 > Cl1 &
      (Hi1 - Op1) >= CL1 * minuppershadow &
      Cl0 > Op0 &
      Op0 > Op1,
    TS)

  result <- LadderBottom

  colnames(result) <- c("LadderBottom")
  xts::xtsAttributes(result) <- list(bars = 5)

  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df
}
