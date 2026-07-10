#' Three Stars in the South
#'
#' Identifies Three Stars in the South: three black candles with shrinking range and body.
#'
#' @param x xts or data.frame with OHLC price data
#' @param maxshadow Maximum shadow ratio for last candle. Default 0.1.
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#'
#' @details
#' Number of candle lines: \bold{3}
#'
#' @return
#' A xts object containing the column(s):
#' \itemize{
#' \item ThreeStarsInSouth: TRUE if pattern detected
#' }
#'
#' @family pattern-3bar
#' @family pattern-bull
#' @importFrom xts reclass xtsAttributes `xtsAttributes<-`
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom stats lag
#' @importFrom zoo index
#' @importFrom tibble as_tibble
#' @export
addCSPThreeStarsInSouth <- function(x, maxshadow = 0.1, output = c("xts", "tibble", "data.frame")) {
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
  Hi2 <- stats::lag(quantmod::Hi(TS), -2)
  Lo2 <- stats::lag(quantmod::Lo(TS), -2)
  Cl2 <- stats::lag(quantmod::Cl(TS), -2)
  Op1 <- stats::lag(quantmod::Op(TS), -1)
  Hi1 <- stats::lag(quantmod::Hi(TS), -1)
  Lo1 <- stats::lag(quantmod::Lo(TS), -1)
  Cl1 <- stats::lag(quantmod::Cl(TS), -1)
  Op0 <- quantmod::Op(TS)
  Hi0 <- quantmod::Hi(TS)
  Lo0 <- quantmod::Lo(TS)
  Cl0 <- quantmod::Cl(TS)
  CL0 <- Hi0 - Lo0

  ThreeStarsInSouth <- xts::reclass(
    Op2 > Cl2 & Op1 > Cl1 & Op0 > Cl0 &
      Lo1 > Lo2 & Hi1 <= Hi2 &
      Lo0 >= Lo1 & Hi0 <= Hi1 &
      (Hi0 - Op0) <= CL0 * maxshadow &
      (Cl0 - Lo0) <= CL0 * maxshadow,
    TS)

  result <- ThreeStarsInSouth

  colnames(result) <- c("ThreeStarsInSouth")
  xts::xtsAttributes(result) <- list(bars = 3)

  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df
}
