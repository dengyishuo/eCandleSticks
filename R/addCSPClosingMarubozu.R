#' Closing Marubozu Candlestick Pattern
#'
#' Identifies Closing Marubozu patterns: candles with no shadow on the closing side.
#'
#' @param x xts or data.frame with OHLC price data
#' @param maxuppershadowCL Max upper shadow ratio. Default 0.05.
#' @param maxlowershadowCL Max lower shadow ratio. Default 0.05.
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#'
#' @details
#' Number of candle lines: \bold{1}
#'
#' BullClosingMarubozu: white candle with no upper shadow (close == high).
#' BearClosingMarubozu: black candle with no lower shadow (close == low).
#'
#' @return
#' A xts object containing the column(s):
#' \itemize{
#' \item BullClosingMarubozu: TRUE if pattern detected
#' \item BearClosingMarubozu: TRUE if pattern detected
#' }
#'
#' @family pattern-1bar
#' @family pattern-bull
#' @family pattern-bear
#' @importFrom xts reclass xtsAttributes `xtsAttributes<-`
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom zoo index
#' @importFrom tibble as_tibble
#' @export
addCSPClosingMarubozu <- function(x, maxuppershadowCL = 0.05, maxlowershadowCL = 0.05, output = c("xts", "tibble", "data.frame")) {
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

  CL      <- quantmod::Hi(TS) - quantmod::Lo(TS)
  BodyHi  <- pmax(quantmod::Op(TS), quantmod::Cl(TS))
  BodyLo  <- pmin(quantmod::Op(TS), quantmod::Cl(TS))

  BullClosingMarubozu <- xts::reclass(
    quantmod::Cl(TS) > quantmod::Op(TS) &
      (quantmod::Hi(TS) - BodyHi) <= CL * maxuppershadowCL,
    TS)

  BearClosingMarubozu <- xts::reclass(
    quantmod::Op(TS) > quantmod::Cl(TS) &
      (BodyLo - quantmod::Lo(TS)) <= CL * maxlowershadowCL,
    TS)

  result <- cbind(BullClosingMarubozu, BearClosingMarubozu)

  colnames(result) <- c("BullClosingMarubozu", "BearClosingMarubozu")
  xts::xtsAttributes(result) <- list(bars = 1)

  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df
}
