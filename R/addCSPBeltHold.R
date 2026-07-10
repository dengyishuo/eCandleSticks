#' Belt-Hold Candlestick Pattern
#'
#' Identifies Bullish and Bearish Belt-Hold patterns in an OHLC series.
#'
#' @param x xts or data.frame with OHLC price data
#' @param n Number of periods for average body. Default 20.
#' @param minbodysizeMedian Minimum body vs average. Default 1.
#' @param maxuppershadowCL Max upper shadow ratio. Default 0.1.
#' @param maxlowershadowCL Max lower shadow ratio. Default 0.1.
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#'
#' @details
#' Number of candle lines: \bold{1}
#'
#' BullBeltHold: white candle opening at/near low with no lower shadow.
#' BearBeltHold: black candle opening at/near high with no upper shadow.
#'
#' @return
#' A xts object containing the column(s):
#' \itemize{
#' \item BullBeltHold: TRUE if pattern detected
#' \item BearBeltHold: TRUE if pattern detected
#' }
#'
#' @family pattern-1bar
#' @family pattern-bull
#' @family pattern-bear
#' @importFrom xts reclass xtsAttributes `xtsAttributes<-`
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom zoo rollmean index
#' @importFrom tibble as_tibble
#' @export
addCSPBeltHold <- function(x, n = 20, minbodysizeMedian = 1, maxuppershadowCL = 0.1, maxlowershadowCL = 0.1, output = c("xts", "tibble", "data.frame")) {
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

  CL  <- quantmod::Hi(TS) - quantmod::Lo(TS)
  BL  <- abs(quantmod::Cl(TS) - quantmod::Op(TS))
  avg_body <- zoo::rollmean(BL, k = n, fill = NA, align = "right")

  BullBeltHold <- xts::reclass(
    quantmod::Cl(TS) > quantmod::Op(TS) &
      (quantmod::Op(TS) - quantmod::Lo(TS)) <= CL * maxlowershadowCL &
      BL >= avg_body * minbodysizeMedian,
    TS)

  BearBeltHold <- xts::reclass(
    quantmod::Op(TS) > quantmod::Cl(TS) &
      (quantmod::Hi(TS) - quantmod::Op(TS)) <= CL * maxuppershadowCL &
      BL >= avg_body * minbodysizeMedian,
    TS)

  result <- cbind(BullBeltHold, BearBeltHold)

  colnames(result) <- c("BullBeltHold", "BearBeltHold")
  xts::xtsAttributes(result) <- list(bars = 1)

  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df
}
