#' Long-Legged Doji Candlestick Pattern
#'
#' Identifies Long-Legged Doji candles: doji with long upper and lower shadows.
#'
#' @param x xts or data.frame with OHLC price data
#' @param maxbodyCL Maximum body ratio to candle length for doji. Default 0.05.
#' @param minshadowCL Minimum shadow ratio. Default 0.25.
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#'
#' @details
#' Number of candle lines: \bold{1}
#'
#' @return
#' A xts object containing the column(s):
#' \itemize{
#' \item LongLeggedDoji: TRUE if pattern detected
#' }
#'
#' @family pattern-1bar
#' @importFrom xts reclass xtsAttributes `xtsAttributes<-`
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom zoo index
#' @importFrom tibble as_tibble
#' @export
addCSPLongLeggedDoji <- function(x, maxbodyCL = 0.05, minshadowCL = 0.25, output = c("xts", "tibble", "data.frame")) {
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

  CL         <- quantmod::Hi(TS) - quantmod::Lo(TS)
  BL         <- abs(quantmod::Cl(TS) - quantmod::Op(TS))
  BodyHi     <- pmax(quantmod::Op(TS), quantmod::Cl(TS))
  BodyLo     <- pmin(quantmod::Op(TS), quantmod::Cl(TS))
  UpperShadow <- quantmod::Hi(TS) - BodyHi
  LowerShadow <- BodyLo - quantmod::Lo(TS)

  LongLeggedDoji <- xts::reclass(
    BL <= CL * maxbodyCL &
      UpperShadow >= CL * minshadowCL &
      LowerShadow >= CL * minshadowCL,
    TS)

  result <- LongLeggedDoji

  colnames(result) <- c("LongLeggedDoji")
  xts::xtsAttributes(result) <- list(bars = 1)

  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df
}
