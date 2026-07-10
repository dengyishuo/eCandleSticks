#' Rickshaw Man Candlestick Pattern
#'
#' Identifies Rickshaw Man candles: doji with body near candle midpoint.
#'
#' @param x xts or data.frame with OHLC price data
#' @param maxbodyCL Maximum body ratio. Default 0.05.
#' @param maxmiddevCL Maximum deviation of body from midpoint. Default 0.1.
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#'
#' @details
#' Number of candle lines: \bold{1}
#'
#' @return
#' A xts object containing the column(s):
#' \itemize{
#' \item RickshawMan: TRUE if pattern detected
#' }
#'
#' @family pattern-1bar
#' @importFrom xts reclass xtsAttributes `xtsAttributes<-`
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom zoo index
#' @importFrom tibble as_tibble
#' @export
addCSPRickshawMan <- function(x, maxbodyCL = 0.05, maxmiddevCL = 0.1, output = c("xts", "tibble", "data.frame")) {
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
  BL      <- abs(quantmod::Cl(TS) - quantmod::Op(TS))
  BodyMid <- (quantmod::Op(TS) + quantmod::Cl(TS)) / 2
  CandMid <- (quantmod::Hi(TS) + quantmod::Lo(TS)) / 2

  RickshawMan <- xts::reclass(
    BL <= CL * maxbodyCL &
      abs(BodyMid - CandMid) <= CL * maxmiddevCL,
    TS)

  result <- RickshawMan

  colnames(result) <- c("RickshawMan")
  xts::xtsAttributes(result) <- list(bars = 1)

  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df
}
