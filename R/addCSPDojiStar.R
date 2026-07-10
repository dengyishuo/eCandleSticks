#' Doji Star Candlestick Pattern
#'
#' Identifies Doji Star patterns: long candle followed by a gap doji.
#'
#' @param x xts or data.frame with OHLC price data
#' @param maxbodyCL Maximum body ratio for doji. Default 0.05.
#' @param minbodyCL1 Minimum body ratio of Bar 1. Default 0.3.
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#'
#' @details
#' Number of candle lines: \bold{2}
#'
#' @return
#' A xts object containing the column(s):
#' \itemize{
#' \item BullDojiStar: TRUE if pattern detected
#' \item BearDojiStar: TRUE if pattern detected
#' }
#'
#' @family pattern-2bar
#' @family pattern-bull
#' @family pattern-bear
#' @importFrom xts reclass xtsAttributes `xtsAttributes<-`
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom zoo index
#' @importFrom tibble as_tibble
#' @export
addCSPDojiStar <- function(x, maxbodyCL = 0.05, minbodyCL1 = 0.3, output = c("xts", "tibble", "data.frame")) {
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
  Op1  <- quantmod::Op(LAGTS)
  Cl1  <- quantmod::Cl(LAGTS)
  Op0  <- quantmod::Op(TS)
  Cl0  <- quantmod::Cl(TS)
  CL0  <- quantmod::Hi(TS) - quantmod::Lo(TS)
  BL0  <- abs(Cl0 - Op0)
  BL1  <- abs(Cl1 - Op1)
  CL1  <- abs(quantmod::Hi(LAGTS) - quantmod::Lo(LAGTS))

  isDoji <- BL0 <= CL0 * maxbodyCL

  BullDojiStar <- xts::reclass(
    Op1 > Cl1 &
      BL1 >= CL1 * minbodyCL1 &
      pmax(Op0, Cl0) < Cl1 &
      isDoji,
    TS)

  BearDojiStar <- xts::reclass(
    Cl1 > Op1 &
      BL1 >= CL1 * minbodyCL1 &
      pmin(Op0, Cl0) > Cl1 &
      isDoji,
    TS)

  result <- cbind(BullDojiStar, BearDojiStar)

  colnames(result) <- c("BullDojiStar", "BearDojiStar")
  xts::xtsAttributes(result) <- list(bars = 2)

  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df
}
