#' Gap Side-by-Side White Lines
#'
#' Identifies Gap Side-by-Side White Lines: a gap followed by two side-by-side white candles.
#'
#' @param x xts or data.frame with OHLC price data
#' @param tolerance Size/open tolerance. Default 0.1.
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#'
#' @details
#' Number of candle lines: \bold{3}
#'
#' @return
#' A xts object containing the column(s):
#' \itemize{
#' \item BullGapSideSideWhite: TRUE if pattern detected
#' \item BearGapSideSideWhite: TRUE if pattern detected
#' }
#'
#' @family pattern-3bar
#' @family pattern-bull
#' @family pattern-bear
#' @importFrom xts reclass xtsAttributes `xtsAttributes<-`
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom stats lag
#' @importFrom zoo index
#' @importFrom tibble as_tibble
#' @export
addCSPGapSideSideWhite <- function(x, tolerance = 0.1, output = c("xts", "tibble", "data.frame")) {
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
  Lo0 <- quantmod::Lo(TS)
  Cl0 <- quantmod::Cl(TS)

  CL1  <- Cl1 - Op1
  CL0  <- Cl0 - Op0
  SideBySide <- Cl1 > Op1 & Cl0 > Op0 & abs(CL0 - CL1) <= CL1 * tolerance &
                abs(Op0 - Op1) <= (Cl1 - Op1) * tolerance

  BullGapSideSideWhite <- xts::reclass(
    Lo1 > Hi2 & SideBySide,
    TS)

  BearGapSideSideWhite <- xts::reclass(
    Hi1 < Lo2 & SideBySide,
    TS)

  result <- cbind(BullGapSideSideWhite, BearGapSideSideWhite)

  colnames(result) <- c("BullGapSideSideWhite", "BearGapSideSideWhite")
  xts::xtsAttributes(result) <- list(bars = 3)

  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df
}
