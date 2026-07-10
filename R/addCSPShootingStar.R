#' Shooting Star Candlestick Pattern
#'
#' Identifies Shooting Star patterns in an OHLC price series.
#' The shooting star is a one-day bearish reversal pattern that appears during an uptrend.
#'
#' @param x xts Time Series containing Open, High, Low and Close Prices
#' @param minuppershadowCL Minimum upper shadow to candle length ratio. Default is 2/3.
#' @param maxlowershadowCL Maximum tolerated lower shadow to candle length ratio. Default is 0.1.
#' @param minbodyCL Minimum body to candle length ratio. Default is 0.1.
#'
#' @details
#' Number of candle lines: \bold{1}
#'
#' The shooting star is a one-day formation expected to be an early sign for the reversal
#' of an uptrend into a downtrend. It has:
#' \itemize{
#' \item A long upper shadow (at least \code{minuppershadowCL} of the candle length)
#' \item A small body at the bottom of the candle (at least \code{minbodyCL} of the candle length)
#' \item No or only a very short lower shadow (at most \code{maxlowershadowCL} of the candle length)
#' \item The color of the body is not important, but a bearish (close < open) body is more significant.
#' }
#'
#' @return
#' A xts object containing the column:
#' \itemize{
#' \item ShootingStar: TRUE if shooting star pattern detected
#' }
#'
#' @references
#' \url{http://www.candlesticker.com/Cs20.asp}
#'
#' @note
#' The function filters candles that look like shooting stars, without considering
#' the current trend direction. If only shooting star patterns in an uptrend should be
#' filtered, an external trend detection function must be used.
#'
#' @seealso
#' \code{\link{addCSPHangingMan}} \code{\link{addCSPDoji}}
#'
#' @examples
#' \dontrun{
#' getSymbols("AAPL", adjust = TRUE)
#' ss <- addCSPShootingStar(AAPL)
#' # Filter for shooting stars in an uptrend
#' ss.in.uptrend <- ss & TrendDetectionChannel(AAPL)[, "UpTrend"]
#' }
#'
#' @importFrom tibble as_tibble
#' @importFrom zoo index
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#' @family pattern-1bar
#' @family pattern-bear
#' @export
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom xts reclass
addCSPShootingStar <- function(x, minuppershadowCL = 2 / 3, maxlowershadowCL = 0.1, minbodyCL = 0.1,
                              output = c("xts", "tibble", "data.frame")) {
  # ── accept data.frame / tibble input ─────────────────────────────────────
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

  if (!(has.Op(TS) && has.Hi(TS) && has.Lo(TS) && has.Cl(TS))) {
    stop("Price series must contain Open, High, Low and Close.")
  }

  CandleLength <- quantmod::Hi(TS) - quantmod::Lo(TS)
  BodyHi <- pmax(quantmod::Op(TS), quantmod::Cl(TS))
  BodyLo <- pmin(quantmod::Op(TS), quantmod::Cl(TS))

  # The logic is the mirror image of CSPHangingMan/CSPHammer
  ShootingStar <- xts::reclass(
    (quantmod::Hi(TS) - BodyHi) > CandleLength * minuppershadowCL & # Long Upper Shadow
      (BodyLo - quantmod::Lo(TS)) <= CandleLength * maxlowershadowCL & # Little to no Lower Shadow
      (BodyHi - BodyLo) > CandleLength * minbodyCL, # Body exists and meets minimum size
    TS
  )
  colnames(ShootingStar) <- c("ShootingStar")
  xts::xtsAttributes(ShootingStar) <- list(bars = 1)
  return(ShootingStar)
}
