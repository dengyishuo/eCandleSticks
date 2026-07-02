#' Shooting Star Candlestick Pattern
#'
#' Identifies Shooting Star patterns in an OHLC price series.
#' The shooting star is a one-day bearish reversal pattern that appears during an uptrend.
#'
#' @param TS xts Time Series containing Open, High, Low and Close Prices
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
#' \code{\link{CSPHangingMan}} \code{\link{CSPDoji}}
#'
#' @examples
#' \dontrun{
#' getSymbols("AAPL", adjust = TRUE)
#' ss <- CSPShootingStar(AAPL)
#' # Filter for shooting stars in an uptrend
#' ss.in.uptrend <- ss & TrendDetectionChannel(AAPL)[, "UpTrend"]
#' }
#'
#' @export
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom xts reclass
CSPShootingStar <- function(TS, minuppershadowCL = 2 / 3, maxlowershadowCL = 0.1, minbodyCL = 0.1) {
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
