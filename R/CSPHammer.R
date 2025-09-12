#' Hammer Candlestick Pattern
#'
#' Identifies Hammer patterns in an OHLC price series.
#' The hammer is a one-day bullish reversal pattern that appears during a downtrend.
#'
#' @param TS xts Time Series containing Open, High, Low and Close Prices
#' @param minlowershadowCL Minimum lower shadow to candle length ratio. Default is 2/3.
#' @param maxuppershadowCL Maximum tolerated upper shadow to candle length ratio. Default is 0.1.
#' @param minbodyCL Minimum body to candle length ratio. Default is 0.1.
#'
#' @details
#' Number of candle lines: \bold{1}
#'
#' The hammer is a one-day formation expected to be an early sign for the reversal
#' of a downtrend into an uptrend. It has:
#' \itemize{
#' \item A long lower shadow (at least \code{minlowershadowCL} of the candle length)
#' \item A small body at the top of the candle (at least \code{minbodyCL} of the candle length)
#' \item No or only a very short upper shadow (at most \code{maxuppershadowCL} of the candle length)
#' \item The color of the body is not important
#' }
#'
#' @return
#' A xts object containing the column:
#' \itemize{
#' \item Hammer: TRUE if hammer pattern detected
#' }
#'
#' @references
#' The following site was used to code/document this candlestick pattern:
#' \url{http://www.candlesticker.com/Cs18.asp}
#'
#' @note
#' The function filters candles that look like hammers, without considering
#' the current trend direction. If only hammer patterns in a downtrend should be
#' filtered, an external trend detection function must be used. See examples.
#'
#' @seealso
#' \code{\link{CSPDoji}}
#' \code{\link{TrendDetectionChannel}}
#' \code{\link{TrendDetectionSMA}}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#'
#' # Filter for hammer patterns
#' CSPHammer(YHOO)
#'
#' # Filter for hammer patterns that occur in downtrends
#' CSPHammer(YHOO) & TrendDetectionChannel(YHOO)[, "DownTrend"]
#' }
#'
#' @export
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom xts reclass xtsAttributes
CSPHammer <- function(TS, minlowershadowCL = 2 / 3, maxuppershadowCL = 0.1, minbodyCL = 0.1) {
  if (!(has.Op(TS) && has.Hi(TS) && has.Lo(TS) && has.Cl(TS))) {
    stop("Price series must contain Open, High, Low and Close.")
  }

  CL <- quantmod::Hi(TS) - quantmod::Lo(TS)
  BodyHi <- pmax(quantmod::Op(TS), quantmod::Cl(TS))
  BodyLo <- pmin(quantmod::Op(TS), quantmod::Cl(TS))

  Hammer <- xts::reclass(
    (BodyLo - quantmod::Lo(TS)) > CL * minlowershadowCL & # lower shadow greater than minlowershadowCL * CandleLength
      (quantmod::Hi(TS) - BodyHi) <= CL * maxuppershadowCL & # upper shadow missing or very short
      abs(quantmod::Cl(TS) - quantmod::Op(TS)) > CL * minbodyCL, # Body length greater than minbodyCL * CandleLength
    TS
  )

  colnames(Hammer) <- c("Hammer")
  xts::xtsAttributes(Hammer) <- list(bars = 1)
  return(Hammer)
}
