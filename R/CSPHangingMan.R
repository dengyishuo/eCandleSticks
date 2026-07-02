#' Hanging Man Candlestick Pattern
#'
#' Identifies Hanging Man patterns in an OHLC price series.
#' The hanging man is a one-day bearish reversal pattern that appears during an uptrend.
#'
#' @param TS xts Time Series containing Open, High, Low and Close Prices
#' @param minlowershadowCL Minimum lower shadow to candle length ratio. Default is 2/3.
#' @param maxuppershadowCL Maximum tolerated upper shadow to candle length ratio. Default is 0.1.
#' @param minbodyCL Minimum body to candle length ratio. Default is 0.1.
#'
#' @details
#' Number of candle lines: \bold{1}
#'
#' The hanging man is a one-day formation expected to be an early sign for the reversal
#' of an uptrend into a downtrend. It has:
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
#' \item HangingMan: TRUE if hanging man pattern detected
#' }
#'
#' @references
#' The following site was used to code/document this candlestick pattern:
#' \url{http://www.candlesticker.com/Cs19.asp}
#'
#' @note
#' The function filters candles that look like hanging men, without considering
#' the current trend direction. If only hanging man patterns in an uptrend should be
#' filtered, an external trend detection function must be used. See examples.
#'
#' @seealso
#' \code{\link{CSPHammer}}
#' \code{\link{TrendDetectionChannel}}
#' \code{\link{TrendDetectionSMA}}
#'
#' @author [Your Name]
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#'
#' # Filter for hanging man patterns
#' CSPHangingMan(YHOO)
#'
#' # Filter for hanging man patterns that occur in uptrends
#' CSPHangingMan(YHOO) & TrendDetectionChannel(YHOO)[, "UpTrend"]
#' }
#'
#' @export
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom xts reclass xtsAttributes
CSPHangingMan <- function(TS, minlowershadowCL = 2 / 3, maxuppershadowCL = 0.1, minbodyCL = 0.1) {
  if (!(has.Op(TS) && has.Hi(TS) && has.Lo(TS) && has.Cl(TS))) {
    stop("Price series must contain Open, High, Low and Close.")
  }

  CL <- quantmod::Hi(TS) - quantmod::Lo(TS)
  BodyHi <- pmax(quantmod::Op(TS), quantmod::Cl(TS))
  BodyLo <- pmin(quantmod::Op(TS), quantmod::Cl(TS))

  HangingMan <- xts::reclass(
    (BodyLo - quantmod::Lo(TS)) > CL * minlowershadowCL & # lower shadow greater than minlowershadowCL * CandleLength
      (quantmod::Hi(TS) - BodyHi) <= CL * maxuppershadowCL & # upper shadow missing or very short
      abs(quantmod::Cl(TS) - quantmod::Op(TS)) > CL * minbodyCL, # Body length greater than minbodyCL * CandleLength
    TS
  )

  colnames(HangingMan) <- c("HangingMan")
  xts::xtsAttributes(HangingMan) <- list(bars = 1)
  return(HangingMan)
}
