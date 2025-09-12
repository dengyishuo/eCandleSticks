#' Inverted Hammer Candlestick Pattern
#'
#' Identifies Inverted Hammer patterns in an OHLC price series.
#' The inverted hammer is a one-day bullish reversal pattern that appears during a downtrend.
#'
#' @param TS xts Time Series containing Open, High, Low and Close Prices
#' @param minuppershadowCL Minimum upper shadow to candle length ratio. Default is 2/3.
#' @param maxlowershadowCL Maximum tolerated lower shadow to candle length ratio. Default is 0.1.
#' @param minbodyCL Minimum body to candle length ratio. Default is 0.1.
#'
#' @details
#' Number of candle lines: \bold{1}
#'
#' The inverted hammer pattern has:
#' \itemize{
#' \item A long upper shadow (at least \code{minuppershadowCL} of the candle length)
#' \item A small body at the bottom of the candle (at least \code{minbodyCL} of the candle length)
#' \item No or only a very short lower shadow (at most \code{maxlowershadowCL} of the candle length)
#' \item The color of the body is not important, though a white body is slightly more bullish
#' }
#'
#' This pattern typically appears at the bottom of a downtrend and signals a potential reversal upward.
#'
#' @return
#' A xts object containing the column:
#' \itemize{
#' \item InvertedHammer: TRUE if inverted hammer pattern detected
#' }
#'
#' @references
#' The following site was used to code/document this candlestick pattern:
#' \url{http://stockcharts.com/school/doku.php?id=chart_school:chart_analysis:introduction_to_candlesticks}
#'
#' @note
#' The function filters candles that look like inverted hammers, without considering
#' the current trend direction. If only inverted hammer patterns in a downtrend should be
#' filtered, an external trend detection function must be used. See examples.
#'
#' @seealso
#' \code{\link{CSPDoji}}
#' \code{\link{CSPHammer}}
#' \code{\link{TrendDetectionChannel}}
#' \code{\link{TrendDetectionSMA}}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#'
#' # Filter for inverted hammer patterns
#' CSPInvertedHammer(YHOO)
#'
#' # Filter for inverted hammer patterns that occur in downtrends
#' CSPInvertedHammer(YHOO) & TrendDetectionChannel(YHOO)[, "DownTrend"]
#' }
#'
#' @export
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom xts reclass xtsAttributes
CSPInvertedHammer <- function(TS, minuppershadowCL = 2 / 3, maxlowershadowCL = 0.1, minbodyCL = 0.1) {
  if (!(has.Op(TS) && has.Hi(TS) && has.Lo(TS) && has.Cl(TS))) {
    stop("Price series must contain Open, High, Low and Close.")
  }

  CL <- quantmod::Hi(TS) - quantmod::Lo(TS)
  BodyHi <- pmax(quantmod::Op(TS), quantmod::Cl(TS))
  BodyLo <- pmin(quantmod::Op(TS), quantmod::Cl(TS))

  InvertedHammer <- xts::reclass(
    (quantmod::Hi(TS) - BodyHi) > CL * minuppershadowCL & # upper shadow greater than minuppershadowCL * CandleLength
      (BodyLo - quantmod::Lo(TS)) <= CL * maxlowershadowCL & # lower shadow missing or very short
      abs(quantmod::Cl(TS) - quantmod::Op(TS)) > CL * minbodyCL, # Body length greater than minbodyCL * CandleLength
    TS
  )

  colnames(InvertedHammer) <- c("InvertedHammer")
  xts::xtsAttributes(InvertedHammer) <- list(bars = 1)
  return(InvertedHammer)
}
