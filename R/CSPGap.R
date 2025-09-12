#' Gap Candlestick Pattern
#'
#' Identifies price gaps between two candles in an OHLC price series.
#' Also known as Rising Window (gap up) and Falling Window (gap down).
#'
#' @param TS xts Time Series containing OHLC prices
#' @param ignoreShadows If TRUE, only Open and Close Price are evaluated.
#' High and Low are ignored. Default is FALSE.
#'
#' @details
#' Number of candle lines: \bold{2}
#'
#' When ignoreShadows = FALSE (default), a gap up occurs when the current candle's
#' low is higher than the previous candle's high. A gap down occurs when the
#' current candle's high is lower than the previous candle's low.
#'
#' When ignoreShadows = TRUE, a gap up occurs when the current candle's body
#' (range between open and close) is completely above the previous candle's body.
#' A gap down occurs when the current candle's body is completely below the
#' previous candle's body.
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item GapUp: TRUE if gap up detected
#' \item GapDown: TRUE if gap down detected
#' }
#'
#' @note
#' The up/down gaps are also called \emph{Rising Window}/\emph{Falling Window}
#' in candlestick analysis terminology.
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' CSPGap(YHOO) # examine whole candle length
#' CSPGap(YHOO, ignoreShadows = TRUE) # examine only candle bodies
#' }
#'
#' @export
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom xts reclass xtsAttributes
CSPGap <- function(TS, ignoreShadows = FALSE) {
  if (ignoreShadows == TRUE) {
    if (!(has.Op(TS) && has.Cl(TS))) {
      stop("Price series must contain Open and Close.")
    }
    LAGTS <- LagOC(TS, k = 1)
    TSOC <- cbind(quantmod::Op(TS), quantmod::Cl(TS))
    LAGTSOC <- cbind(quantmod::Op(LAGTS), quantmod::Cl(LAGTS))
    UPGAP <- xts::reclass(pmax(quantmod::Op(LAGTSOC), quantmod::Cl(LAGTSOC)) <
      pmin(quantmod::Op(TSOC), quantmod::Cl(TSOC)), TS)
    DOWNGAP <- xts::reclass(pmin(quantmod::Op(LAGTSOC), quantmod::Cl(LAGTSOC)) >
      pmax(quantmod::Op(TSOC), quantmod::Cl(TSOC)), TS)
  } else if (ignoreShadows == FALSE) {
    if (!(has.Hi(TS) && has.Lo(TS) && has.Op(TS) && has.Cl(TS))) {
      stop("Price series must contain Open, High, Low and Close.")
    }
    LAGTS <- LagOHLC(TS, k = 1)
    UPGAP <- xts::reclass(quantmod::Lo(TS) > quantmod::Hi(LAGTS), TS)
    DOWNGAP <- xts::reclass(quantmod::Hi(TS) < quantmod::Lo(LAGTS), TS)
  }
  result <- cbind(UPGAP, DOWNGAP)
  colnames(result) <- c("GapUp", "GapDown")
  xts::xtsAttributes(result) <- list(bars = 2)
  return(result)
}
