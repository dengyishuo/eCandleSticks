#' Long Candle Body Pattern
#'
#' Identifies long candle bodies in an OHLC price series based on their relative length
#' compared to the median of preceding candle bodies.
#'
#' @param TS xts Time Series containing Open and Close prices
#' @param n Number of preceding candles to calculate median candle body length. Default is 20.
#' @param threshold Minimum candle body length in relation to the median candle body length
#' of \code{n} preceding candles. Default is 1.
#'
#' @details
#' Classifies candle bodies as long based on their relative length compared to the median
#' of the relative candle body lengths of the preceding \code{n} candles.
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item LongWhiteCandleBody: TRUE if Long White Candle Body detected
#' \item LongBlackCandleBody: TRUE if Long Black Candle Body detected
#' }
#'
#' @author Andreas Voellenklee
#'
#' @seealso
#' \code{\link{CandleBodyLength}}, \code{\link{CSPLongCandle}}
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' CSPLongCandleBody(YHOO)
#' }
#'
#' @export
#' @importFrom quantmod Op Cl
#' @importFrom TTR runMedian
#' @importFrom xts reclass xtsAttributes
CSPLongCandleBody <- function(TS, n = 20, threshold = 1) {
  if (!(has.Op(TS) && has.Cl(TS))) {
    stop("Price series must contain Open and Close.")
  }

  CBL <- CandleBodyLength(TS)
  CBLMedian <- runMedian(CBL[, 1], n = n) # use relative CandleBodyLength

  LongWhiteCandleBody <- xts::reclass(
    CBL[, 1] >= CBLMedian * threshold & quantmod::Cl(TS) >= quantmod::Op(TS),
    TS
  )

  LongBlackCandleBody <- xts::reclass(
    CBL[, 1] >= CBLMedian * threshold & quantmod::Op(TS) > quantmod::Cl(TS),
    TS
  )

  result <- cbind(LongWhiteCandleBody, LongBlackCandleBody)
  colnames(result) <- c("LongWhiteCandleBody", "LongBlackCandleBody")
  xts::xtsAttributes(result) <- list(bars = 1)
  return(result)
}
