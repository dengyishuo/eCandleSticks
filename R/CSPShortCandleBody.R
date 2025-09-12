#' Short Candle Body Pattern
#'
#' Identifies short candle bodies in an OHLC price series based on their relative length
#' compared to the median of preceding candle bodies.
#'
#' @param TS xts Time Series containing Open and Close prices
#' @param n Number of preceding candles to calculate median candle body length. Default is 20.
#' @param threshold Maximum candle body length in relation to the median candle body length
#' of \code{n} preceding candles. Default is 1.
#'
#' @details
#' Classifies candle bodies as short based on their relative length compared to the median
#' of the relative candle body lengths of the preceding \code{n} candles.
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item ShortWhiteCandleBody: TRUE if Short White Candle Body detected
#' \item ShortBlackCandleBody: TRUE if Short Black Candle Body detected
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
#' CSPShortCandleBody(YHOO)
#' }
#'
#' @export
#' @importFrom quantmod Op Cl
#' @importFrom TTR runMedian
#' @importFrom xts reclass xtsAttributes
CSPShortCandleBody <- function(TS, n = 20, threshold = 1) {
  if (!(has.Op(TS) && has.Cl(TS))) {
    stop("Price series must contain Open and Close.")
  }

  CBL <- CandleBodyLength(TS)
  CBLMedian <- runMedian(CBL[, 1], n = n) # use relative CandleBodyLength

  ShortWhiteCandleBody <- xts::reclass(
    CBL[, 1] < CBLMedian * threshold & quantmod::Cl(TS) >= quantmod::Op(TS),
    TS
  )

  ShortBlackCandleBody <- xts::reclass(
    CBL[, 1] < CBLMedian * threshold & quantmod::Op(TS) > quantmod::Cl(TS),
    TS
  )

  result <- cbind(ShortWhiteCandleBody, ShortBlackCandleBody)
  colnames(result) <- c("ShortWhiteCandleBody", "ShortBlackCandleBody")
  xts::xtsAttributes(result) <- list(bars = 1)
  return(result)
}
