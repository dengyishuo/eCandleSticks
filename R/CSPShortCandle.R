#' Short Candlestick Pattern
#'
#' Identifies short candlesticks in an OHLC price series based on their relative length
#' compared to the median of preceding candles.
#'
#' @param TS xts Time Series containing OHLC prices
#' @param n Number of preceding candles to calculate median candle length. Default is 20.
#' @param threshold Maximum candle length in relation to the median candle length
#' of \code{n} preceding candles. Default is 1.
#'
#' @details
#' Classifies candles as short based on their relative length compared to the median
#' of the relative candle lengths of the preceding \code{n} candles.
#'
#' Relative candle length is calculated as \code{(Hi-Lo)/((Hi+Lo)/2)}.
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item ShortWhiteCandle: TRUE if Short White Candle detected
#' \item ShortBlackCandle: TRUE if Short Black Candle detected
#' }
#'
#' @author Andreas Voellenklee
#'
#' @seealso
#' \code{\link{CandleLength}}, \code{\link{CSPLongCandle}}
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' CSPShortCandle(YHOO, threshold = 0.5) # filter for very small candles
#' }
#'
#' @export
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom TTR runMedian
#' @importFrom xts reclass xtsAttributes
CSPShortCandle <- function(TS, n = 20, threshold = 1) {
  if (!(has.Op(TS) && has.Hi(TS) && has.Lo(TS) && has.Cl(TS))) {
    stop("Price series must contain Open, High, Low and Close.")
  }

  CL <- CandleLength(TS)
  CLMedian <- runMedian(CL[, 1], n = n) # use relative CandleLength

  ShortWhiteCandle <- xts::reclass(
    CL[, 1] < CLMedian * threshold & quantmod::Cl(TS) >= quantmod::Op(TS),
    TS
  )

  ShortBlackCandle <- xts::reclass(
    CL[, 1] < CLMedian * threshold & quantmod::Op(TS) > quantmod::Cl(TS),
    TS
  )

  result <- cbind(ShortWhiteCandle, ShortBlackCandle)
  colnames(result) <- c("ShortWhiteCandle", "ShortBlackCandle")
  xts::xtsAttributes(result) <- list(bars = 1)
  return(result)
}
