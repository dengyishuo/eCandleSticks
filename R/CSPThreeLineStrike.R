#' Three Line Strike Candlestick Pattern
#'
#' Identifies bullish and bearish Three Line Strike patterns in an Open/Close price series.
#' This is a four-candle continuation pattern that signals potential trend continuation.
#'
#' @param TS xts Time Series containing Open and Close prices
#' @param n Number of preceding candles to calculate median candle length. Default is 25.
#' @param minbodysizeMedian Minimum candle length in relation to the median candle length
#' of \code{n} preceding candles. Default is 0.5.
#'
#' @details
#' Number of candle lines: \bold{4}
#'
#' \strong{Bullish Three Line Strike:}
#' \itemize{
#' \item The first three candles must all be white with not too small candle bodies
#' \item The close of the second candle is above the close of the first
#' \item The close of the third candle is above the close of the second
#' \item The open of the last candle is at or above the close of the third candle
#' \item The price then moves down heavily and closes at or below the open of the first candle
#' \item This pattern signals a potential continuation of a downtrend
#' }
#'
#' \strong{Bearish Three Line Strike:}
#' \itemize{
#' \item The opposite of the bullish pattern
#' \item Three black candles followed by a long white candle
#' \item This pattern signals a potential continuation of an uptrend
#' }
#'
#' This formation is very rare.
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item Bull.ThreeLineStrike: TRUE if bullish Three Line Strike pattern detected
#' \item Bear.ThreeLineStrike: TRUE if bearish Three Line Strike pattern detected
#' }
#'
#' @references
#' The following sites were used to code/document this indicator:
#' \itemize{
#' \item \url{https://hitandruncandlesticks.com/bullish-three-line-strike/}
#' \item \url{https://hitandruncandlesticks.com/bearish-three-line-strike/}
#' \item \url{http://thepatternsite.com/ThreeLineStrikeBull.html}
#' }
#'
#' @note
#' The function filters patterns that look like three line strikes, without considering
#' the current trend direction. If only patterns in specific trends should be filtered,
#' an external trend detection function must be used. See examples.
#'
#' @seealso
#' \code{\link{CSPNLongWhiteCandleBodies}}
#' \code{\link{CSPNLongBlackCandleBodies}}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' Sys.setenv(TZ = "UTC")
#'
#' getSymbols("COG")
#' TLS <- CSPThreeLineStrike(COG)
#'
#' # How often does that occur?
#' colSums(TLS, na.rm = TRUE)
#'
#' # When did that occur?
#' TLS[TLS[, 1] > 0 | TLS[, 2] > 0, ]
#'
#' # Filter for bearish three line strikes that occur in downtrends
#' TLS1 <- CSPThreeLineStrike(COG)[, "Bear.ThreeLineStrike"] &
#'   TrendDetectionChannel(lag(COG, k = 4))[, "DownTrend"]
#' TLS1[TLS1[, 1] > 0, ]
#'
#' # Show in a chart
#' chartSeries(COG["2014-09/2014-10"])
#' }
#'
#' @export
#' @importFrom quantmod Op Cl
#' @importFrom xts reclass xtsAttributes
#' @importFrom stats lag
CSPThreeLineStrike <- function(TS, n = 25, minbodysizeMedian = 0.5) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }

  # 手动生成滞后序列并明确命名列名
  lagged_oc <- do.call(merge, lapply(0:3, function(k) {
    merge(lag(quantmod::Op(TS), -k), lag(quantmod::Cl(TS), -k))
  }))
  colnames(lagged_oc) <- c(
    "Op.L0", "Cl.L0", "Op.L1", "Cl.L1",
    "Op.L2", "Cl.L2", "Op.L3", "Cl.L3"
  )

  # 提取需要的列（使用列名代替Cl()/Op()自动匹配）
  Cl_LAGTS <- lagged_oc[, c("Cl.L0", "Cl.L1", "Cl.L2", "Cl.L3")]
  Op_LAGTS <- lagged_oc[, c("Op.L0", "Op.L1", "Op.L2", "Op.L3")]

  # 获取三连阳/阴信号（假设这些函数返回单列结果）
  THREELWCB <- CSPNLongWhiteCandleBodies(TS, N = 3, n = n, threshold = minbodysizeMedian)
  LAGTHREELWCB <- stats::lag(THREELWCB, 1) # 信号滞后1期
  THREELBCB <- CSPNLongBlackCandleBodies(TS, N = 3, n = n, threshold = minbodysizeMedian)
  LAGTHREELBCB <- stats::lag(THREELBCB, 1)

  # 牛市三线打击形态判断（使用明确的列名引用）
  BullTLS <- reclass(
    LAGTHREELWCB[, 1] & # 前三根为长白蜡烛（滞后1期）
      Cl_LAGTS$Cl.L2 > Cl_LAGTS$Cl.L3 & # 第2根收盘 > 第1根收盘
      Cl_LAGTS$Cl.L1 > Cl_LAGTS$Cl.L2 & # 第3根收盘 > 第2根收盘
      Op_LAGTS$Op.L0 >= Cl_LAGTS$Cl.L1 & # 第4根开盘 >= 第3根收盘
      Cl_LAGTS$Cl.L0 <= Op_LAGTS$Op.L3, # 第4根收盘 <= 第1根开盘
    TS
  )

  # 熊市三线打击形态判断
  BearTLS <- reclass(
    LAGTHREELBCB[, 1] & # 前三根为长黑蜡烛（滞后1期）
      Cl_LAGTS$Cl.L2 < Cl_LAGTS$Cl.L3 & # 第2根收盘 < 第1根收盘
      Cl_LAGTS$Cl.L1 < Cl_LAGTS$Cl.L2 & # 第3根收盘 < 第2根收盘
      Op_LAGTS$Op.L0 <= Cl_LAGTS$Cl.L1 & # 第4根开盘 <= 第3根收盘
      Cl_LAGTS$Cl.L0 >= Op_LAGTS$Op.L3, # 第4根收盘 >= 第1根开盘
    TS
  )

  result <- cbind(BullTLS, BearTLS)
  colnames(result) <- c("Bull.ThreeLineStrike", "Bear.ThreeLineStrike")
  xts::xtsAttributes(result) <- list(bars = 4)
  return(result)
}
