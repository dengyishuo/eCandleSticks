#' Upside/Downside Tasuki Gap Candlestick Pattern
#'
#' Identifies Upside and Downside Tasuki Gap patterns in an OHLC price series.
#' These are three-candle continuation patterns that occur within existing trends.
#'
#' @param TS xts Time Series containing OHLC prices
#'
#' @details
#' Number of candle lines: \bold{3}
#'
#' \strong{Upside Tasuki Gap:}
#' \itemize{
#' \item The market is in an uptrend
#' \item First candle: white candle
#' \item Second candle: white candle that gaps above the high of the first candle
#' \item Third candle: black candle that opens within the body of the second candle and closes within the gap between the first and second candle
#' \item This pattern signals a potential continuation of the uptrend
#' }
#'
#' \strong{Downside Tasuki Gap:}
#' \itemize{
#' \item The market is in a downtrend
#' \item First candle: black candle
#' \item Second candle: black candle that gaps below the low of the first candle
#' \item Third candle: white candle that opens within the body of the second candle and closes within the gap between the first and second candle
#' \item This pattern signals a potential continuation of the downtrend
#' }
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item UpsideTasukiGap: TRUE if Upside Tasuki Gap pattern detected
#' \item DownsideTasukiGap: TRUE if Downside Tasuki Gap pattern detected
#' }
#'
#' @references
#' The following sites were used to code/document this indicator:
#' \itemize{
#' \item \url{http://www.investopedia.com/terms/u/upside-tasuki-gap.asp#axzz1jtuDsM6i}
#' \item \url{http://www.investopedia.com/terms/d/downside-tasuki-gap.asp#axzz1jtuDsM6i}
#' \item \url{http://thepatternsite.com/UpsideTasukiGap.html}
#' \item \url{http://thepatternsite.com/DownsideTasukiGap.html}
#' }
#'
#' @note
#' The function filters patterns that look like Tasuki gap patterns, without considering
#' the current trend direction. If only patterns in specific trends should be filtered,
#' an external trend detection function must be used. See examples.
#'
#' @seealso
#' \code{\link{CSPGap}}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("SSRI", adjust = TRUE)
#' CSPTasukiGap(SSRI)
#'
#' # Filter upside Tasuki gaps in uptrends
#' CSPTasukiGap(SSRI)[, "UpsideTasukiGap"] &
#'   TrendDetectionChannel(lag(SSRI, k = 3))[, "UpTrend"]
#' }
#'
#' @export
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom xts reclass xtsAttributes
CSPTasukiGap <- function(TS) {
  if (!(has.Op(TS) && has.Hi(TS) && has.Lo(TS) && has.Cl(TS))) {
    stop("Price series must contain Open, High, Low and Close.")
  }

  LAG2TS <- LagOHLC(TS, k = 2)
  LAG1TS <- LagOHLC(TS, k = 1)
  GAP1 <- CSPGap(LAG1TS, ignoreShadows = FALSE)

  UTG <- xts::reclass(
    quantmod::Op(LAG2TS) < quantmod::Cl(LAG2TS) & # 1st candle: white
      GAP1[, 1] & # Up Gap btwn 1st and 2nd candle
      quantmod::Op(LAG1TS) < quantmod::Cl(LAG1TS) & # 2nd candle: white
      quantmod::Op(TS) < quantmod::Cl(LAG1TS) & quantmod::Op(TS) > quantmod::Op(LAG1TS) & # 3rd candle opens within 2nd candle's body
      quantmod::Cl(TS) < quantmod::Lo(LAG1TS) & quantmod::Cl(TS) > quantmod::Hi(LAG2TS), # 3rd candle closes within gap of 1st and 2nd candle
    TS
  )

  DTG <- xts::reclass(
    quantmod::Op(LAG2TS) > quantmod::Cl(LAG2TS) & # 1st candle: black
      GAP1[, 2] & # Down Gap btwn 1st and 2nd candle
      quantmod::Op(LAG1TS) > quantmod::Cl(LAG1TS) & # 2nd candle: black
      quantmod::Op(TS) > quantmod::Cl(LAG1TS) & quantmod::Op(TS) < quantmod::Op(LAG1TS) & # 3rd candle opens within 2nd candle's body
      quantmod::Cl(TS) > quantmod::Hi(LAG1TS) & quantmod::Cl(TS) < quantmod::Lo(LAG2TS), # 3rd candle closes within gap of 1st and 2nd candle
    TS
  )

  result <- cbind(UTG, DTG)
  colnames(result) <- c("UpsideTasukiGap", "DownsideTasukiGap")
  xts::xtsAttributes(result) <- list(bars = 3)
  return(result)
}
