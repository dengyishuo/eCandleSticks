#' Identify Outside Day Patterns
#'
#' Detects outside day patterns in an OHLC price series. An outside day occurs when
#' the current bar's high is higher than the previous day's high and the low is lower
#' than the previous day's low.
#'
#' @param TS xts Time Series containing OHLC prices
#'
#' @details
#' Number of candle lines: \bold{2}
#'
#' \strong{Outside Day:}
#' In an outside day, the current bar's high is higher than the previous day's
#' high and the low is lower than the previous day's low. This pattern indicates
#' increased volatility and can signal potential trend reversals or breakouts.
#'
#' @return A xts object containing the column:
#' \itemize{
#' \item OutsideDay: TRUE if current candle is an outside day
#' }
#'
#' @references
#' \url{http://www.investopedia.com/terms/o/outside-days.asp#axzz1jWelu1cm}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("AAPL", adjust = TRUE)
#' CSPOutsideDay(AAPL)
#' }
#'
#' @export
#' @importFrom quantmod Hi Lo has.Op has.Hi has.Lo has.Cl
#' @importFrom xts reclass xtsAttributes
CSPOutsideDay <- function(TS) {
  if (!(has.Op(TS) && has.Hi(TS) && has.Lo(TS) && has.Cl(TS))) {
    stop("Price series must contain Open, High, Low and Close.")
  }

  LAGTS <- LagOHLC(TS, k = 1)
  result <- xts::reclass(
    quantmod::Hi(TS) > quantmod::Hi(LAGTS) &
      quantmod::Lo(TS) < quantmod::Lo(LAGTS),
    TS
  )

  colnames(result) <- "OutsideDay"
  xts::xtsAttributes(result) <- list(bars = 2)
  return(result)
}
