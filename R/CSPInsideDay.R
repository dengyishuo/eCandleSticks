#' Identify Inside Day Patterns
#'
#' Detects inside day patterns in an OHLC price series. An inside day occurs when
#' the current bar remains completely within the previous day's high and low range.
#'
#' @param TS xts Time Series containing OHLC prices
#'
#' @details
#' Number of candle lines: \bold{2}
#'
#' \strong{Inside Day:}
#' In an inside day, the current bar remains completely within the previous day's
#' bar high and low range. This pattern often indicates consolidation and can
#' precede significant price moves.
#'
#' @return A xts object containing the column:
#' \itemize{
#' \item InsideDay: TRUE if current candle is an inside day
#' }
#'
#' @references
#' \url{http://www.investopedia.com/terms/i/inside_day.asp#axzz1jWelu1cm}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' CSPInsideDay(YHOO)
#' }
#'
#' @export
#' @importFrom quantmod Hi Lo has.Op has.Hi has.Lo has.Cl
#' @importFrom xts reclass xtsAttributes
CSPInsideDay <- function(TS) {
  if (!(has.Op(TS) && has.Hi(TS) && has.Lo(TS) && has.Cl(TS))) {
    stop("Price series must contain Open, High, Low and Close.")
  }

  LAGTS <- LagOHLC(TS, k = 1)
  result <- xts::reclass(
    quantmod::Hi(TS) <= quantmod::Hi(LAGTS) &
      quantmod::Lo(TS) >= quantmod::Lo(LAGTS),
    TS
  )

  colnames(result) <- "InsideDay"
  xts::xtsAttributes(result) <- list(bars = 2)
  return(result)
}
