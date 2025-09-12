#' Add OHLC Information for Detected Candlestick Patterns
#'
#' This function returns not only the occurrences of candlestick patterns, but also the OHLC price information
#' of the pattern's formation. The OHLC price information is only set on dates when the pattern is detected,
#' and 0 otherwise.
#'
#' @param TS An xts time series containing OHLC prices
#' @param CSP An xts time series of detected candlestick patterns, based on TS
#'
#' @details
#' The xtsAttribute \code{bars} of \code{CSP} is used to calculate the blended candlestick of the formation.
#'
#' @return
#' An xts object containing the columns:
#' \itemize{
#' \item All columns of CSP
#' \item \code{Formation.Open}: Opening price of the detected candlestick formation
#' \item \code{Formation.High}: Highest high of the detected candlestick formation
#' \item \code{Formation.Low}: Lowest low of the detected candlestick formation
#' \item \code{Formation.Close}: Close price of the detected candlestick formation
#' }
#'
#' @author Andreas Voellenklee
#' @seealso \code{\link{CSPNBlended}}
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#'
#' # Return detected Engulfing Pattern
#' CSPEngulfing(YHOO)
#'
#' # Return detected Engulfing Pattern including formation's OHLC
#' addPriceInfo(YHOO, CSPEngulfing(YHOO))
#' }
#'
#' @export
addPriceInfo <- function(TS, CSP) {
  #  Validate the 'bars' attribute

  attr <- xts::xtsAttributes(CSP)
  if (!is.numeric(attr$bars) | attr$bars < 1) {
    stop("Invalid xts Attribute 'bars'")
  }

  #  Create a multiplier based on pattern occurrences

  multiplicator <- xts::reclass(xts::as.xts(apply(CSP, 1, max)), TS)

  #  Calculate blended candlestick for the pattern formation

  BLEND <- CSPNBlended(TS, N = attr$bars)

  #  Apply the multiplier to get formation OHLC prices

  PINFO <- cbind(
    BLEND[, 1] * multiplicator,
    BLEND[, 2] * multiplicator,
    BLEND[, 3] * multiplicator,
    BLEND[, 4] * multiplicator
  )

  #  Set appropriate column names

  colnames(PINFO) <- c("Formation.Open", "Formation.High", "Formation.Low", "Formation.Close")

  #  Combine pattern information with formation prices

  result <- cbind(CSP, PINFO)

  return(result)
}
