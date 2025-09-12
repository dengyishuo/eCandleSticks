#' Check for High/Low Data
#'
#' Function to check for appropriate High/Low column names within a data object.
#'
#' @param x A data object (typically an xts time series)
#'
#' @details
#' The candlestick package uses functions to check for OHLC data from the \pkg{quantmod} package.
#' The function \code{is.HL} is not included in quantmod, so it is provided here.
#'
#' @return
#' \itemize{
#' \item \code{is.HL}: TRUE if x includes columns for High and Low
#' }
#'
#' @seealso \code{\link[quantmod]{has.OHLC}}
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO")
#' is.HL(YHOO)
#' }
#'
#' @export
is.HL <- function(x) {
  if (quantmod::has.Hi(x) & quantmod::has.Lo(x)) {
    TRUE
  } else {
    FALSE
  }
}
