#' Check for Open/Close Data
#'
#' Functions to check for appropriate Open/Close and High/Low column names within a data object.
#'
#' @param x A data object (typically an xts time series)
#'
#' @details
#' The candlestick package uses functions to check for OHLC data from the \pkg{quantmod} package.
#' The functions \code{is.OC} and \code{is.HL} are not included in quantmod, so they are provided here.
#'
#' @return
#' \itemize{
#' \item \code{is.OC}: TRUE if x includes columns for Open and Close
#' \item \code{is.HL}: TRUE if x includes columns for High and Low
#' }
#'
#' @seealso \code{\link[quantmod]{has.OHLC}}
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO")
#' is.OC(YHOO)
#' is.HL(YHOO)
#' }
#'
#' @name is.OC
NULL

#' @rdname is.OC
#' @export
is.OC <- function(x) {
  if (quantmod::has.Op(x) & quantmod::has.Cl(x)) {
    TRUE
  } else {
    FALSE
  }
}
