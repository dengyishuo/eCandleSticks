#' Donchian Channel
#'
#' Calculates Donchian Channels with a 1-period lag correction, as used in the candlesticks package.
#' Donchian Channels were created by Richard Donchian and were used to generate buy and
#' sell signals for the Turtle Trading system.
#'
#' @param HL Object that is coercible to xts or matrix and contains High and Low prices or Close prices.
#' @param n Number of periods to include in price band calculation.
#'
#' @details
#' Donchian Channels consist of two (sometimes three) lines:
#' \itemize{
#' \item The top line is the highest high of the past \code{n} periods.
#' \item The bottom line is the lowest low of the past \code{n} periods.
#' \item The middle line is the average of the top and bottom lines.
#' }
#'
#' @return
#' An object of the same class as \code{HL} or a matrix containing the columns:
#' \itemize{
#' \item high: The highest high series (lagged by 1 period)
#' \item mid: The average of \code{high} and \code{low} (lagged by 1 period)
#' \item low: The lowest low series (lagged by 1 period)
#' }
#'
#' @note
#' The DonchianChannel function in the \code{TTR} package includes the current price bar
#' for the channel calculation. In contrast, this DonchianChannel2 function calculates
#' the price bands on the \emph{previous} \code{n} price bars and includes a 1-period lag.
#'
#' @author G See
#'
#' @references
#' The following site(s) were used to code/document this indicator:
#' \url{https://www.investopedia.com/terms/d/donchianchannels.asp}
#'
#' @seealso
#' See \code{\link[TTR]{DonchianChannel}} in the TTR package.
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' dc <- DonchianChannel2(YHOO)
#' }
#'
#' @keywords ts
#' @export
#' @importFrom quantmod Hi Lo has.Hi has.Lo
#' @importFrom TTR runMax runMin
#' @importFrom stats lag
DonchianChannel2 <- function(HL, n = 10) {
  if (NCOL(HL) > 1 && (!has.Lo(HL) || !has.Hi(HL))) {
    stop("Price series must either contain High and Low, or be univariate.")
  }

  if (NCOL(HL) > 1) {
    hi <- quantmod::Hi(HL)[, 1]
    lo <- quantmod::Lo(HL)[, 1]
  } else {
    hi <- lo <- HL
  }

  high <- TTR::runMax(hi, n)
  low <- TTR::runMin(lo, n)
  mid <- (high + low) / 2

  result <- stats::lag(cbind(high, mid, low))
  colnames(result) <- c("high", "mid", "low")

  return(result)
}
