#' Candle Length and Body Length Calculations
#'
#' These functions calculate both relative and absolute lengths of price candles and their bodies.
#'
#' @param TS An xts time series containing price data
#'
#' @details
#' \describe{
#' \item{\code{CandleLength}}{Calculates both relative and absolute candle length.\cr
#' Relative candle length: \code{(High - Low) / ((High + Low) / 2)}\cr
#' Absolute candle length: \code{High - Low}}
#' \item{\code{CandleBodyLength}}{Calculates both relative and absolute candle body length.\cr
#' Relative candle body length: \code{abs(Open - Close) / ((Open + Close) / 2)}\cr
#' Absolute candle body length: \code{abs(Open - Close)}}
#' }
#'
#' @return
#' An xts object containing the following columns:
#' \itemize{
#' \item \code{relCandleLength} or \code{relCandleBodyLength}: Relative length (as a ratio)
#' \item \code{absCandleLength} or \code{absCandleBodyLength}: Absolute length (in price units)
#' }
#'
#' @author Andreas Voellenklee
#' @seealso \code{\link{addCSPLongCandle}}
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' CandleLength(YHOO)
#' CandleBodyLength(YHOO)
#' }
#'
#' @name CandleLength
NULL

#' @rdname CandleLength
#' @export
CandleLength <- function(TS) {
  if (!quantmod::is.HL(TS)) {
    stop("Price series must contain High and Low.")
  }
  RCL <- 2 * (quantmod::Hi(TS) - quantmod::Lo(TS)) / (quantmod::Hi(TS) + quantmod::Lo(TS))
  ACL <- quantmod::Hi(TS) - quantmod::Lo(TS)
  result <- cbind(RCL, ACL)
  colnames(result) <- c("relCandleLength", "absCandleLength")
  return(result)
}

#' @rdname CandleLength
#' @export
CandleBodyLength <- function(TS) {
  if (!is.OC(TS)) {
    stop("Price series must contain Open and Close.")
  }
  RCBL <- 2 * (abs(quantmod::Op(TS) - quantmod::Cl(TS))) / (quantmod::Op(TS) + quantmod::Cl(TS))
  ACBL <- abs(quantmod::Op(TS) - quantmod::Cl(TS))
  result <- cbind(RCBL, ACBL)
  colnames(result) <- c("relCandleBodyLength", "absCandleBodyLength")
  return(result)
}
