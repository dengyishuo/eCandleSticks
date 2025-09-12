#' Calculate HLC Average
#'
#' Calculates the average value of High, Low, and Close prices.
#'
#' @param TS An xts time series containing HLC prices
#'
#' @details
#' The HLC Average is calculated as: (High + Low + Close) / 3
#'
#' @return
#' An xts object containing a single column:
#' \itemize{
#' \item \code{HLC_Average}: The average of High, Low, and Close prices
#' }
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' head(HLC_Average(YHOO))
#' }
#'
#' @export
HLC_Average <- function(TS) {
  if (!quantmod::is.HLC(TS)) {
    stop("Price series must contain High, Low and Close.")
  }
  Av <- xts::as.xts(apply(cbind(quantmod::Hi(TS), quantmod::Lo(TS), quantmod::Cl(TS)), 1, mean))
  colnames(Av) <- c("HLC_Average")
  return(xts::reclass(Av, TS))
}
