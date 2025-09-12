#' Calculate HL Average
#'
#' Calculates the average value of High and Low prices.
#'
#' @param TS An xts time series containing HL prices
#'
#' @details
#' The HL Average is calculated as: (High + Low) / 2
#'
#' @return
#' An xts object containing a single column:
#' \itemize{
#' \item \code{HL_Average}: The average of High and Low prices
#' }
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' head(HL_Average(YHOO))
#' }
#'
#' @export
HL_Average <- function(TS) {
  if (!quantmod::is.HL(TS)) {
    stop("Price series must contain High and Low.")
  }
  Av <- xts::as.xts(apply(cbind(quantmod::Hi(TS), quantmod::Lo(TS)), 1, mean))
  colnames(Av) <- c("HL_Average")
  return(xts::reclass(Av, TS))
}
