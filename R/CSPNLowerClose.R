#' N Consecutive Lower Close Prices
#'
#' Identifies sequences of N consecutive lower close prices in a time series.
#'
#' @param TS xts Time Series containing Close prices
#' @param N Number of consecutive candles to test for lower close series
#'
#' @details
#' Only the close price is evaluated. The colors of the candles are ignored.
#'
#' Identifies when the current close price is lower than the previous close price,
#' and this pattern continues for N consecutive periods.
#'
#' @return
#' A xts object containing the column:
#' \itemize{
#' \item {N}LowerClose: TRUE if current close is the \code{N}th lower close in a row
#' }
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' CSPNLowerClose(YHOO, N = 4) # filter for 4 consecutive lower close
#' }
#'
#' @export
#' @importFrom quantmod Cl
#' @importFrom xts reclass xtsAttributes
#' @importFrom stats lag
CSPNLowerClose <- function(TS, N) {
  if (!(has.Cl(TS))) {
    stop("Price series must contain Close prices")
  }
  if (N < 1) {
    stop("N must be an integer >= 1")
  }

  #  Explicitly extract Close column and generate lagged series

  close_prices <- quantmod::Cl(TS)
  lagged_closes <- do.call(merge, lapply(0:N, function(k) stats::lag(close_prices, -k)))
  colnames(lagged_closes) <- paste0("Close.L", 0:N)

  #  Compare consecutive N days' closing prices

  result <- lagged_closes[, 1] < lagged_closes[, 2]
  for (i in 2:N) {
    result <- result & (lagged_closes[, i] < lagged_closes[, i + 1])
  }

  # Format adjustment

  result <- xts::reclass(result, TS)
  colnames(result) <- paste(N, "LowerClose", sep = "")
  xts::xtsAttributes(result) <- list(bars = N)
  return(result)
}
