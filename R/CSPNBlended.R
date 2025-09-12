#' Calculate Open/High/Low/Close of N Blended Bars
#'
#' This function calculates OHLC prices of a combined candle representing N bars.
#' It creates a "blended" candle that aggregates price action over multiple periods.
#'
#' @param TS xts Time Series containing OHLC prices
#' @param N Number of bars to combine into one bar. Must be an integer >= 1.
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item {N}.Blended.Open: Opening price of the \code{N}-th elapsed candle
#' \item {N}.Blended.High: Highest high of the past \code{N} candles
#' \item {N}.Blended.Low: Lowest low of the past \code{N} candles
#' \item {N}.Blended.Close: Close price of the current candle
#' }
#'
#' @note
#' This function is used by \code{\link{addPriceInfo}} to add price information
#' of detected candlestick patterns.
#'
#' @author Andreas Voellenklee
#'
#' @seealso
#' \code{\link{addPriceInfo}}
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' CSPNBlended(YHOO, N = 3) # combine 3 candles into one
#' }
#'
#' @export
#' @importFrom quantmod Op Hi Lo Cl Lag is.OHLC
#' @importFrom zoo rollapply
#' @importFrom stats na.omit
CSPNBlended <- function(TS, N) {
  if (!quantmod::is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  if (N < 1 || N != as.integer(N)) {
    stop("N must be an integer >= 1")
  }

  OpLagN <- quantmod::Lag(quantmod::Op(TS), k = N)

  HighRoll <- zoo::rollapply(quantmod::Hi(TS),
    width = N,
    FUN = max, align = "right", fill = NA
  )
  LowRoll <- zoo::rollapply(quantmod::Lo(TS),
    width = N,
    FUN = min, align = "right", fill = NA
  )

  result <- merge(
    Open = OpLagN,
    High = HighRoll,
    Low = LowRoll,
    Close = quantmod::Cl(TS)
  )

  colnames(result) <- paste(N,
    c("Blended.Open", "Blended.High", "Blended.Low", "Blended.Close"),
    sep = "."
  )

  return(stats::na.omit(result))
}
