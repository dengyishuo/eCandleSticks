#' Calculate Open/High/Low/Close of N Blended Bars
#'
#' This function calculates OHLC prices of a combined candle representing N bars.
#' It creates a "blended" candle that aggregates price action over multiple periods.
#'
#' @param x xts Time Series containing OHLC prices
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
#' addCSPNBlended(YHOO, N = 3) # combine 3 candles into one
#' }
#'
#' @importFrom tibble as_tibble
#' @importFrom zoo index
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#' @family pattern-nbar
#' @export
#' @importFrom quantmod Op Hi Lo Cl Lag is.OHLC
#' @importFrom zoo rollapply
#' @importFrom stats na.omit
addCSPNBlended <- function(x, N,
                              output = c("xts", "tibble", "data.frame")) {
  # ── accept data.frame / tibble input ─────────────────────────────────────
  if (!xts::is.xts(x)) {
    nms <- tolower(colnames(x))
    date_col  <- colnames(x)[nms %in% c("date", "time", "index")][1]
    open_col  <- colnames(x)[nms == "open"][1]
    high_col  <- colnames(x)[nms == "high"][1]
    low_col   <- colnames(x)[nms == "low"][1]
    close_col <- colnames(x)[nms == "close"][1]
    if (any(is.na(c(date_col, open_col, high_col, low_col, close_col))))
      stop("x must contain open/high/low/close columns or be an xts OHLC object.")
    mat <- as.matrix(x[, c(open_col, high_col, low_col, close_col)])
    colnames(mat) <- c("Open", "High", "Low", "Close")
    x <- xts::xts(mat, order.by = as.Date(x[[date_col]]))
  }
  TS <- x

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
