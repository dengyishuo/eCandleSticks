#' N Consecutive Higher Close Prices
#'
#' Identifies sequences of N consecutive higher close prices in a time series.
#'
#' @param x xts Time Series containing Close prices
#' @param N Number of consecutive candles to test for higher close series
#'
#' @details
#' Only the close price is evaluated. The colors of the candles are ignored.
#'
#' Identifies when the current close price is higher than the previous close price,
#' and this pattern continues for N consecutive periods.
#'
#' @return
#' A xts object containing the column:
#' \itemize{
#' \item {N}HigherClose: TRUE if current close is the \code{N}th higher close in a row
#' }
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("AAPL", adjust = TRUE)
#' addCSPNHigherClose(AAPL, N = 3) # filter for 3 consecutive higher close
#' }
#'
#' @importFrom tibble as_tibble
#' @importFrom zoo index
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#' @family pattern-nbar
#' @family pattern-bull
#' @export
#' @importFrom quantmod Cl
#' @importFrom xts reclass xtsAttributes
#' @importFrom stats lag
addCSPNHigherClose <- function(x, N,
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

  result <- lagged_closes[, 1] > lagged_closes[, 2]
  for (i in 2:N) {
    result <- result & (lagged_closes[, i] > lagged_closes[, i + 1])
  }

  # Format adjustment

  result <- xts::reclass(result, TS)
  colnames(result) <- paste(N, "HigherClose", sep = "")
  xts::xtsAttributes(result) <- list(bars = N)

  # ── output format ────────────────────────────────────────────────────────
  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df

}
