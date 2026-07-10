#' Doji Candlestick Pattern
#'
#' Identify Doji Patterns in an OHLC price series
#'
#' @param x An xts time series containing Open and Close prices
#' @param maxbodyCL Maximum body-to-length ratio, default = 0.1
#' @param maxshadowCL Maximum tolerated upper (lower) shadow ratio to identify a dragonfly (gravestone) doji, default = 0.1
#'
#' @details
#' Number of candle lines: \strong{1}
#'
#' A Doji is formed when the open and close prices are the same or very close. By default settings, a Doji is identified when the body of the candle is smaller than or equal to 1/10 of its full length.
#'
#' The bullish Dragonfly Doji pattern has a long lower shadow but almost no upper shadow. Conversely, the bearish Gravestone Doji has a long upper shadow but almost no lower shadow. Dragonfly Doji and Gravestone Doji can be reversal signals during downtrends/uptrends.
#'
#' @return An xts object containing the following columns:
#' \itemize{
#' \item \code{Doji}: TRUE if a Doji pattern is detected
#' \item \code{DragonflyDoji}: TRUE if a Dragonfly Doji pattern is detected
#' \item \code{GravestoneDoji}: TRUE if a Gravestone Doji pattern is detected
#' }
#'
#' @author Andreas Voellenklee
#' @references
#' Reference sites used for coding/documenting this candlestick pattern:
#' \itemize{
#' \item \url{http://www.onlinetradingconcepts.com/TechnicalAnalysis/Candlesticks/Doji.html}
#' \item \url{http://www.candlesticker.com/Bullish.asp}
#' \item \url{http://www.candlesticker.com/Bearish.asp}
#' }
#'
#' @note
#' This function only filters candlesticks that resemble Doji patterns, without considering the current trend direction. To filter for Doji patterns in an uptrend, an external trend detection function must be used. See examples.
#'
#' @examples
#' \dontrun{
#' # Obtain example data
#' getSymbols("YHOO", adjust = TRUE)
#'
#' # Identify Doji patterns
#' addCSPDoji(YHOO)
#'
#' # Filter for Doji patterns where the open price equals the close price
#' addCSPDoji(YHOO, maxbodyCL = 0)
#'
#' # Filter for Gravestone Doji patterns in an uptrend
#' addCSPDoji(YHOO)[, "GravestoneDoji"] & TrendDetectionChannel(YHOO)[, "UpTrend"]
#' }
#'
#' @importFrom tibble as_tibble
#' @importFrom zoo index
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#' @family pattern-1bar
#' @family pattern-bull
#' @family pattern-bear
#' @export
addCSPDoji <- function(x, maxbodyCL = .1, maxshadowCL = .1,
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

  #  Validate input data contains OHLC data

  if (!quantmod::is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }

  #  Calculate candlestick body length (absolute difference between Close and Open prices)

  BL <- abs(quantmod::Cl(TS) - quantmod::Op(TS))

  #  Calculate total candlestick length (High - Low)

  CL <- quantmod::Hi(TS) - quantmod::Lo(TS)

  # Determine the high point of the candlestick body (maximum of Open/Close)

  BodyHi <- pmax(quantmod::Op(TS), quantmod::Cl(TS))

  #  Determine the low point of the candlestick body (minimum of Open/Close)

  BodyLo <- pmin(quantmod::Op(TS), quantmod::Cl(TS))

  #  Identify standard Doji

  Doji <- xts::reclass(BL < CL * maxbodyCL, TS)

  #  Identify Dragonfly Doji (standard Doji + minimal upper shadow)

  DFDoji <- xts::reclass(Doji & (quantmod::Hi(TS) - BodyHi <= CL * maxshadowCL), TS)

  #  Identify Gravestone Doji (standard Doji + minimal lower shadow)

  GSDoji <- xts::reclass(Doji & (BodyLo - quantmod::Lo(TS) <= CL * maxshadowCL), TS)

  #  Combine results and set column names

  result <- cbind(Doji, DFDoji, GSDoji)
  colnames(result) <- c("Doji", "DragonflyDoji", "GravestoneDoji")

  #  Add attribute to mark that this pattern requires 1 candlestick

  xts::xtsAttributes(result) <- list(bars = 1)

  # ── output format ────────────────────────────────────────────────────────
  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df

}
