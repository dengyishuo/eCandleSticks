#' Inverted Hammer Candlestick Pattern
#'
#' Identifies Inverted Hammer patterns in an OHLC price series.
#' The inverted hammer is a one-day bullish reversal pattern that appears during a downtrend.
#'
#' @param x xts Time Series containing Open, High, Low and Close Prices
#' @param minuppershadowCL Minimum upper shadow to candle length ratio. Default is 2/3.
#' @param maxlowershadowCL Maximum tolerated lower shadow to candle length ratio. Default is 0.1.
#' @param minbodyCL Minimum body to candle length ratio. Default is 0.1.
#'
#' @details
#' Number of candle lines: \bold{1}
#'
#' The inverted hammer pattern has:
#' \itemize{
#' \item A long upper shadow (at least \code{minuppershadowCL} of the candle length)
#' \item A small body at the bottom of the candle (at least \code{minbodyCL} of the candle length)
#' \item No or only a very short lower shadow (at most \code{maxlowershadowCL} of the candle length)
#' \item The color of the body is not important, though a white body is slightly more bullish
#' }
#'
#' This pattern typically appears at the bottom of a downtrend and signals a potential reversal upward.
#'
#' @return
#' A xts object containing the column:
#' \itemize{
#' \item InvertedHammer: TRUE if inverted hammer pattern detected
#' }
#'
#' @references
#' The following site was used to code/document this candlestick pattern:
#' \url{http://stockcharts.com/school/doku.php?id=chart_school:chart_analysis:introduction_to_candlesticks}
#'
#' @note
#' The function filters candles that look like inverted hammers, without considering
#' the current trend direction. If only inverted hammer patterns in a downtrend should be
#' filtered, an external trend detection function must be used. See examples.
#'
#' @seealso
#' \code{\link{addCSPDoji}}
#' \code{\link{addCSPHammer}}
#' \code{\link{TrendDetectionChannel}}
#' \code{\link{TrendDetectionSMA}}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#'
#' # Filter for inverted hammer patterns
#' addCSPInvertedHammer(YHOO)
#'
#' # Filter for inverted hammer patterns that occur in downtrends
#' addCSPInvertedHammer(YHOO) & TrendDetectionChannel(YHOO)[, "DownTrend"]
#' }
#'
#' @importFrom tibble as_tibble
#' @importFrom zoo index
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#' @family pattern-1bar
#' @family pattern-bull
#' @export
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom xts reclass xtsAttributes
addCSPInvertedHammer <- function(x, minuppershadowCL = 2 / 3, maxlowershadowCL = 0.1, minbodyCL = 0.1,
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

  if (!(has.Op(TS) && has.Hi(TS) && has.Lo(TS) && has.Cl(TS))) {
    stop("Price series must contain Open, High, Low and Close.")
  }

  CL <- quantmod::Hi(TS) - quantmod::Lo(TS)
  BodyHi <- pmax(quantmod::Op(TS), quantmod::Cl(TS))
  BodyLo <- pmin(quantmod::Op(TS), quantmod::Cl(TS))

  InvertedHammer <- xts::reclass(
    (quantmod::Hi(TS) - BodyHi) > CL * minuppershadowCL & # upper shadow greater than minuppershadowCL * CandleLength
      (BodyLo - quantmod::Lo(TS)) <= CL * maxlowershadowCL & # lower shadow missing or very short
      abs(quantmod::Cl(TS) - quantmod::Op(TS)) > CL * minbodyCL, # Body length greater than minbodyCL * CandleLength
    TS
  )

  colnames(InvertedHammer) <- c("InvertedHammer")
  xts::xtsAttributes(InvertedHammer) <- list(bars = 1)
  return(InvertedHammer)
}
