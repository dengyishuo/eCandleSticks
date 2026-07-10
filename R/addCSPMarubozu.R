#' Marubozu Candlestick Pattern
#'
#' Identifies Marubozu candlestick patterns in an OHLC price series.
#' Marubozu candles have long bodies with very small or no shadows.
#'
#' @param x xts Time Series containing OHLC prices
#' @param n Number of preceding candles to calculate Average True Range. Default is 20.
#' @param ATRFactor Minimum size of candle body compared to the ATR. Default is 1.
#' @param maxuppershadowCL Maximum tolerated upper shadow to candle length ratio. Default is 0.1.
#' @param maxlowershadowCL Maximum tolerated lower shadow to candle length ratio. Default is 0.1.
#'
#' @details
#' Number of candle lines: \bold{1}
#'
#' \strong{White Marubozu:}
#' A long white candle that has no shadows or only small shadows on either end.
#'
#' \strong{Black Marubozu:}
#' A long black candle that has no shadows or only small shadows on either end.
#'
#' In default settings, the candle \emph{body} length must be greater than the average
#' true range of last \code{n} periods. The threshold can be varied by \code{ATRFactor}.
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item WhiteMarubozu: TRUE if pattern detected
#' \item BlackMarubozu: TRUE if pattern detected
#' }
#'
#' @references
#' The following sites were used to code/document this indicator:
#' \itemize{
#' \item \url{http://www.candlesticker.com/Bullish.asp}
#' \item \url{http://www.candlesticker.com/Bearish.asp}
#' }
#'
#' @seealso
#' \code{\link{CandleBodyLength}}
#' \code{\link[TTR]{ATR}}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#'
#' addCSPMarubozu(YHOO)
#'
#' # Include not-so-long-marubozus
#' addCSPMarubozu(YHOO, ATRFactor = 0.8)
#'
#' # Filter for white closing marubozus (Cl(TS) = Hi(TS))
#' addCSPMarubozu(YHOO, maxuppershadowCL = 0)[, "WhiteMarubozu"]
#' }
#'
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#' @family pattern-1bar
#' @family pattern-bull
#' @family pattern-bear
#' @importFrom xts reclass xtsAttributes `xtsAttributes<-`
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom TTR ATR
#' @importFrom tibble as_tibble
#' @importFrom zoo index
#' @export
addCSPMarubozu <- function(x, n = 20, ATRFactor = 1, maxuppershadowCL = 0.1,
                        maxlowershadowCL = 0.1,
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

  if (!is.OHLC(TS)) {
    stop("Price series must contain Open, High, Low and Close.")
  }
  LongCandle <- CandleBodyLength(TS)[, "absCandleBodyLength"] >
    ATR(cbind(Hi(TS), Lo(TS), Cl(TS)), n = n, maType = "SMA")[
      ,
      "atr"
    ] * ATRFactor
  CL <- Hi(TS) - Lo(TS)
  BodyHi <- pmax(Op(TS), Cl(TS))
  BodyLo <- pmin(Op(TS), Cl(TS))
  ShortShadow <- Hi(TS) - BodyHi <= CL * maxuppershadowCL &
    BodyLo - Lo(TS) <= CL * maxlowershadowCL
  WhiteMarubozu <- reclass(LongCandle & ShortShadow & Op(TS) <
    Cl(TS), TS)
  BlackMarubozu <- reclass(LongCandle & ShortShadow & Op(TS) >
    Cl(TS), TS)
  result <- cbind(WhiteMarubozu, BlackMarubozu)
  colnames(result) <- c("WhiteMarubozu", "BlackMarubozu")
  xtsAttributes(result) <- list(bars = 1)

  # ── output format ────────────────────────────────────────────────────────
  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df

}
