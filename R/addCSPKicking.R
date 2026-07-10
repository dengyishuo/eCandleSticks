#' Kicking Candlestick Pattern
#'
#' Identifies bullish and bearish Kicking candlestick patterns in an OHLC price series.
#' This is a two-candle pattern that signals strong reversal potential.
#'
#' @param x xts Time Series containing OHLC prices
#' @param ignoreShadows If TRUE, only Open and Close Price are evaluated.
#' High and Low are ignored. Default is TRUE.
#' @param n Number of preceding candles to calculate median candle length or average True Range. Default is 20.
#' @param threshold Minimum candle length in relation to the median candle length of \code{n} preceding candles. Default is 1.
#' @param ATRFactor Minimum size of candle body compared to the ATR. Default is 1.
#' @param maxshadowCL Maximum tolerated upper and lower shadow to candle length ratio. Default is 0.1.
#' @details
#' Number of candle lines: \bold{2}
#'
#' \strong{Bullish Kicking Pattern:}
#' \itemize{
#' \item A White Marubozu following a Black Marubozu
#' \item After the Black Marubozu, market gaps higher on the opening
#' \item The pattern is filtered with \code{ignoreShadows=FALSE} and occurs very scarcely
#' \item When \code{ignoreShadows=TRUE}, a less strict filter is applied: a long black candle body
#' followed by a long white candle body with a gap
#' }
#'
#' \strong{Bearish Kicking Pattern:}
#' \itemize{
#' \item Opposite of Bullish Kicking Pattern
#' \item A Black Marubozu following a White Marubozu with a gap down
#' }
#'
#' Unlike most other candlestick patterns, the previous market direction is not important for this pattern.
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item Bull.Kicking: TRUE if Bullish Kicking pattern detected
#' \item Bear.Kicking: TRUE if Bearish Kicking pattern detected
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
#' \code{\link{addCSPLongCandleBody}}
#' \code{\link{addCSPMarubozu}}
#' \code{\link{addCSPGap}}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#'
#' # Look for Kicking Pattern right out of the textbook
#' # They occur only once in a blue moon
#' addCSPKicking(YHOO, ignoreShadows = FALSE, maxshadowCL = 0)
#'
#' # Use less strict filter rules
#' addCSPKicking(YHOO)
#' }
#'
#' @importFrom tibble as_tibble
#' @importFrom zoo index
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#' @family pattern-2bar
#' @family pattern-bull
#' @family pattern-bear
#' @export
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom xts reclass xtsAttributes
#' @importFrom TTR ATR
addCSPKicking <- function(x, ignoreShadows = TRUE, n = 20, threshold = 1, ATRFactor = 1, maxshadowCL = 0.1,
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

  TSGAP <- addCSPGap(TS, ignoreShadows = ignoreShadows)

  if (ignoreShadows == FALSE) {
    MB <- addCSPMarubozu(TS,
      n = n, ATRFactor = ATRFactor,
      maxuppershadowCL = maxshadowCL,
      maxlowershadowCL = maxshadowCL
    )

    # 修正列索引：原代码错误地使用了第4列，应该是第2列
    WMB1 <- quantmod::Lag(MB[, 1], k = 1) # 白色Marubozu
    BMB1 <- quantmod::Lag(MB[, 2], k = 1) # 黑色Marubozu

    BULLK <- xts::reclass(
      TSGAP[, 1] & # Gap Up
        BMB1 & MB[, 1], # 前黑Marubozu，后白Marubozu
      TS
    )

    BEARK <- xts::reclass(
      TSGAP[, 2] & # Gap Down
        WMB1 & MB[, 2], # 前白Marubozu，后黑Marubozu
      TS
    )
  } else if (ignoreShadows == TRUE) {
    LCB <- addCSPLongCandleBody(TS, n = n, threshold = threshold)
    LWCB1 <- quantmod::Lag(LCB[, 1], k = 1) # 长白实体
    LBCB1 <- quantmod::Lag(LCB[, 2], k = 1) # 长黑实体


    BULLK <- xts::reclass(
      TSGAP[, 1] & # Gap Up
        LBCB1 & LCB[, 1], # 前长黑实体，后长白实体
      TS
    )

    BEARK <- xts::reclass(
      TSGAP[, 2] & # Gap Down
        LWCB1 & LCB[, 2], # 前长白实体，后长黑实体
      TS
    )
  } else {
    stop("ignoreShadows must be either TRUE or FALSE")
  }

  result <- cbind(BULLK, BEARK)
  colnames(result) <- c("Bull.Kicking", "Bear.Kicking")
  xts::xtsAttributes(result) <- list(bars = 2)

  # ── output format ────────────────────────────────────────────────────────
  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df

}
