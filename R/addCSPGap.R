#' Gap Candlestick Pattern
#'
#' Identifies price gaps between two candles in an OHLC price series.
#' Also known as Rising Window (gap up) and Falling Window (gap down).
#'
#' @param x xts Time Series containing OHLC prices
#' @param ignoreShadows If TRUE, only Open and Close Price are evaluated.
#' High and Low are ignored. Default is FALSE.
#'
#' @details
#' Number of candle lines: \bold{2}
#'
#' When ignoreShadows = FALSE (default), a gap up occurs when the current candle's
#' low is higher than the previous candle's high. A gap down occurs when the
#' current candle's high is lower than the previous candle's low.
#'
#' When ignoreShadows = TRUE, a gap up occurs when the current candle's body
#' (range between open and close) is completely above the previous candle's body.
#' A gap down occurs when the current candle's body is completely below the
#' previous candle's body.
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item GapUp: TRUE if gap up detected
#' \item GapDown: TRUE if gap down detected
#' }
#'
#' @note
#' The up/down gaps are also called \emph{Rising Window}/\emph{Falling Window}
#' in candlestick analysis terminology.
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' addCSPGap(YHOO) # examine whole candle length
#' addCSPGap(YHOO, ignoreShadows = TRUE) # examine only candle bodies
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
addCSPGap <- function(x, ignoreShadows = FALSE,
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

  if (ignoreShadows == TRUE) {
    if (!(has.Op(TS) && has.Cl(TS))) {
      stop("Price series must contain Open and Close.")
    }
    LAGTS <- LagOC(TS, k = 1)
    TSOC <- cbind(quantmod::Op(TS), quantmod::Cl(TS))
    LAGTSOC <- cbind(quantmod::Op(LAGTS), quantmod::Cl(LAGTS))
    UPGAP <- xts::reclass(pmax(quantmod::Op(LAGTSOC), quantmod::Cl(LAGTSOC)) <
      pmin(quantmod::Op(TSOC), quantmod::Cl(TSOC)), TS)
    DOWNGAP <- xts::reclass(pmin(quantmod::Op(LAGTSOC), quantmod::Cl(LAGTSOC)) >
      pmax(quantmod::Op(TSOC), quantmod::Cl(TSOC)), TS)
  } else if (ignoreShadows == FALSE) {
    if (!(has.Hi(TS) && has.Lo(TS) && has.Op(TS) && has.Cl(TS))) {
      stop("Price series must contain Open, High, Low and Close.")
    }
    LAGTS <- LagOHLC(TS, k = 1)
    UPGAP <- xts::reclass(quantmod::Lo(TS) > quantmod::Hi(LAGTS), TS)
    DOWNGAP <- xts::reclass(quantmod::Hi(TS) < quantmod::Lo(LAGTS), TS)
  }
  result <- cbind(UPGAP, DOWNGAP)
  colnames(result) <- c("GapUp", "GapDown")
  xts::xtsAttributes(result) <- list(bars = 2)

  # ── output format ────────────────────────────────────────────────────────
  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df

}
