#' Short Candle Body Pattern
#'
#' Identifies short candle bodies in an OHLC price series based on their relative length
#' compared to the median of preceding candle bodies.
#'
#' @param x xts Time Series containing Open and Close prices
#' @param n Number of preceding candles to calculate median candle body length. Default is 20.
#' @param threshold Maximum candle body length in relation to the median candle body length
#' of \code{n} preceding candles. Default is 1.
#'
#' @details
#' Classifies candle bodies as short based on their relative length compared to the median
#' of the relative candle body lengths of the preceding \code{n} candles.
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item ShortWhiteCandleBody: TRUE if Short White Candle Body detected
#' \item ShortBlackCandleBody: TRUE if Short Black Candle Body detected
#' }
#'
#' @author Andreas Voellenklee
#'
#' @seealso
#' \code{\link{CandleBodyLength}}, \code{\link{addCSPLongCandle}}
#'
#' @examples
#' \dontrun{
#' getSymbols("YHOO", adjust = TRUE)
#' addCSPShortCandleBody(YHOO)
#' }
#'
#' @importFrom tibble as_tibble
#' @importFrom zoo index
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#' @family pattern-1bar
#' @family pattern-bull
#' @family pattern-bear
#' @export
#' @importFrom quantmod Op Cl
#' @importFrom TTR runMedian
#' @importFrom xts reclass xtsAttributes
addCSPShortCandleBody <- function(x, n = 20, threshold = 1,
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

  if (!(has.Op(TS) && has.Cl(TS))) {
    stop("Price series must contain Open and Close.")
  }

  CBL <- CandleBodyLength(TS)
  CBLMedian <- runMedian(CBL[, 1], n = n) # use relative CandleBodyLength

  ShortWhiteCandleBody <- xts::reclass(
    CBL[, 1] < CBLMedian * threshold & quantmod::Cl(TS) >= quantmod::Op(TS),
    TS
  )

  ShortBlackCandleBody <- xts::reclass(
    CBL[, 1] < CBLMedian * threshold & quantmod::Op(TS) > quantmod::Cl(TS),
    TS
  )

  result <- cbind(ShortWhiteCandleBody, ShortBlackCandleBody)
  colnames(result) <- c("ShortWhiteCandleBody", "ShortBlackCandleBody")
  xts::xtsAttributes(result) <- list(bars = 1)

  # ── output format ────────────────────────────────────────────────────────
  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df

}
