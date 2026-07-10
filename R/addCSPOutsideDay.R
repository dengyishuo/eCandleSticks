#' Identify Outside Day Patterns
#'
#' Detects outside day patterns in an OHLC price series. An outside day occurs when
#' the current bar's high is higher than the previous day's high and the low is lower
#' than the previous day's low.
#'
#' @param x xts Time Series containing OHLC prices
#'
#' @details
#' Number of candle lines: \bold{2}
#'
#' \strong{Outside Day:}
#' In an outside day, the current bar's high is higher than the previous day's
#' high and the low is lower than the previous day's low. This pattern indicates
#' increased volatility and can signal potential trend reversals or breakouts.
#'
#' @return A xts object containing the column:
#' \itemize{
#' \item OutsideDay: TRUE if current candle is an outside day
#' }
#'
#' @references
#' \url{http://www.investopedia.com/terms/o/outside-days.asp#axzz1jWelu1cm}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("AAPL", adjust = TRUE)
#' addCSPOutsideDay(AAPL)
#' }
#'
#' @importFrom tibble as_tibble
#' @importFrom zoo index
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#' @family pattern-2bar
#' @export
#' @importFrom quantmod Hi Lo has.Op has.Hi has.Lo has.Cl
#' @importFrom xts reclass xtsAttributes
addCSPOutsideDay <- function(x,
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

  LAGTS <- LagOHLC(TS, k = 1)
  result <- xts::reclass(
    quantmod::Hi(TS) > quantmod::Hi(LAGTS) &
      quantmod::Lo(TS) < quantmod::Lo(LAGTS),
    TS
  )

  colnames(result) <- "OutsideDay"
  xts::xtsAttributes(result) <- list(bars = 2)

  # ── output format ────────────────────────────────────────────────────────
  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df

}
