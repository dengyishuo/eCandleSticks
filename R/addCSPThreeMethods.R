#' Rising/Falling Three Methods Candlestick Pattern
#'
#' Identifies Rising and Falling Three Methods patterns in an OHLC price series.
#' These are five-candle continuation patterns that occur within existing trends.
#'
#' @param x xts Time Series containing OHLC prices
#' @param n Number of preceding candles to calculate median candle body length. Default is 20.
#' @param threshold Minimum/maximum candle body length in relation to the median candle length
#' of \code{n} preceding candle bodies. Default is 1.5.
#'
#' @details
#' Number of candle lines: \bold{5}
#'
#' \strong{Rising Three Methods:}
#' \itemize{
#' \item Prior trend: \bold{up}
#' \item The formation starts with a long white candle body
#' \item The following three bars are smaller candles that open/close within the high/low span of the first candle
#' \item The last candle is again a long white candle that closes above the close price of the first candle
#' \item This pattern signals a potential continuation of the uptrend
#' }
#'
#' \strong{Falling Three Methods:}
#' \itemize{
#' \item Prior trend: \bold{down}
#' \item The formation starts with a long black candle body
#' \item The following three bars are smaller candles that open/close within the high/low span of the first candle
#' \item The last candle is again a long black candle that closes below the close price of the first candle
#' \item This pattern signals a potential continuation of the downtrend
#' }
#'
#' @return
#' A xts object containing the columns:
#' \itemize{
#' \item RisingThreeMethods: TRUE if Rising Three Methods pattern detected
#' \item FallingThreeMethods: TRUE if Falling Three Methods pattern detected
#' }
#'
#' @references
#' The following sites were used to code/document this candlestick pattern:
#' \itemize{
#' \item \url{http://www.investopedia.com/terms/r/rising-three-methods.asp}
#' \item \url{http://www.investopedia.com/terms/f/falling-three-methods.asp}
#' \item \url{http://www.candlesticker.com/Bullish.asp}
#' \item \url{http://www.candlesticker.com/Bearish.asp}
#' }
#'
#' @note
#' The function filters patterns that look like three methods, without considering
#' the current trend direction. If only patterns in specific trends should be filtered,
#' an external trend detection function must be used. See examples.
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' getSymbols("URZ", adjust = TRUE)
#' addCSPThreeMethods(URZ)
#'
#' # Filter rising three methods in uptrends
#' addCSPThreeMethods(URZ)[, "RisingThreeMethods"] &
#'   TrendDetectionChannel(lag(URZ, k = 5))[, "UpTrend"]
#' }
#'
#' @importFrom tibble as_tibble
#' @importFrom zoo index
#' @param output Character. Return format: \code{"xts"} (default), \code{"tibble"}, or \code{"data.frame"}.
#' @family pattern-3bar
#' @family pattern-bull
#' @family pattern-bear
#' @export
#' @importFrom quantmod Op Cl Hi Lo
#' @importFrom TTR runMax runMin
#' @importFrom xts reclass xtsAttributes
#' @importFrom stats lag
addCSPThreeMethods <- function(x, n = 20, threshold = 1.5,
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

  LAG4TS <- LagOHLC(TS, k = 4)
  LAG3TS <- LagOC(TS, k = 3)
  LAG1TS <- LagOC(TS, k = 1)

  MAXOP <- stats::lag(TTR::runMax(quantmod::Op(TS), n = 3), k = 1) # max open for middle 3 candles
  MAXCL <- stats::lag(TTR::runMax(quantmod::Cl(TS), n = 3), k = 1) # max close for middle 3 candles
  MINOP <- stats::lag(TTR::runMin(quantmod::Op(TS), n = 3), k = 1) # min open for middle 3 candles
  MINCL <- stats::lag(TTR::runMin(quantmod::Cl(TS), n = 3), k = 1) # min close for middle 3 candles

  LC4 <- addCSPLongCandleBody(LAG4TS, n = n, threshold = threshold)
  LC0 <- addCSPLongCandleBody(TS, n = n, threshold = threshold)

  RTM <- xts::reclass(
    LC4[, 1] & # 1st candle: long white candle body
      quantmod::Op(LAG3TS) > quantmod::Cl(LAG3TS) & # 2nd candle: black candle
      quantmod::Op(LAG1TS) > quantmod::Cl(LAG1TS) & # 4th candle: black candle
      MAXOP < quantmod::Hi(LAG4TS) & MAXOP > quantmod::Lo(LAG4TS) & # candle bodies 2,3,4 within range of 1st candle
      MAXCL < quantmod::Hi(LAG4TS) & MAXCL > quantmod::Lo(LAG4TS) &
      MINOP < quantmod::Hi(LAG4TS) & MINOP > quantmod::Lo(LAG4TS) &
      MINCL < quantmod::Hi(LAG4TS) & MINCL > quantmod::Lo(LAG4TS) &
      LC0[, 1] & quantmod::Cl(TS) > quantmod::Cl(LAG4TS), # 5th candle: long white candle body that closes higher than 1st candle
    TS
  )

  FTM <- xts::reclass(
    LC4[, 2] & # 1st candle: long black candle body
      quantmod::Op(LAG3TS) < quantmod::Cl(LAG3TS) & # 2nd candle: white candle
      quantmod::Op(LAG1TS) < quantmod::Cl(LAG1TS) & # 4th candle: white candle
      MAXOP < quantmod::Hi(LAG4TS) & MAXOP > quantmod::Lo(LAG4TS) & # candle bodies 2,3,4 within range of 1st candle
      MAXCL < quantmod::Hi(LAG4TS) & MAXCL > quantmod::Lo(LAG4TS) &
      MINOP < quantmod::Hi(LAG4TS) & MINOP > quantmod::Lo(LAG4TS) &
      MINCL < quantmod::Hi(LAG4TS) & MINCL > quantmod::Lo(LAG4TS) &
      LC0[, 2] & quantmod::Cl(TS) < quantmod::Cl(LAG4TS), # 5th candle: long black candle body that closes lower than 1st candle
    TS
  )

  result <- cbind(RTM, FTM)
  colnames(result) <- c("RisingThreeMethods", "FallingThreeMethods")
  xts::xtsAttributes(result) <- list(bars = 5)

  # ── output format ────────────────────────────────────────────────────────
  output <- match.arg(output)
  if (output == "xts") return(result)
  df <- data.frame(date = zoo::index(result), as.data.frame(result),
                   row.names = NULL, check.names = FALSE)
  if (output == "tibble") return(tibble::as_tibble(df))
  df

}
