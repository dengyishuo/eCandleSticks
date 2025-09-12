#' Position and color of the next candle in a OC price series
#'
#' Examines open and close price of the following candle relative to the close price
#' of the current candle. Also returns the color of the following candle.
#'
#' @param TS xts Time Series containing Open and Close Prices
#'
#' @details
#' Sometimes it is suggested to wait for a confirmation in the form of a higher/lower
#' following close and/or a white/black candle before considering trading the pattern.
#' This function compares the price levels and returns a xts object with columns of
#' boolean values for each condition.
#'
#' @return A xts object containing the columns:
#' \itemize{
#' \item HigherOpen: TRUE if following candle opens higher than current close
#' \item LowerOpen: TRUE if following candle opens lower than current close
#' \item HigherClose: TRUE if following candle closes higher than current close
#' \item LowerClose: TRUE if following candle closes lower than current close
#' \item White: TRUE if following candle is white (Cl > Op)
#' \item Black: TRUE if following candle is black (Cl < Op)
#' }
#'
#' @note This function uses the Next function from the \pkg{quantmod} package.
#'
#' @seealso \code{\link[quantmod]{Next}}
#'
#' @author Andreas Voellenklee
#'
#' @examples
#' \dontrun{
#' # Filter for inverted hammers in uptrends that are followed by a lower close the next day
#' getSymbols("YHOO", adjust = TRUE)
#' CSPInvertedHammer(YHOO) &
#'   TrendDetectionChannel(YHOO)[, "UpTrend"] &
#'   nextCandlePosition(YHOO)[, "LowerClose"]
#' }
#'
#' @export
#' @importFrom quantmod Op Cl Next has.Op has.Cl
#' @importFrom xts as.xts reclass
nextCandlePosition <- function(TS) {
  if (!(has.Op(TS) && has.Cl(TS))) {
    stop("Price series must contain Open and Close.")
  }

  NextOp <- xts::as.xts(quantmod::Next(quantmod::Op(TS)))
  NextCl <- xts::as.xts(quantmod::Next(quantmod::Cl(TS)))

  HigherOpen <- NextOp > quantmod::Op(TS)
  LowerOpen <- NextOp < quantmod::Op(TS)
  HigherClose <- NextCl > quantmod::Cl(TS)
  LowerClose <- NextCl < quantmod::Cl(TS)
  NextWhite <- NextOp < NextCl
  NextBlack <- NextOp > NextCl

  result <- xts::reclass(
    cbind(HigherOpen, LowerOpen, HigherClose, LowerClose, NextWhite, NextBlack),
    TS
  )
  colnames(result) <- c(
    "HigherOpen", "LowerOpen", "HigherClose",
    "LowerClose", "White", "Black"
  )
  return(result)
}
