#' Enhanced Candlestick Charting and pattern detect package
#'
#' @description
#' This package contains functions to detect the most popular japanese candlestick formations like Doji, Engulfing Pattern, Harami, etc. out of OHLC price data.
#'
#' @name ecandlesticks-package
#' @aliases ecandlesticks-package
#' @docType package
#'
#' @details
#' \tabular{ll}{
#' Package: \tab ecandlesticks\cr
#' Version: \tab see description file\cr
#' Date: \tab see description file\cr
#' Depends: \tab R (>= 4.0.0)\cr
#' Imports: \tab ggplot2 (>= 3.4.0), cowplot (>= 1.1.1), quantmod (>= 0.4.25),\cr
#'          \tab dplyr (>= 1.1.0), lubridate (>= 1.9.0), zoo (>= 1.8-12)\cr
#' Suggests: \tab knitr (>= 1.42), rmarkdown (>= 2.21)\cr
#' License: \tab GPL-3\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#'
#' @note
#' All candlestick pattern detection functions (CSP*) preserve the xts time series' attributes and add one attribute bars, that represents the number of bars the formation consists of.
#' The timezone of the R session should be set to UTC to prevent issues with internal lag functions.
#' Sys.setenv(TZ="UTC")
#'
#' @author
#' Andreas Voellenklee , DengYishuo
#'
#' Maintainer: Andreas Voellenklee \email{wotuzu17@gmail.com} , DengYishuo \email{dengyishuo@163.com}
#'
#' @references
#' The following sites were used to code/document this package:
#' http://www.onlinetradingconcepts.com/TechnicalAnalysis/Candlesticks/CandlestickBasics.html
#' http://candlesticker.com/
#' http://thepatternsite.com/CandleEntry.html
