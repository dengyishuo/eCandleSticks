#' Draw CSP Closing Marubozu on Candlestick Chart
#'
#' Adds Closing Marubozu pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_closingmarubozu_result Result from \code{\link{addCSPClosingMarubozu}}
#' @param bullish_closing_marubozu_color Color for Bullish Closing Marubozu points. Default "green".
#' @param bullish_closing_marubozu_shape Shape for Bullish Closing Marubozu points. Default 24.
#' @param bearish_closing_marubozu_color Color for Bearish Closing Marubozu points. Default "red".
#' @param bearish_closing_marubozu_shape Shape for Bearish Closing Marubozu points. Default 25.
#' @param point_size Size for the pattern points. Default 3.
#' @param point_alpha Alpha transparency. Default 0.8.
#'
#' @return Modified eCandleSticks result list
#' @family pattern-1bar
#' @family pattern-bull
#' @family pattern-bear
#' @importFrom ggplot2 geom_point aes scale_color_manual guides guide_legend
#' @importFrom cowplot plot_grid
#' @importFrom xts is.xts
#' @importFrom zoo index
#' @export
drawCSPClosingMarubozu <- function(eCandleSticks_result, csp_closingmarubozu_result, bullish_closing_marubozu_color = "green", bullish_closing_marubozu_shape = 24, bearish_closing_marubozu_color = "red", bearish_closing_marubozu_shape = 25, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_closingmarubozu_result) && !xts::is.xts(csp_closingmarubozu_result))
    stop("csp_closingmarubozu_result must be a data frame or xts object")

  if (xts::is.xts(csp_closingmarubozu_result)) {
    csp_closingmarubozu_result <- data.frame(
      Date = zoo::index(csp_closingmarubozu_result),
      as.data.frame(csp_closingmarubozu_result)
    )
  }

  if (!"Date" %in% colnames(csp_closingmarubozu_result))
    stop("csp_closingmarubozu_result must contain a 'Date' column")

  required_cols <- c("BullClosingMarubozu", "BearClosingMarubozu")
  if (!all(required_cols %in% colnames(csp_closingmarubozu_result)))
    stop(paste("csp_closingmarubozu_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_closingmarubozu_result$Date <- as.Date(csp_closingmarubozu_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_closingmarubozu_result, by = "Date", all.x = TRUE)

  bullish_closing_marubozu_points <- merged_data[merged_data$BullClosingMarubozu == TRUE & !is.na(merged_data$BullClosingMarubozu), ]
  bearish_closing_marubozu_points <- merged_data[merged_data$BearClosingMarubozu == TRUE & !is.na(merged_data$BearClosingMarubozu), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(bullish_closing_marubozu_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bullish_closing_marubozu_points,
        ggplot2::aes(x = Date, y = Low, color = "Bullish Closing Marubozu"),
        size = point_size, shape = bullish_closing_marubozu_shape, alpha = point_alpha
      )
  }
  if (nrow(bearish_closing_marubozu_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bearish_closing_marubozu_points,
        ggplot2::aes(x = Date, y = High, color = "Bearish Closing Marubozu"),
        size = point_size, shape = bearish_closing_marubozu_shape, alpha = point_alpha
      )
  }

  if (nrow(bullish_closing_marubozu_points) > 0 || nrow(bearish_closing_marubozu_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Closing Marubozu Patterns",
        values = c(      "Bullish Closing Marubozu" = bullish_closing_marubozu_color,       "Bearish Closing Marubozu" = bearish_closing_marubozu_color),
        breaks = c("Bullish Closing Marubozu", "Bearish Closing Marubozu")
      )
  }

  eCandleSticks_result$price_plot <- price_plot

  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr", rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot
  }

  eCandleSticks_result$csp_closingmarubozu_data <- csp_closingmarubozu_result
  return(eCandleSticks_result)
}
