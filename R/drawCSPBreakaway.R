#' Draw CSP Breakaway on Candlestick Chart
#'
#' Adds Breakaway pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_breakaway_result Result from \code{\link{addCSPBreakaway}}
#' @param bullish_breakaway_color Color for Bullish Breakaway points. Default "green".
#' @param bullish_breakaway_shape Shape for Bullish Breakaway points. Default 24.
#' @param bearish_breakaway_color Color for Bearish Breakaway points. Default "red".
#' @param bearish_breakaway_shape Shape for Bearish Breakaway points. Default 25.
#' @param point_size Size for the pattern points. Default 3.
#' @param point_alpha Alpha transparency. Default 0.8.
#'
#' @return Modified eCandleSticks result list
#' @family pattern-5bar
#' @family pattern-bull
#' @family pattern-bear
#' @importFrom ggplot2 geom_point aes scale_color_manual guides guide_legend
#' @importFrom cowplot plot_grid
#' @importFrom xts is.xts
#' @importFrom zoo index
#' @export
drawCSPBreakaway <- function(eCandleSticks_result, csp_breakaway_result, bullish_breakaway_color = "green", bullish_breakaway_shape = 24, bearish_breakaway_color = "red", bearish_breakaway_shape = 25, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_breakaway_result) && !xts::is.xts(csp_breakaway_result))
    stop("csp_breakaway_result must be a data frame or xts object")

  if (xts::is.xts(csp_breakaway_result)) {
    csp_breakaway_result <- data.frame(
      Date = zoo::index(csp_breakaway_result),
      as.data.frame(csp_breakaway_result)
    )
  }

  if (!"Date" %in% colnames(csp_breakaway_result))
    stop("csp_breakaway_result must contain a 'Date' column")

  required_cols <- c("BullBreakaway", "BearBreakaway")
  if (!all(required_cols %in% colnames(csp_breakaway_result)))
    stop(paste("csp_breakaway_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_breakaway_result$Date <- as.Date(csp_breakaway_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_breakaway_result, by = "Date", all.x = TRUE)

  bullish_breakaway_points <- merged_data[merged_data$BullBreakaway == TRUE & !is.na(merged_data$BullBreakaway), ]
  bearish_breakaway_points <- merged_data[merged_data$BearBreakaway == TRUE & !is.na(merged_data$BearBreakaway), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(bullish_breakaway_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bullish_breakaway_points,
        ggplot2::aes(x = Date, y = Low, color = "Bullish Breakaway"),
        size = point_size, shape = bullish_breakaway_shape, alpha = point_alpha
      )
  }
  if (nrow(bearish_breakaway_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bearish_breakaway_points,
        ggplot2::aes(x = Date, y = High, color = "Bearish Breakaway"),
        size = point_size, shape = bearish_breakaway_shape, alpha = point_alpha
      )
  }

  if (nrow(bullish_breakaway_points) > 0 || nrow(bearish_breakaway_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Breakaway Patterns",
        values = c(      "Bullish Breakaway" = bullish_breakaway_color,       "Bearish Breakaway" = bearish_breakaway_color),
        breaks = c("Bullish Breakaway", "Bearish Breakaway")
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

  eCandleSticks_result$csp_breakaway_data <- csp_breakaway_result
  return(eCandleSticks_result)
}
