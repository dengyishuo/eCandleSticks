#' Draw CSP Spinning Top on Candlestick Chart
#'
#' Adds Spinning Top pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_spinningtop_result Result from \code{\link{addCSPSpinningTop}}
#' @param bullish_spinning_top_color Color for Bullish Spinning Top points. Default "green".
#' @param bullish_spinning_top_shape Shape for Bullish Spinning Top points. Default 24.
#' @param bearish_spinning_top_color Color for Bearish Spinning Top points. Default "red".
#' @param bearish_spinning_top_shape Shape for Bearish Spinning Top points. Default 25.
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
drawCSPSpinningTop <- function(eCandleSticks_result, csp_spinningtop_result, bullish_spinning_top_color = "green", bullish_spinning_top_shape = 24, bearish_spinning_top_color = "red", bearish_spinning_top_shape = 25, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_spinningtop_result) && !xts::is.xts(csp_spinningtop_result))
    stop("csp_spinningtop_result must be a data frame or xts object")

  if (xts::is.xts(csp_spinningtop_result)) {
    csp_spinningtop_result <- data.frame(
      Date = zoo::index(csp_spinningtop_result),
      as.data.frame(csp_spinningtop_result)
    )
  }

  if (!"Date" %in% colnames(csp_spinningtop_result))
    stop("csp_spinningtop_result must contain a 'Date' column")

  required_cols <- c("BullSpinningTop", "BearSpinningTop")
  if (!all(required_cols %in% colnames(csp_spinningtop_result)))
    stop(paste("csp_spinningtop_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_spinningtop_result$Date <- as.Date(csp_spinningtop_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_spinningtop_result, by = "Date", all.x = TRUE)

  bullish_spinning_top_points <- merged_data[merged_data$BullSpinningTop == TRUE & !is.na(merged_data$BullSpinningTop), ]
  bearish_spinning_top_points <- merged_data[merged_data$BearSpinningTop == TRUE & !is.na(merged_data$BearSpinningTop), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(bullish_spinning_top_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bullish_spinning_top_points,
        ggplot2::aes(x = Date, y = Low, color = "Bullish Spinning Top"),
        size = point_size, shape = bullish_spinning_top_shape, alpha = point_alpha
      )
  }
  if (nrow(bearish_spinning_top_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bearish_spinning_top_points,
        ggplot2::aes(x = Date, y = High, color = "Bearish Spinning Top"),
        size = point_size, shape = bearish_spinning_top_shape, alpha = point_alpha
      )
  }

  if (nrow(bullish_spinning_top_points) > 0 || nrow(bearish_spinning_top_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Spinning Top Patterns",
        values = c(      "Bullish Spinning Top" = bullish_spinning_top_color,       "Bearish Spinning Top" = bearish_spinning_top_color),
        breaks = c("Bullish Spinning Top", "Bearish Spinning Top")
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

  eCandleSticks_result$csp_spinningtop_data <- csp_spinningtop_result
  return(eCandleSticks_result)
}
