#' Draw CSP Long-Legged Doji on Candlestick Chart
#'
#' Adds Long-Legged Doji pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_longleggeddoji_result Result from \code{\link{addCSPLongLeggedDoji}}
#' @param long_legged_doji_color Color for Long-Legged Doji points. Default "purple".
#' @param long_legged_doji_shape Shape for Long-Legged Doji points. Default 4.
#' @param point_size Size for the pattern points. Default 3.
#' @param point_alpha Alpha transparency. Default 0.8.
#'
#' @return Modified eCandleSticks result list
#' @family pattern-1bar
#' @importFrom ggplot2 geom_point aes scale_color_manual guides guide_legend
#' @importFrom cowplot plot_grid
#' @importFrom xts is.xts
#' @importFrom zoo index
#' @export
drawCSPLongLeggedDoji <- function(eCandleSticks_result, csp_longleggeddoji_result, long_legged_doji_color = "purple", long_legged_doji_shape = 4, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_longleggeddoji_result) && !xts::is.xts(csp_longleggeddoji_result))
    stop("csp_longleggeddoji_result must be a data frame or xts object")

  if (xts::is.xts(csp_longleggeddoji_result)) {
    csp_longleggeddoji_result <- data.frame(
      Date = zoo::index(csp_longleggeddoji_result),
      as.data.frame(csp_longleggeddoji_result)
    )
  }

  if (!"Date" %in% colnames(csp_longleggeddoji_result))
    stop("csp_longleggeddoji_result must contain a 'Date' column")

  required_cols <- c("LongLeggedDoji")
  if (!all(required_cols %in% colnames(csp_longleggeddoji_result)))
    stop(paste("csp_longleggeddoji_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_longleggeddoji_result$Date <- as.Date(csp_longleggeddoji_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_longleggeddoji_result, by = "Date", all.x = TRUE)

  long_legged_doji_points <- merged_data[merged_data$LongLeggedDoji == TRUE & !is.na(merged_data$LongLeggedDoji), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(long_legged_doji_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = long_legged_doji_points,
        ggplot2::aes(x = Date, y = Low, color = "Long-Legged Doji"),
        size = point_size, shape = long_legged_doji_shape, alpha = point_alpha
      )
  }

  if (nrow(long_legged_doji_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Long-Legged Doji Patterns",
        values = c(      "Long-Legged Doji" = long_legged_doji_color),
        breaks = c("Long-Legged Doji")
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

  eCandleSticks_result$csp_longleggeddoji_data <- csp_longleggeddoji_result
  return(eCandleSticks_result)
}
