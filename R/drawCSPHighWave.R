#' Draw CSP High-Wave on Candlestick Chart
#'
#' Adds High-Wave pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_highwave_result Result from \code{\link{addCSPHighWave}}
#' @param high_wave_color Color for High-Wave points. Default "purple".
#' @param high_wave_shape Shape for High-Wave points. Default 3.
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
drawCSPHighWave <- function(eCandleSticks_result, csp_highwave_result, high_wave_color = "purple", high_wave_shape = 3, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_highwave_result) && !xts::is.xts(csp_highwave_result))
    stop("csp_highwave_result must be a data frame or xts object")

  if (xts::is.xts(csp_highwave_result)) {
    csp_highwave_result <- data.frame(
      Date = zoo::index(csp_highwave_result),
      as.data.frame(csp_highwave_result)
    )
  }

  if (!"Date" %in% colnames(csp_highwave_result))
    stop("csp_highwave_result must contain a 'Date' column")

  required_cols <- c("HighWave")
  if (!all(required_cols %in% colnames(csp_highwave_result)))
    stop(paste("csp_highwave_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_highwave_result$Date <- as.Date(csp_highwave_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_highwave_result, by = "Date", all.x = TRUE)

  high_wave_points <- merged_data[merged_data$HighWave == TRUE & !is.na(merged_data$HighWave), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(high_wave_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = high_wave_points,
        ggplot2::aes(x = Date, y = Low, color = "High-Wave"),
        size = point_size, shape = high_wave_shape, alpha = point_alpha
      )
  }

  if (nrow(high_wave_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP High-Wave Patterns",
        values = c(      "High-Wave" = high_wave_color),
        breaks = c("High-Wave")
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

  eCandleSticks_result$csp_highwave_data <- csp_highwave_result
  return(eCandleSticks_result)
}
