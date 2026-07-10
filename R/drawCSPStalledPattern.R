#' Draw CSP Stalled Pattern on Candlestick Chart
#'
#' Adds Stalled Pattern pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_stalledpattern_result Result from \code{\link{addCSPStalledPattern}}
#' @param stalled_pattern_color Color for Stalled Pattern points. Default "red".
#' @param stalled_pattern_shape Shape for Stalled Pattern points. Default 25.
#' @param point_size Size for the pattern points. Default 3.
#' @param point_alpha Alpha transparency. Default 0.8.
#'
#' @return Modified eCandleSticks result list
#' @family pattern-3bar
#' @family pattern-bear
#' @importFrom ggplot2 geom_point aes scale_color_manual guides guide_legend
#' @importFrom cowplot plot_grid
#' @importFrom xts is.xts
#' @importFrom zoo index
#' @export
drawCSPStalledPattern <- function(eCandleSticks_result, csp_stalledpattern_result, stalled_pattern_color = "red", stalled_pattern_shape = 25, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_stalledpattern_result) && !xts::is.xts(csp_stalledpattern_result))
    stop("csp_stalledpattern_result must be a data frame or xts object")

  if (xts::is.xts(csp_stalledpattern_result)) {
    csp_stalledpattern_result <- data.frame(
      Date = zoo::index(csp_stalledpattern_result),
      as.data.frame(csp_stalledpattern_result)
    )
  }

  if (!"Date" %in% colnames(csp_stalledpattern_result))
    stop("csp_stalledpattern_result must contain a 'Date' column")

  required_cols <- c("StalledPattern")
  if (!all(required_cols %in% colnames(csp_stalledpattern_result)))
    stop(paste("csp_stalledpattern_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_stalledpattern_result$Date <- as.Date(csp_stalledpattern_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_stalledpattern_result, by = "Date", all.x = TRUE)

  stalled_pattern_points <- merged_data[merged_data$StalledPattern == TRUE & !is.na(merged_data$StalledPattern), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(stalled_pattern_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = stalled_pattern_points,
        ggplot2::aes(x = Date, y = High, color = "Stalled Pattern"),
        size = point_size, shape = stalled_pattern_shape, alpha = point_alpha
      )
  }

  if (nrow(stalled_pattern_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Stalled Pattern Patterns",
        values = c(      "Stalled Pattern" = stalled_pattern_color),
        breaks = c("Stalled Pattern")
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

  eCandleSticks_result$csp_stalledpattern_data <- csp_stalledpattern_result
  return(eCandleSticks_result)
}
