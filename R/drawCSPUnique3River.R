#' Draw CSP Unique Three River on Candlestick Chart
#'
#' Adds Unique Three River pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_unique3river_result Result from \code{\link{addCSPUnique3River}}
#' @param unique_three_river_color Color for Unique Three River points. Default "green".
#' @param unique_three_river_shape Shape for Unique Three River points. Default 24.
#' @param point_size Size for the pattern points. Default 3.
#' @param point_alpha Alpha transparency. Default 0.8.
#'
#' @return Modified eCandleSticks result list
#' @family pattern-3bar
#' @family pattern-bull
#' @importFrom ggplot2 geom_point aes scale_color_manual guides guide_legend
#' @importFrom cowplot plot_grid
#' @importFrom xts is.xts
#' @importFrom zoo index
#' @export
drawCSPUnique3River <- function(eCandleSticks_result, csp_unique3river_result, unique_three_river_color = "green", unique_three_river_shape = 24, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_unique3river_result) && !xts::is.xts(csp_unique3river_result))
    stop("csp_unique3river_result must be a data frame or xts object")

  if (xts::is.xts(csp_unique3river_result)) {
    csp_unique3river_result <- data.frame(
      Date = zoo::index(csp_unique3river_result),
      as.data.frame(csp_unique3river_result)
    )
  }

  if (!"Date" %in% colnames(csp_unique3river_result))
    stop("csp_unique3river_result must contain a 'Date' column")

  required_cols <- c("Unique3River")
  if (!all(required_cols %in% colnames(csp_unique3river_result)))
    stop(paste("csp_unique3river_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_unique3river_result$Date <- as.Date(csp_unique3river_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_unique3river_result, by = "Date", all.x = TRUE)

  unique_three_river_points <- merged_data[merged_data$Unique3River == TRUE & !is.na(merged_data$Unique3River), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(unique_three_river_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = unique_three_river_points,
        ggplot2::aes(x = Date, y = Low, color = "Unique Three River"),
        size = point_size, shape = unique_three_river_shape, alpha = point_alpha
      )
  }

  if (nrow(unique_three_river_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Unique Three River Patterns",
        values = c(      "Unique Three River" = unique_three_river_color),
        breaks = c("Unique Three River")
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

  eCandleSticks_result$csp_unique3river_data <- csp_unique3river_result
  return(eCandleSticks_result)
}
