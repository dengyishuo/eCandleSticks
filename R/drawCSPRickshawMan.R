#' Draw CSP Rickshaw Man on Candlestick Chart
#'
#' Adds Rickshaw Man pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_rickshawman_result Result from \code{\link{addCSPRickshawMan}}
#' @param rickshaw_man_color Color for Rickshaw Man points. Default "purple".
#' @param rickshaw_man_shape Shape for Rickshaw Man points. Default 4.
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
drawCSPRickshawMan <- function(eCandleSticks_result, csp_rickshawman_result, rickshaw_man_color = "purple", rickshaw_man_shape = 4, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_rickshawman_result) && !xts::is.xts(csp_rickshawman_result))
    stop("csp_rickshawman_result must be a data frame or xts object")

  if (xts::is.xts(csp_rickshawman_result)) {
    csp_rickshawman_result <- data.frame(
      Date = zoo::index(csp_rickshawman_result),
      as.data.frame(csp_rickshawman_result)
    )
  }

  if (!"Date" %in% colnames(csp_rickshawman_result))
    stop("csp_rickshawman_result must contain a 'Date' column")

  required_cols <- c("RickshawMan")
  if (!all(required_cols %in% colnames(csp_rickshawman_result)))
    stop(paste("csp_rickshawman_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_rickshawman_result$Date <- as.Date(csp_rickshawman_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_rickshawman_result, by = "Date", all.x = TRUE)

  rickshaw_man_points <- merged_data[merged_data$RickshawMan == TRUE & !is.na(merged_data$RickshawMan), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(rickshaw_man_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = rickshaw_man_points,
        ggplot2::aes(x = Date, y = Low, color = "Rickshaw Man"),
        size = point_size, shape = rickshaw_man_shape, alpha = point_alpha
      )
  }

  if (nrow(rickshaw_man_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Rickshaw Man Patterns",
        values = c(      "Rickshaw Man" = rickshaw_man_color),
        breaks = c("Rickshaw Man")
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

  eCandleSticks_result$csp_rickshawman_data <- csp_rickshawman_result
  return(eCandleSticks_result)
}
