#' Draw CSP Thrusting on Candlestick Chart
#'
#' Adds Thrusting pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_thrusting_result Result from \code{\link{addCSPThrusting}}
#' @param thrusting_color Color for Thrusting points. Default "red".
#' @param thrusting_shape Shape for Thrusting points. Default 25.
#' @param point_size Size for the pattern points. Default 3.
#' @param point_alpha Alpha transparency. Default 0.8.
#'
#' @return Modified eCandleSticks result list
#' @family pattern-2bar
#' @family pattern-bear
#' @importFrom ggplot2 geom_point aes scale_color_manual guides guide_legend
#' @importFrom cowplot plot_grid
#' @importFrom xts is.xts
#' @importFrom zoo index
#' @export
drawCSPThrusting <- function(eCandleSticks_result, csp_thrusting_result, thrusting_color = "red", thrusting_shape = 25, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_thrusting_result) && !xts::is.xts(csp_thrusting_result))
    stop("csp_thrusting_result must be a data frame or xts object")

  if (xts::is.xts(csp_thrusting_result)) {
    csp_thrusting_result <- data.frame(
      Date = zoo::index(csp_thrusting_result),
      as.data.frame(csp_thrusting_result)
    )
  }

  if (!"Date" %in% colnames(csp_thrusting_result))
    stop("csp_thrusting_result must contain a 'Date' column")

  required_cols <- c("Thrusting")
  if (!all(required_cols %in% colnames(csp_thrusting_result)))
    stop(paste("csp_thrusting_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_thrusting_result$Date <- as.Date(csp_thrusting_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_thrusting_result, by = "Date", all.x = TRUE)

  thrusting_points <- merged_data[merged_data$Thrusting == TRUE & !is.na(merged_data$Thrusting), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(thrusting_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = thrusting_points,
        ggplot2::aes(x = Date, y = High, color = "Thrusting"),
        size = point_size, shape = thrusting_shape, alpha = point_alpha
      )
  }

  if (nrow(thrusting_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Thrusting Patterns",
        values = c(      "Thrusting" = thrusting_color),
        breaks = c("Thrusting")
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

  eCandleSticks_result$csp_thrusting_data <- csp_thrusting_result
  return(eCandleSticks_result)
}
