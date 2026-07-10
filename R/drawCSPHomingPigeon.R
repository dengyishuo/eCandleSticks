#' Draw CSP Homing Pigeon on Candlestick Chart
#'
#' Adds Homing Pigeon pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_homingpigeon_result Result from \code{\link{addCSPHomingPigeon}}
#' @param homing_pigeon_color Color for Homing Pigeon points. Default "green".
#' @param homing_pigeon_shape Shape for Homing Pigeon points. Default 24.
#' @param point_size Size for the pattern points. Default 3.
#' @param point_alpha Alpha transparency. Default 0.8.
#'
#' @return Modified eCandleSticks result list
#' @family pattern-2bar
#' @family pattern-bull
#' @importFrom ggplot2 geom_point aes scale_color_manual guides guide_legend
#' @importFrom cowplot plot_grid
#' @importFrom xts is.xts
#' @importFrom zoo index
#' @export
drawCSPHomingPigeon <- function(eCandleSticks_result, csp_homingpigeon_result, homing_pigeon_color = "green", homing_pigeon_shape = 24, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_homingpigeon_result) && !xts::is.xts(csp_homingpigeon_result))
    stop("csp_homingpigeon_result must be a data frame or xts object")

  if (xts::is.xts(csp_homingpigeon_result)) {
    csp_homingpigeon_result <- data.frame(
      Date = zoo::index(csp_homingpigeon_result),
      as.data.frame(csp_homingpigeon_result)
    )
  }

  if (!"Date" %in% colnames(csp_homingpigeon_result))
    stop("csp_homingpigeon_result must contain a 'Date' column")

  required_cols <- c("HomingPigeon")
  if (!all(required_cols %in% colnames(csp_homingpigeon_result)))
    stop(paste("csp_homingpigeon_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_homingpigeon_result$Date <- as.Date(csp_homingpigeon_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_homingpigeon_result, by = "Date", all.x = TRUE)

  homing_pigeon_points <- merged_data[merged_data$HomingPigeon == TRUE & !is.na(merged_data$HomingPigeon), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(homing_pigeon_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = homing_pigeon_points,
        ggplot2::aes(x = Date, y = Low, color = "Homing Pigeon"),
        size = point_size, shape = homing_pigeon_shape, alpha = point_alpha
      )
  }

  if (nrow(homing_pigeon_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Homing Pigeon Patterns",
        values = c(      "Homing Pigeon" = homing_pigeon_color),
        breaks = c("Homing Pigeon")
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

  eCandleSticks_result$csp_homingpigeon_data <- csp_homingpigeon_result
  return(eCandleSticks_result)
}
