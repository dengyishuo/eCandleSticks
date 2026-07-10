#' Draw CSP In-Neck on Candlestick Chart
#'
#' Adds In-Neck pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_inneck_result Result from \code{\link{addCSPInNeck}}
#' @param in_neck_color Color for In-Neck points. Default "red".
#' @param in_neck_shape Shape for In-Neck points. Default 25.
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
drawCSPInNeck <- function(eCandleSticks_result, csp_inneck_result, in_neck_color = "red", in_neck_shape = 25, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_inneck_result) && !xts::is.xts(csp_inneck_result))
    stop("csp_inneck_result must be a data frame or xts object")

  if (xts::is.xts(csp_inneck_result)) {
    csp_inneck_result <- data.frame(
      Date = zoo::index(csp_inneck_result),
      as.data.frame(csp_inneck_result)
    )
  }

  if (!"Date" %in% colnames(csp_inneck_result))
    stop("csp_inneck_result must contain a 'Date' column")

  required_cols <- c("InNeck")
  if (!all(required_cols %in% colnames(csp_inneck_result)))
    stop(paste("csp_inneck_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_inneck_result$Date <- as.Date(csp_inneck_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_inneck_result, by = "Date", all.x = TRUE)

  in_neck_points <- merged_data[merged_data$InNeck == TRUE & !is.na(merged_data$InNeck), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(in_neck_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = in_neck_points,
        ggplot2::aes(x = Date, y = High, color = "In-Neck"),
        size = point_size, shape = in_neck_shape, alpha = point_alpha
      )
  }

  if (nrow(in_neck_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP In-Neck Patterns",
        values = c(      "In-Neck" = in_neck_color),
        breaks = c("In-Neck")
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

  eCandleSticks_result$csp_inneck_data <- csp_inneck_result
  return(eCandleSticks_result)
}
