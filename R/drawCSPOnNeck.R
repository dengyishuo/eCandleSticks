#' Draw CSP On-Neck on Candlestick Chart
#'
#' Adds On-Neck pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_onneck_result Result from \code{\link{addCSPOnNeck}}
#' @param on_neck_color Color for On-Neck points. Default "red".
#' @param on_neck_shape Shape for On-Neck points. Default 25.
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
drawCSPOnNeck <- function(eCandleSticks_result, csp_onneck_result, on_neck_color = "red", on_neck_shape = 25, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_onneck_result) && !xts::is.xts(csp_onneck_result))
    stop("csp_onneck_result must be a data frame or xts object")

  if (xts::is.xts(csp_onneck_result)) {
    csp_onneck_result <- data.frame(
      Date = zoo::index(csp_onneck_result),
      as.data.frame(csp_onneck_result)
    )
  }

  if (!"Date" %in% colnames(csp_onneck_result))
    stop("csp_onneck_result must contain a 'Date' column")

  required_cols <- c("OnNeck")
  if (!all(required_cols %in% colnames(csp_onneck_result)))
    stop(paste("csp_onneck_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_onneck_result$Date <- as.Date(csp_onneck_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_onneck_result, by = "Date", all.x = TRUE)

  on_neck_points <- merged_data[merged_data$OnNeck == TRUE & !is.na(merged_data$OnNeck), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(on_neck_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = on_neck_points,
        ggplot2::aes(x = Date, y = High, color = "On-Neck"),
        size = point_size, shape = on_neck_shape, alpha = point_alpha
      )
  }

  if (nrow(on_neck_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP On-Neck Patterns",
        values = c(      "On-Neck" = on_neck_color),
        breaks = c("On-Neck")
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

  eCandleSticks_result$csp_onneck_data <- csp_onneck_result
  return(eCandleSticks_result)
}
