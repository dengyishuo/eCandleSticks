#' Draw CSP Concealing Baby Swallow on Candlestick Chart
#'
#' Adds Concealing Baby Swallow pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_concealbabyswallow_result Result from \code{\link{addCSPConcealBabySwallow}}
#' @param concealing_baby_swallow_color Color for Concealing Baby Swallow points. Default "green".
#' @param concealing_baby_swallow_shape Shape for Concealing Baby Swallow points. Default 24.
#' @param point_size Size for the pattern points. Default 3.
#' @param point_alpha Alpha transparency. Default 0.8.
#'
#' @return Modified eCandleSticks result list
#' @family pattern-4bar
#' @family pattern-bull
#' @importFrom ggplot2 geom_point aes scale_color_manual guides guide_legend
#' @importFrom cowplot plot_grid
#' @importFrom xts is.xts
#' @importFrom zoo index
#' @export
drawCSPConcealBabySwallow <- function(eCandleSticks_result, csp_concealbabyswallow_result, concealing_baby_swallow_color = "green", concealing_baby_swallow_shape = 24, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_concealbabyswallow_result) && !xts::is.xts(csp_concealbabyswallow_result))
    stop("csp_concealbabyswallow_result must be a data frame or xts object")

  if (xts::is.xts(csp_concealbabyswallow_result)) {
    csp_concealbabyswallow_result <- data.frame(
      Date = zoo::index(csp_concealbabyswallow_result),
      as.data.frame(csp_concealbabyswallow_result)
    )
  }

  if (!"Date" %in% colnames(csp_concealbabyswallow_result))
    stop("csp_concealbabyswallow_result must contain a 'Date' column")

  required_cols <- c("ConcealBabySwallow")
  if (!all(required_cols %in% colnames(csp_concealbabyswallow_result)))
    stop(paste("csp_concealbabyswallow_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_concealbabyswallow_result$Date <- as.Date(csp_concealbabyswallow_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_concealbabyswallow_result, by = "Date", all.x = TRUE)

  concealing_baby_swallow_points <- merged_data[merged_data$ConcealBabySwallow == TRUE & !is.na(merged_data$ConcealBabySwallow), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(concealing_baby_swallow_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = concealing_baby_swallow_points,
        ggplot2::aes(x = Date, y = Low, color = "Concealing Baby Swallow"),
        size = point_size, shape = concealing_baby_swallow_shape, alpha = point_alpha
      )
  }

  if (nrow(concealing_baby_swallow_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Concealing Baby Swallow Patterns",
        values = c(      "Concealing Baby Swallow" = concealing_baby_swallow_color),
        breaks = c("Concealing Baby Swallow")
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

  eCandleSticks_result$csp_concealbabyswallow_data <- csp_concealbabyswallow_result
  return(eCandleSticks_result)
}
