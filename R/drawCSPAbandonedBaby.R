#' Draw CSP Abandoned Baby on Candlestick Chart
#'
#' Adds Abandoned Baby pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_abandonedbaby_result Result from \code{\link{addCSPAbandonedBaby}}
#' @param bullish_abandoned_baby_color Color for Bullish Abandoned Baby points. Default "green".
#' @param bullish_abandoned_baby_shape Shape for Bullish Abandoned Baby points. Default 24.
#' @param bearish_abandoned_baby_color Color for Bearish Abandoned Baby points. Default "red".
#' @param bearish_abandoned_baby_shape Shape for Bearish Abandoned Baby points. Default 25.
#' @param point_size Size for the pattern points. Default 3.
#' @param point_alpha Alpha transparency. Default 0.8.
#'
#' @return Modified eCandleSticks result list
#' @family pattern-3bar
#' @family pattern-bull
#' @family pattern-bear
#' @importFrom ggplot2 geom_point aes scale_color_manual guides guide_legend
#' @importFrom cowplot plot_grid
#' @importFrom xts is.xts
#' @importFrom zoo index
#' @export
drawCSPAbandonedBaby <- function(eCandleSticks_result, csp_abandonedbaby_result, bullish_abandoned_baby_color = "green", bullish_abandoned_baby_shape = 24, bearish_abandoned_baby_color = "red", bearish_abandoned_baby_shape = 25, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_abandonedbaby_result) && !xts::is.xts(csp_abandonedbaby_result))
    stop("csp_abandonedbaby_result must be a data frame or xts object")

  if (xts::is.xts(csp_abandonedbaby_result)) {
    csp_abandonedbaby_result <- data.frame(
      Date = zoo::index(csp_abandonedbaby_result),
      as.data.frame(csp_abandonedbaby_result)
    )
  }

  if (!"Date" %in% colnames(csp_abandonedbaby_result))
    stop("csp_abandonedbaby_result must contain a 'Date' column")

  required_cols <- c("BullAbandonedBaby", "BearAbandonedBaby")
  if (!all(required_cols %in% colnames(csp_abandonedbaby_result)))
    stop(paste("csp_abandonedbaby_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_abandonedbaby_result$Date <- as.Date(csp_abandonedbaby_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_abandonedbaby_result, by = "Date", all.x = TRUE)

  bullish_abandoned_baby_points <- merged_data[merged_data$BullAbandonedBaby == TRUE & !is.na(merged_data$BullAbandonedBaby), ]
  bearish_abandoned_baby_points <- merged_data[merged_data$BearAbandonedBaby == TRUE & !is.na(merged_data$BearAbandonedBaby), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(bullish_abandoned_baby_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bullish_abandoned_baby_points,
        ggplot2::aes(x = Date, y = Low, color = "Bullish Abandoned Baby"),
        size = point_size, shape = bullish_abandoned_baby_shape, alpha = point_alpha
      )
  }
  if (nrow(bearish_abandoned_baby_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bearish_abandoned_baby_points,
        ggplot2::aes(x = Date, y = High, color = "Bearish Abandoned Baby"),
        size = point_size, shape = bearish_abandoned_baby_shape, alpha = point_alpha
      )
  }

  if (nrow(bullish_abandoned_baby_points) > 0 || nrow(bearish_abandoned_baby_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Abandoned Baby Patterns",
        values = c(      "Bullish Abandoned Baby" = bullish_abandoned_baby_color,       "Bearish Abandoned Baby" = bearish_abandoned_baby_color),
        breaks = c("Bullish Abandoned Baby", "Bearish Abandoned Baby")
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

  eCandleSticks_result$csp_abandonedbaby_data <- csp_abandonedbaby_result
  return(eCandleSticks_result)
}
