#' Draw CSP Belt-Hold on Candlestick Chart
#'
#' Adds Belt-Hold pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_belthold_result Result from \code{\link{addCSPBeltHold}}
#' @param bullish_belt_hold_color Color for Bullish Belt-Hold points. Default "green".
#' @param bullish_belt_hold_shape Shape for Bullish Belt-Hold points. Default 24.
#' @param bearish_belt_hold_color Color for Bearish Belt-Hold points. Default "red".
#' @param bearish_belt_hold_shape Shape for Bearish Belt-Hold points. Default 25.
#' @param point_size Size for the pattern points. Default 3.
#' @param point_alpha Alpha transparency. Default 0.8.
#'
#' @return Modified eCandleSticks result list
#' @family pattern-1bar
#' @family pattern-bull
#' @family pattern-bear
#' @importFrom ggplot2 geom_point aes scale_color_manual guides guide_legend
#' @importFrom cowplot plot_grid
#' @importFrom xts is.xts
#' @importFrom zoo index
#' @export
drawCSPBeltHold <- function(eCandleSticks_result, csp_belthold_result, bullish_belt_hold_color = "green", bullish_belt_hold_shape = 24, bearish_belt_hold_color = "red", bearish_belt_hold_shape = 25, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_belthold_result) && !xts::is.xts(csp_belthold_result))
    stop("csp_belthold_result must be a data frame or xts object")

  if (xts::is.xts(csp_belthold_result)) {
    csp_belthold_result <- data.frame(
      Date = zoo::index(csp_belthold_result),
      as.data.frame(csp_belthold_result)
    )
  }

  if (!"Date" %in% colnames(csp_belthold_result))
    stop("csp_belthold_result must contain a 'Date' column")

  required_cols <- c("BullBeltHold", "BearBeltHold")
  if (!all(required_cols %in% colnames(csp_belthold_result)))
    stop(paste("csp_belthold_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_belthold_result$Date <- as.Date(csp_belthold_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_belthold_result, by = "Date", all.x = TRUE)

  bullish_belt_hold_points <- merged_data[merged_data$BullBeltHold == TRUE & !is.na(merged_data$BullBeltHold), ]
  bearish_belt_hold_points <- merged_data[merged_data$BearBeltHold == TRUE & !is.na(merged_data$BearBeltHold), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(bullish_belt_hold_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bullish_belt_hold_points,
        ggplot2::aes(x = Date, y = Low, color = "Bullish Belt-Hold"),
        size = point_size, shape = bullish_belt_hold_shape, alpha = point_alpha
      )
  }
  if (nrow(bearish_belt_hold_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bearish_belt_hold_points,
        ggplot2::aes(x = Date, y = High, color = "Bearish Belt-Hold"),
        size = point_size, shape = bearish_belt_hold_shape, alpha = point_alpha
      )
  }

  if (nrow(bullish_belt_hold_points) > 0 || nrow(bearish_belt_hold_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Belt-Hold Patterns",
        values = c(      "Bullish Belt-Hold" = bullish_belt_hold_color,       "Bearish Belt-Hold" = bearish_belt_hold_color),
        breaks = c("Bullish Belt-Hold", "Bearish Belt-Hold")
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

  eCandleSticks_result$csp_belthold_data <- csp_belthold_result
  return(eCandleSticks_result)
}
