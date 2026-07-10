#' Draw CSP Gap Side-by-Side White on Candlestick Chart
#'
#' Adds Gap Side-by-Side White pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_gapsidesidewhite_result Result from \code{\link{addCSPGapSideSideWhite}}
#' @param bullish_gap_side_by_side_white_color Color for Bullish Gap Side-by-Side White points. Default "green".
#' @param bullish_gap_side_by_side_white_shape Shape for Bullish Gap Side-by-Side White points. Default 24.
#' @param bearish_gap_side_by_side_white_color Color for Bearish Gap Side-by-Side White points. Default "red".
#' @param bearish_gap_side_by_side_white_shape Shape for Bearish Gap Side-by-Side White points. Default 25.
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
drawCSPGapSideSideWhite <- function(eCandleSticks_result, csp_gapsidesidewhite_result, bullish_gap_side_by_side_white_color = "green", bullish_gap_side_by_side_white_shape = 24, bearish_gap_side_by_side_white_color = "red", bearish_gap_side_by_side_white_shape = 25, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_gapsidesidewhite_result) && !xts::is.xts(csp_gapsidesidewhite_result))
    stop("csp_gapsidesidewhite_result must be a data frame or xts object")

  if (xts::is.xts(csp_gapsidesidewhite_result)) {
    csp_gapsidesidewhite_result <- data.frame(
      Date = zoo::index(csp_gapsidesidewhite_result),
      as.data.frame(csp_gapsidesidewhite_result)
    )
  }

  if (!"Date" %in% colnames(csp_gapsidesidewhite_result))
    stop("csp_gapsidesidewhite_result must contain a 'Date' column")

  required_cols <- c("BullGapSideSideWhite", "BearGapSideSideWhite")
  if (!all(required_cols %in% colnames(csp_gapsidesidewhite_result)))
    stop(paste("csp_gapsidesidewhite_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_gapsidesidewhite_result$Date <- as.Date(csp_gapsidesidewhite_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_gapsidesidewhite_result, by = "Date", all.x = TRUE)

  bullish_gap_side_by_side_white_points <- merged_data[merged_data$BullGapSideSideWhite == TRUE & !is.na(merged_data$BullGapSideSideWhite), ]
  bearish_gap_side_by_side_white_points <- merged_data[merged_data$BearGapSideSideWhite == TRUE & !is.na(merged_data$BearGapSideSideWhite), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(bullish_gap_side_by_side_white_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bullish_gap_side_by_side_white_points,
        ggplot2::aes(x = Date, y = Low, color = "Bullish Gap Side-by-Side White"),
        size = point_size, shape = bullish_gap_side_by_side_white_shape, alpha = point_alpha
      )
  }
  if (nrow(bearish_gap_side_by_side_white_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bearish_gap_side_by_side_white_points,
        ggplot2::aes(x = Date, y = High, color = "Bearish Gap Side-by-Side White"),
        size = point_size, shape = bearish_gap_side_by_side_white_shape, alpha = point_alpha
      )
  }

  if (nrow(bullish_gap_side_by_side_white_points) > 0 || nrow(bearish_gap_side_by_side_white_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Gap Side-by-Side White Patterns",
        values = c(      "Bullish Gap Side-by-Side White" = bullish_gap_side_by_side_white_color,       "Bearish Gap Side-by-Side White" = bearish_gap_side_by_side_white_color),
        breaks = c("Bullish Gap Side-by-Side White", "Bearish Gap Side-by-Side White")
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

  eCandleSticks_result$csp_gapsidesidewhite_data <- csp_gapsidesidewhite_result
  return(eCandleSticks_result)
}
