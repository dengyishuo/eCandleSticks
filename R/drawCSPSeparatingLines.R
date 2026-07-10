#' Draw CSP Separating Lines on Candlestick Chart
#'
#' Adds Separating Lines pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_separatinglines_result Result from \code{\link{addCSPSeparatingLines}}
#' @param bullish_separating_lines_color Color for Bullish Separating Lines points. Default "green".
#' @param bullish_separating_lines_shape Shape for Bullish Separating Lines points. Default 24.
#' @param bearish_separating_lines_color Color for Bearish Separating Lines points. Default "red".
#' @param bearish_separating_lines_shape Shape for Bearish Separating Lines points. Default 25.
#' @param point_size Size for the pattern points. Default 3.
#' @param point_alpha Alpha transparency. Default 0.8.
#'
#' @return Modified eCandleSticks result list
#' @family pattern-2bar
#' @family pattern-bull
#' @family pattern-bear
#' @importFrom ggplot2 geom_point aes scale_color_manual guides guide_legend
#' @importFrom cowplot plot_grid
#' @importFrom xts is.xts
#' @importFrom zoo index
#' @export
drawCSPSeparatingLines <- function(eCandleSticks_result, csp_separatinglines_result, bullish_separating_lines_color = "green", bullish_separating_lines_shape = 24, bearish_separating_lines_color = "red", bearish_separating_lines_shape = 25, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_separatinglines_result) && !xts::is.xts(csp_separatinglines_result))
    stop("csp_separatinglines_result must be a data frame or xts object")

  if (xts::is.xts(csp_separatinglines_result)) {
    csp_separatinglines_result <- data.frame(
      Date = zoo::index(csp_separatinglines_result),
      as.data.frame(csp_separatinglines_result)
    )
  }

  if (!"Date" %in% colnames(csp_separatinglines_result))
    stop("csp_separatinglines_result must contain a 'Date' column")

  required_cols <- c("BullSeparatingLines", "BearSeparatingLines")
  if (!all(required_cols %in% colnames(csp_separatinglines_result)))
    stop(paste("csp_separatinglines_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_separatinglines_result$Date <- as.Date(csp_separatinglines_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_separatinglines_result, by = "Date", all.x = TRUE)

  bullish_separating_lines_points <- merged_data[merged_data$BullSeparatingLines == TRUE & !is.na(merged_data$BullSeparatingLines), ]
  bearish_separating_lines_points <- merged_data[merged_data$BearSeparatingLines == TRUE & !is.na(merged_data$BearSeparatingLines), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(bullish_separating_lines_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bullish_separating_lines_points,
        ggplot2::aes(x = Date, y = Low, color = "Bullish Separating Lines"),
        size = point_size, shape = bullish_separating_lines_shape, alpha = point_alpha
      )
  }
  if (nrow(bearish_separating_lines_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bearish_separating_lines_points,
        ggplot2::aes(x = Date, y = High, color = "Bearish Separating Lines"),
        size = point_size, shape = bearish_separating_lines_shape, alpha = point_alpha
      )
  }

  if (nrow(bullish_separating_lines_points) > 0 || nrow(bearish_separating_lines_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Separating Lines Patterns",
        values = c(      "Bullish Separating Lines" = bullish_separating_lines_color,       "Bearish Separating Lines" = bearish_separating_lines_color),
        breaks = c("Bullish Separating Lines", "Bearish Separating Lines")
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

  eCandleSticks_result$csp_separatinglines_data <- csp_separatinglines_result
  return(eCandleSticks_result)
}
