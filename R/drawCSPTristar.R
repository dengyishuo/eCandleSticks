#' Draw CSP Tristar on Candlestick Chart
#'
#' Adds Tristar pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_tristar_result Result from \code{\link{addCSPTristar}}
#' @param bullish_tristar_color Color for Bullish Tristar points. Default "green".
#' @param bullish_tristar_shape Shape for Bullish Tristar points. Default 24.
#' @param bearish_tristar_color Color for Bearish Tristar points. Default "red".
#' @param bearish_tristar_shape Shape for Bearish Tristar points. Default 25.
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
drawCSPTristar <- function(eCandleSticks_result, csp_tristar_result, bullish_tristar_color = "green", bullish_tristar_shape = 24, bearish_tristar_color = "red", bearish_tristar_shape = 25, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_tristar_result) && !xts::is.xts(csp_tristar_result))
    stop("csp_tristar_result must be a data frame or xts object")

  if (xts::is.xts(csp_tristar_result)) {
    csp_tristar_result <- data.frame(
      Date = zoo::index(csp_tristar_result),
      as.data.frame(csp_tristar_result)
    )
  }

  if (!"Date" %in% colnames(csp_tristar_result))
    stop("csp_tristar_result must contain a 'Date' column")

  required_cols <- c("BullTristar", "BearTristar")
  if (!all(required_cols %in% colnames(csp_tristar_result)))
    stop(paste("csp_tristar_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_tristar_result$Date <- as.Date(csp_tristar_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_tristar_result, by = "Date", all.x = TRUE)

  bullish_tristar_points <- merged_data[merged_data$BullTristar == TRUE & !is.na(merged_data$BullTristar), ]
  bearish_tristar_points <- merged_data[merged_data$BearTristar == TRUE & !is.na(merged_data$BearTristar), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(bullish_tristar_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bullish_tristar_points,
        ggplot2::aes(x = Date, y = Low, color = "Bullish Tristar"),
        size = point_size, shape = bullish_tristar_shape, alpha = point_alpha
      )
  }
  if (nrow(bearish_tristar_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bearish_tristar_points,
        ggplot2::aes(x = Date, y = High, color = "Bearish Tristar"),
        size = point_size, shape = bearish_tristar_shape, alpha = point_alpha
      )
  }

  if (nrow(bullish_tristar_points) > 0 || nrow(bearish_tristar_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Tristar Patterns",
        values = c(      "Bullish Tristar" = bullish_tristar_color,       "Bearish Tristar" = bearish_tristar_color),
        breaks = c("Bullish Tristar", "Bearish Tristar")
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

  eCandleSticks_result$csp_tristar_data <- csp_tristar_result
  return(eCandleSticks_result)
}
