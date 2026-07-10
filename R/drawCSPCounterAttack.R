#' Draw CSP Counter Attack on Candlestick Chart
#'
#' Adds Counter Attack pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_counterattack_result Result from \code{\link{addCSPCounterAttack}}
#' @param bullish_counter_attack_color Color for Bullish Counter Attack points. Default "green".
#' @param bullish_counter_attack_shape Shape for Bullish Counter Attack points. Default 24.
#' @param bearish_counter_attack_color Color for Bearish Counter Attack points. Default "red".
#' @param bearish_counter_attack_shape Shape for Bearish Counter Attack points. Default 25.
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
drawCSPCounterAttack <- function(eCandleSticks_result, csp_counterattack_result, bullish_counter_attack_color = "green", bullish_counter_attack_shape = 24, bearish_counter_attack_color = "red", bearish_counter_attack_shape = 25, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_counterattack_result) && !xts::is.xts(csp_counterattack_result))
    stop("csp_counterattack_result must be a data frame or xts object")

  if (xts::is.xts(csp_counterattack_result)) {
    csp_counterattack_result <- data.frame(
      Date = zoo::index(csp_counterattack_result),
      as.data.frame(csp_counterattack_result)
    )
  }

  if (!"Date" %in% colnames(csp_counterattack_result))
    stop("csp_counterattack_result must contain a 'Date' column")

  required_cols <- c("BullCounterAttack", "BearCounterAttack")
  if (!all(required_cols %in% colnames(csp_counterattack_result)))
    stop(paste("csp_counterattack_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_counterattack_result$Date <- as.Date(csp_counterattack_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_counterattack_result, by = "Date", all.x = TRUE)

  bullish_counter_attack_points <- merged_data[merged_data$BullCounterAttack == TRUE & !is.na(merged_data$BullCounterAttack), ]
  bearish_counter_attack_points <- merged_data[merged_data$BearCounterAttack == TRUE & !is.na(merged_data$BearCounterAttack), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(bullish_counter_attack_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bullish_counter_attack_points,
        ggplot2::aes(x = Date, y = Low, color = "Bullish Counter Attack"),
        size = point_size, shape = bullish_counter_attack_shape, alpha = point_alpha
      )
  }
  if (nrow(bearish_counter_attack_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bearish_counter_attack_points,
        ggplot2::aes(x = Date, y = High, color = "Bearish Counter Attack"),
        size = point_size, shape = bearish_counter_attack_shape, alpha = point_alpha
      )
  }

  if (nrow(bullish_counter_attack_points) > 0 || nrow(bearish_counter_attack_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Counter Attack Patterns",
        values = c(      "Bullish Counter Attack" = bullish_counter_attack_color,       "Bearish Counter Attack" = bearish_counter_attack_color),
        breaks = c("Bullish Counter Attack", "Bearish Counter Attack")
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

  eCandleSticks_result$csp_counterattack_data <- csp_counterattack_result
  return(eCandleSticks_result)
}
