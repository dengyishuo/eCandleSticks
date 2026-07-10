#' Draw CSP Doji Star on Candlestick Chart
#'
#' Adds Doji Star pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_dojistar_result Result from \code{\link{addCSPDojiStar}}
#' @param bullish_doji_star_color Color for Bullish Doji Star points. Default "green".
#' @param bullish_doji_star_shape Shape for Bullish Doji Star points. Default 24.
#' @param bearish_doji_star_color Color for Bearish Doji Star points. Default "red".
#' @param bearish_doji_star_shape Shape for Bearish Doji Star points. Default 25.
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
drawCSPDojiStar <- function(eCandleSticks_result, csp_dojistar_result, bullish_doji_star_color = "green", bullish_doji_star_shape = 24, bearish_doji_star_color = "red", bearish_doji_star_shape = 25, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_dojistar_result) && !xts::is.xts(csp_dojistar_result))
    stop("csp_dojistar_result must be a data frame or xts object")

  if (xts::is.xts(csp_dojistar_result)) {
    csp_dojistar_result <- data.frame(
      Date = zoo::index(csp_dojistar_result),
      as.data.frame(csp_dojistar_result)
    )
  }

  if (!"Date" %in% colnames(csp_dojistar_result))
    stop("csp_dojistar_result must contain a 'Date' column")

  required_cols <- c("BullDojiStar", "BearDojiStar")
  if (!all(required_cols %in% colnames(csp_dojistar_result)))
    stop(paste("csp_dojistar_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_dojistar_result$Date <- as.Date(csp_dojistar_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_dojistar_result, by = "Date", all.x = TRUE)

  bullish_doji_star_points <- merged_data[merged_data$BullDojiStar == TRUE & !is.na(merged_data$BullDojiStar), ]
  bearish_doji_star_points <- merged_data[merged_data$BearDojiStar == TRUE & !is.na(merged_data$BearDojiStar), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(bullish_doji_star_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bullish_doji_star_points,
        ggplot2::aes(x = Date, y = Low, color = "Bullish Doji Star"),
        size = point_size, shape = bullish_doji_star_shape, alpha = point_alpha
      )
  }
  if (nrow(bearish_doji_star_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = bearish_doji_star_points,
        ggplot2::aes(x = Date, y = High, color = "Bearish Doji Star"),
        size = point_size, shape = bearish_doji_star_shape, alpha = point_alpha
      )
  }

  if (nrow(bullish_doji_star_points) > 0 || nrow(bearish_doji_star_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Doji Star Patterns",
        values = c(      "Bullish Doji Star" = bullish_doji_star_color,       "Bearish Doji Star" = bearish_doji_star_color),
        breaks = c("Bullish Doji Star", "Bearish Doji Star")
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

  eCandleSticks_result$csp_dojistar_data <- csp_dojistar_result
  return(eCandleSticks_result)
}
