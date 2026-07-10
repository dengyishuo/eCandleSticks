#' Draw CSP Stick Sandwich on Candlestick Chart
#'
#' Adds Stick Sandwich pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_sticksandwich_result Result from \code{\link{addCSPStickSandwich}}
#' @param stick_sandwich_color Color for Stick Sandwich points. Default "green".
#' @param stick_sandwich_shape Shape for Stick Sandwich points. Default 24.
#' @param point_size Size for the pattern points. Default 3.
#' @param point_alpha Alpha transparency. Default 0.8.
#'
#' @return Modified eCandleSticks result list
#' @family pattern-3bar
#' @family pattern-bull
#' @importFrom ggplot2 geom_point aes scale_color_manual guides guide_legend
#' @importFrom cowplot plot_grid
#' @importFrom xts is.xts
#' @importFrom zoo index
#' @export
drawCSPStickSandwich <- function(eCandleSticks_result, csp_sticksandwich_result, stick_sandwich_color = "green", stick_sandwich_shape = 24, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_sticksandwich_result) && !xts::is.xts(csp_sticksandwich_result))
    stop("csp_sticksandwich_result must be a data frame or xts object")

  if (xts::is.xts(csp_sticksandwich_result)) {
    csp_sticksandwich_result <- data.frame(
      Date = zoo::index(csp_sticksandwich_result),
      as.data.frame(csp_sticksandwich_result)
    )
  }

  if (!"Date" %in% colnames(csp_sticksandwich_result))
    stop("csp_sticksandwich_result must contain a 'Date' column")

  required_cols <- c("StickSandwich")
  if (!all(required_cols %in% colnames(csp_sticksandwich_result)))
    stop(paste("csp_sticksandwich_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_sticksandwich_result$Date <- as.Date(csp_sticksandwich_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_sticksandwich_result, by = "Date", all.x = TRUE)

  stick_sandwich_points <- merged_data[merged_data$StickSandwich == TRUE & !is.na(merged_data$StickSandwich), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(stick_sandwich_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = stick_sandwich_points,
        ggplot2::aes(x = Date, y = Low, color = "Stick Sandwich"),
        size = point_size, shape = stick_sandwich_shape, alpha = point_alpha
      )
  }

  if (nrow(stick_sandwich_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Stick Sandwich Patterns",
        values = c(      "Stick Sandwich" = stick_sandwich_color),
        breaks = c("Stick Sandwich")
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

  eCandleSticks_result$csp_sticksandwich_data <- csp_sticksandwich_result
  return(eCandleSticks_result)
}
