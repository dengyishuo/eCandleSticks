#' Draw CSP Three Stars In South on Candlestick Chart
#'
#' Adds Three Stars In South pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_threestarsinsouth_result Result from \code{\link{addCSPThreeStarsInSouth}}
#' @param three_stars_in_south_color Color for Three Stars In South points. Default "green".
#' @param three_stars_in_south_shape Shape for Three Stars In South points. Default 24.
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
drawCSPThreeStarsInSouth <- function(eCandleSticks_result, csp_threestarsinsouth_result, three_stars_in_south_color = "green", three_stars_in_south_shape = 24, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_threestarsinsouth_result) && !xts::is.xts(csp_threestarsinsouth_result))
    stop("csp_threestarsinsouth_result must be a data frame or xts object")

  if (xts::is.xts(csp_threestarsinsouth_result)) {
    csp_threestarsinsouth_result <- data.frame(
      Date = zoo::index(csp_threestarsinsouth_result),
      as.data.frame(csp_threestarsinsouth_result)
    )
  }

  if (!"Date" %in% colnames(csp_threestarsinsouth_result))
    stop("csp_threestarsinsouth_result must contain a 'Date' column")

  required_cols <- c("ThreeStarsInSouth")
  if (!all(required_cols %in% colnames(csp_threestarsinsouth_result)))
    stop(paste("csp_threestarsinsouth_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_threestarsinsouth_result$Date <- as.Date(csp_threestarsinsouth_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_threestarsinsouth_result, by = "Date", all.x = TRUE)

  three_stars_in_south_points <- merged_data[merged_data$ThreeStarsInSouth == TRUE & !is.na(merged_data$ThreeStarsInSouth), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(three_stars_in_south_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = three_stars_in_south_points,
        ggplot2::aes(x = Date, y = Low, color = "Three Stars In South"),
        size = point_size, shape = three_stars_in_south_shape, alpha = point_alpha
      )
  }

  if (nrow(three_stars_in_south_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Three Stars In South Patterns",
        values = c(      "Three Stars In South" = three_stars_in_south_color),
        breaks = c("Three Stars In South")
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

  eCandleSticks_result$csp_threestarsinsouth_data <- csp_threestarsinsouth_result
  return(eCandleSticks_result)
}
