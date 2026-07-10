#' Draw CSP Two Crows on Candlestick Chart
#'
#' Adds Two Crows pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_twocrows_result Result from \code{\link{addCSPTwoCrows}}
#' @param two_crows_color Color for Two Crows points. Default "red".
#' @param two_crows_shape Shape for Two Crows points. Default 25.
#' @param point_size Size for the pattern points. Default 3.
#' @param point_alpha Alpha transparency. Default 0.8.
#'
#' @return Modified eCandleSticks result list
#' @family pattern-3bar
#' @family pattern-bear
#' @importFrom ggplot2 geom_point aes scale_color_manual guides guide_legend
#' @importFrom cowplot plot_grid
#' @importFrom xts is.xts
#' @importFrom zoo index
#' @export
drawCSPTwoCrows <- function(eCandleSticks_result, csp_twocrows_result, two_crows_color = "red", two_crows_shape = 25, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_twocrows_result) && !xts::is.xts(csp_twocrows_result))
    stop("csp_twocrows_result must be a data frame or xts object")

  if (xts::is.xts(csp_twocrows_result)) {
    csp_twocrows_result <- data.frame(
      Date = zoo::index(csp_twocrows_result),
      as.data.frame(csp_twocrows_result)
    )
  }

  if (!"Date" %in% colnames(csp_twocrows_result))
    stop("csp_twocrows_result must contain a 'Date' column")

  required_cols <- c("TwoCrows")
  if (!all(required_cols %in% colnames(csp_twocrows_result)))
    stop(paste("csp_twocrows_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_twocrows_result$Date <- as.Date(csp_twocrows_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_twocrows_result, by = "Date", all.x = TRUE)

  two_crows_points <- merged_data[merged_data$TwoCrows == TRUE & !is.na(merged_data$TwoCrows), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(two_crows_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = two_crows_points,
        ggplot2::aes(x = Date, y = High, color = "Two Crows"),
        size = point_size, shape = two_crows_shape, alpha = point_alpha
      )
  }

  if (nrow(two_crows_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Two Crows Patterns",
        values = c(      "Two Crows" = two_crows_color),
        breaks = c("Two Crows")
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

  eCandleSticks_result$csp_twocrows_data <- csp_twocrows_result
  return(eCandleSticks_result)
}
