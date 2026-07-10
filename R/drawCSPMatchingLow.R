#' Draw CSP Matching Low on Candlestick Chart
#'
#' Adds Matching Low pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_matchinglow_result Result from \code{\link{addCSPMatchingLow}}
#' @param matching_low_color Color for Matching Low points. Default "green".
#' @param matching_low_shape Shape for Matching Low points. Default 24.
#' @param point_size Size for the pattern points. Default 3.
#' @param point_alpha Alpha transparency. Default 0.8.
#'
#' @return Modified eCandleSticks result list
#' @family pattern-2bar
#' @family pattern-bull
#' @importFrom ggplot2 geom_point aes scale_color_manual guides guide_legend
#' @importFrom cowplot plot_grid
#' @importFrom xts is.xts
#' @importFrom zoo index
#' @export
drawCSPMatchingLow <- function(eCandleSticks_result, csp_matchinglow_result, matching_low_color = "green", matching_low_shape = 24, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_matchinglow_result) && !xts::is.xts(csp_matchinglow_result))
    stop("csp_matchinglow_result must be a data frame or xts object")

  if (xts::is.xts(csp_matchinglow_result)) {
    csp_matchinglow_result <- data.frame(
      Date = zoo::index(csp_matchinglow_result),
      as.data.frame(csp_matchinglow_result)
    )
  }

  if (!"Date" %in% colnames(csp_matchinglow_result))
    stop("csp_matchinglow_result must contain a 'Date' column")

  required_cols <- c("MatchingLow")
  if (!all(required_cols %in% colnames(csp_matchinglow_result)))
    stop(paste("csp_matchinglow_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_matchinglow_result$Date <- as.Date(csp_matchinglow_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_matchinglow_result, by = "Date", all.x = TRUE)

  matching_low_points <- merged_data[merged_data$MatchingLow == TRUE & !is.na(merged_data$MatchingLow), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(matching_low_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = matching_low_points,
        ggplot2::aes(x = Date, y = Low, color = "Matching Low"),
        size = point_size, shape = matching_low_shape, alpha = point_alpha
      )
  }

  if (nrow(matching_low_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Matching Low Patterns",
        values = c(      "Matching Low" = matching_low_color),
        breaks = c("Matching Low")
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

  eCandleSticks_result$csp_matchinglow_data <- csp_matchinglow_result
  return(eCandleSticks_result)
}
