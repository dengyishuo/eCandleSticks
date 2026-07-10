#' Draw CSP Ladder Bottom on Candlestick Chart
#'
#' Adds Ladder Bottom pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_ladderbottom_result Result from \code{\link{addCSPLadderBottom}}
#' @param ladder_bottom_color Color for Ladder Bottom points. Default "green".
#' @param ladder_bottom_shape Shape for Ladder Bottom points. Default 24.
#' @param point_size Size for the pattern points. Default 3.
#' @param point_alpha Alpha transparency. Default 0.8.
#'
#' @return Modified eCandleSticks result list
#' @family pattern-5bar
#' @family pattern-bull
#' @importFrom ggplot2 geom_point aes scale_color_manual guides guide_legend
#' @importFrom cowplot plot_grid
#' @importFrom xts is.xts
#' @importFrom zoo index
#' @export
drawCSPLadderBottom <- function(eCandleSticks_result, csp_ladderbottom_result, ladder_bottom_color = "green", ladder_bottom_shape = 24, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_ladderbottom_result) && !xts::is.xts(csp_ladderbottom_result))
    stop("csp_ladderbottom_result must be a data frame or xts object")

  if (xts::is.xts(csp_ladderbottom_result)) {
    csp_ladderbottom_result <- data.frame(
      Date = zoo::index(csp_ladderbottom_result),
      as.data.frame(csp_ladderbottom_result)
    )
  }

  if (!"Date" %in% colnames(csp_ladderbottom_result))
    stop("csp_ladderbottom_result must contain a 'Date' column")

  required_cols <- c("LadderBottom")
  if (!all(required_cols %in% colnames(csp_ladderbottom_result)))
    stop(paste("csp_ladderbottom_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_ladderbottom_result$Date <- as.Date(csp_ladderbottom_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_ladderbottom_result, by = "Date", all.x = TRUE)

  ladder_bottom_points <- merged_data[merged_data$LadderBottom == TRUE & !is.na(merged_data$LadderBottom), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(ladder_bottom_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = ladder_bottom_points,
        ggplot2::aes(x = Date, y = Low, color = "Ladder Bottom"),
        size = point_size, shape = ladder_bottom_shape, alpha = point_alpha
      )
  }

  if (nrow(ladder_bottom_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Ladder Bottom Patterns",
        values = c(      "Ladder Bottom" = ladder_bottom_color),
        breaks = c("Ladder Bottom")
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

  eCandleSticks_result$csp_ladderbottom_data <- csp_ladderbottom_result
  return(eCandleSticks_result)
}
