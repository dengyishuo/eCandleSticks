#' Draw CSP Advance Block on Candlestick Chart
#'
#' Adds Advance Block pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_advanceblock_result Result from \code{\link{addCSPAdvanceBlock}}
#' @param advance_block_color Color for Advance Block points. Default "red".
#' @param advance_block_shape Shape for Advance Block points. Default 25.
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
drawCSPAdvanceBlock <- function(eCandleSticks_result, csp_advanceblock_result, advance_block_color = "red", advance_block_shape = 25, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_advanceblock_result) && !xts::is.xts(csp_advanceblock_result))
    stop("csp_advanceblock_result must be a data frame or xts object")

  if (xts::is.xts(csp_advanceblock_result)) {
    csp_advanceblock_result <- data.frame(
      Date = zoo::index(csp_advanceblock_result),
      as.data.frame(csp_advanceblock_result)
    )
  }

  if (!"Date" %in% colnames(csp_advanceblock_result))
    stop("csp_advanceblock_result must contain a 'Date' column")

  required_cols <- c("AdvanceBlock")
  if (!all(required_cols %in% colnames(csp_advanceblock_result)))
    stop(paste("csp_advanceblock_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_advanceblock_result$Date <- as.Date(csp_advanceblock_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_advanceblock_result, by = "Date", all.x = TRUE)

  advance_block_points <- merged_data[merged_data$AdvanceBlock == TRUE & !is.na(merged_data$AdvanceBlock), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(advance_block_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = advance_block_points,
        ggplot2::aes(x = Date, y = High, color = "Advance Block"),
        size = point_size, shape = advance_block_shape, alpha = point_alpha
      )
  }

  if (nrow(advance_block_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Advance Block Patterns",
        values = c(      "Advance Block" = advance_block_color),
        breaks = c("Advance Block")
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

  eCandleSticks_result$csp_advanceblock_data <- csp_advanceblock_result
  return(eCandleSticks_result)
}
