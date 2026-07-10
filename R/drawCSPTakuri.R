#' Draw CSP Takuri on Candlestick Chart
#'
#' Adds Takuri pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_takuri_result Result from \code{\link{addCSPTakuri}}
#' @param takuri_color Color for Takuri points. Default "green".
#' @param takuri_shape Shape for Takuri points. Default 24.
#' @param point_size Size for the pattern points. Default 3.
#' @param point_alpha Alpha transparency. Default 0.8.
#'
#' @return Modified eCandleSticks result list
#' @family pattern-1bar
#' @family pattern-bull
#' @importFrom ggplot2 geom_point aes scale_color_manual guides guide_legend
#' @importFrom cowplot plot_grid
#' @importFrom xts is.xts
#' @importFrom zoo index
#' @export
drawCSPTakuri <- function(eCandleSticks_result, csp_takuri_result, takuri_color = "green", takuri_shape = 24, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_takuri_result) && !xts::is.xts(csp_takuri_result))
    stop("csp_takuri_result must be a data frame or xts object")

  if (xts::is.xts(csp_takuri_result)) {
    csp_takuri_result <- data.frame(
      Date = zoo::index(csp_takuri_result),
      as.data.frame(csp_takuri_result)
    )
  }

  if (!"Date" %in% colnames(csp_takuri_result))
    stop("csp_takuri_result must contain a 'Date' column")

  required_cols <- c("Takuri")
  if (!all(required_cols %in% colnames(csp_takuri_result)))
    stop(paste("csp_takuri_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_takuri_result$Date <- as.Date(csp_takuri_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_takuri_result, by = "Date", all.x = TRUE)

  takuri_points <- merged_data[merged_data$Takuri == TRUE & !is.na(merged_data$Takuri), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(takuri_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = takuri_points,
        ggplot2::aes(x = Date, y = Low, color = "Takuri"),
        size = point_size, shape = takuri_shape, alpha = point_alpha
      )
  }

  if (nrow(takuri_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Takuri Patterns",
        values = c(      "Takuri" = takuri_color),
        breaks = c("Takuri")
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

  eCandleSticks_result$csp_takuri_data <- csp_takuri_result
  return(eCandleSticks_result)
}
