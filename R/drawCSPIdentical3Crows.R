#' Draw CSP Identical Three Crows on Candlestick Chart
#'
#' Adds Identical Three Crows pattern markers to a candlestick chart.
#'
#' @param eCandleSticks_result Result from \code{\link{eCandleSticks}}
#' @param csp_identical3crows_result Result from \code{\link{addCSPIdentical3Crows}}
#' @param identical_three_crows_color Color for Identical Three Crows points. Default "red".
#' @param identical_three_crows_shape Shape for Identical Three Crows points. Default 25.
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
drawCSPIdentical3Crows <- function(eCandleSticks_result, csp_identical3crows_result, identical_three_crows_color = "red", identical_three_crows_shape = 25, point_size = 3, point_alpha = 0.8) {
  if (!is.data.frame(csp_identical3crows_result) && !xts::is.xts(csp_identical3crows_result))
    stop("csp_identical3crows_result must be a data frame or xts object")

  if (xts::is.xts(csp_identical3crows_result)) {
    csp_identical3crows_result <- data.frame(
      Date = zoo::index(csp_identical3crows_result),
      as.data.frame(csp_identical3crows_result)
    )
  }

  if (!"Date" %in% colnames(csp_identical3crows_result))
    stop("csp_identical3crows_result must contain a 'Date' column")

  required_cols <- c("Identical3Crows")
  if (!all(required_cols %in% colnames(csp_identical3crows_result)))
    stop(paste("csp_identical3crows_result must contain columns:", paste(required_cols, collapse = ", ")))

  csp_identical3crows_result$Date <- as.Date(csp_identical3crows_result$Date)
  merged_data <- merge(eCandleSticks_result$data, csp_identical3crows_result, by = "Date", all.x = TRUE)

  identical_three_crows_points <- merged_data[merged_data$Identical3Crows == TRUE & !is.na(merged_data$Identical3Crows), ]

  price_plot <- eCandleSticks_result$price_plot

  if (nrow(identical_three_crows_points) > 0) {
    price_plot <- price_plot +
      ggplot2::geom_point(
        data = identical_three_crows_points,
        ggplot2::aes(x = Date, y = High, color = "Identical Three Crows"),
        size = point_size, shape = identical_three_crows_shape, alpha = point_alpha
      )
  }

  if (nrow(identical_three_crows_points) > 0) {
    price_plot <- price_plot +
      ggplot2::scale_color_manual(
        name = "CSP Identical Three Crows Patterns",
        values = c(      "Identical Three Crows" = identical_three_crows_color),
        breaks = c("Identical Three Crows")
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

  eCandleSticks_result$csp_identical3crows_data <- csp_identical3crows_result
  return(eCandleSticks_result)
}
