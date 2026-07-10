#' Add CSP Three Black Crows Points to Candlestick Chart
#'
#' This function adds CSP Three Black Crows points to a candlestick chart
#' created by eCandleSticks using the results from a CSP Three Black Crows analysis,
#' and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_three_black_crows_result The result object from CSP Three Black Crows analysis,
#'        which should be a data frame containing a 'Date' column and a logical column 'ThreeBlackCrows'
#' @param three_black_crows_color Color for Three Black Crows points. Default "darkred".
#' @param point_size Size for the Three Black Crows points. Default 3.
#' @param three_black_crows_shape Shape for Three Black Crows points. Default 4 (cross).
#' @param point_alpha Alpha transparency for the Three Black Crows points. Default 0.8.
#' @param mark_at_close Whether to mark at the close price. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Three Black Crows points added to the price plot
#' and the combined plot updated accordingly.
#' @family pattern-3bar
#' @family pattern-bear
#' @export
#' @importFrom ggplot2 geom_point
#' @importFrom cowplot plot_grid
#'
#' @examples
#' \dontrun{
#' library(quantmod)
#' getSymbols("AAPL", src = "yahoo", from = "2023-01-01", to = "2025-09-08")
#'
#' # Create candlestick chart
#' result <- eCandleSticks(AAPL)
#'
#' # Get CSP Three Black Crows results
#' csp_three_black_crows_data <- addCSPThreeBlackCrows(AAPL)
#'
#' # Add Three Black Crows points
#' result_with_three_black_crows <- drawCSPThreeBlackCrows(result, csp_three_black_crows_data)
#'
#' # Display the combined plot with Three Black Crows points
#' print(result_with_three_black_crows$combined_plot)
#' }
drawCSPThreeBlackCrows <- function(eCandleSticks_result, csp_three_black_crows_result,
                                  three_black_crows_color = "darkred", point_size = 3,
                                  three_black_crows_shape = 4, point_alpha = 0.8,
                                  mark_at_close = TRUE) {
  # Validate csp_three_black_crows_result
  if (!is.data.frame(csp_three_black_crows_result) && !xts::is.xts(csp_three_black_crows_result)) {
    stop("csp_three_black_crows_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_three_black_crows_result)) {
    csp_three_black_crows_result <- data.frame(
      Date = zoo::index(csp_three_black_crows_result),
      as.data.frame(csp_three_black_crows_result)
    )
  }

  if (!"Date" %in% colnames(csp_three_black_crows_result)) {
    stop("csp_three_black_crows_result must contain a 'Date' column")
  }

  if (!"ThreeBlackCrows" %in% colnames(csp_three_black_crows_result)) {
    stop("csp_three_black_crows_result must contain a 'ThreeBlackCrows' column")
  }

  # Convert Date to proper format if needed
  csp_three_black_crows_result$Date <- as.Date(csp_three_black_crows_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_three_black_crows_result, by = "Date", all.x = TRUE)

  # Extract Three Black Crows points
  three_black_crows_points <- merged_data[merged_data$ThreeBlackCrows == TRUE & !is.na(merged_data$ThreeBlackCrows), ]

  # Determine y-value for marking
  if (mark_at_close) {
    # Mark at Close price
    three_black_crows_points$ThreeBlackCrowsLevel <- three_black_crows_points$Close
  } else {
    # Mark at the midpoint of the candle
    three_black_crows_points$ThreeBlackCrowsLevel <- (three_black_crows_points$High + three_black_crows_points$Low) / 2
  }

  # Add Three Black Crows points to the price plot
  price_plot_with_three_black_crows <- eCandleSticks_result$price_plot

  # Add Three Black Crows points (if any)
  if (nrow(three_black_crows_points) > 0) {
    price_plot_with_three_black_crows <- price_plot_with_three_black_crows +
      ggplot2::geom_point(
        data = three_black_crows_points,
        aes(x = Date, y = ThreeBlackCrowsLevel, color = "Three Black Crows"),
        size = point_size,
        shape = three_black_crows_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Three Black Crows points
  if (nrow(three_black_crows_points) > 0) {
    price_plot_with_three_black_crows <- price_plot_with_three_black_crows +
      ggplot2::scale_color_manual(
        name = "CSP Patterns",
        values = c("Three Black Crows" = three_black_crows_color),
        breaks = c("Three Black Crows")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = three_black_crows_shape,
            size = point_size,
            alpha = point_alpha
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_three_black_crows

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_three_black_crows, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_three_black_crows
  }

  # Add csp_three_black_crows_result to the output for reference
  eCandleSticks_result$csp_three_black_crows_data <- csp_three_black_crows_result
  eCandleSticks_result$three_black_crows_points <- three_black_crows_points

  return(eCandleSticks_result)
}
