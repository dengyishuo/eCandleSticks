#' Add CSP Long Candle Body Points to Candlestick Chart
#'
#' This function adds CSP Long Candle Body points (Long White Candle Body and Long Black Candle Body)
#' to a candlestick chart created by eCandleSticks using the results from a CSP Long Candle Body analysis,
#' and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_long_candle_body_result The result object from CSP Long Candle Body analysis, which should be a data frame
#'        containing a 'Date' column and logical columns 'LongWhiteCandleBody' and 'LongBlackCandleBody'
#' @param long_white_candle_body_color Color for Long White Candle Body points. Default "green".
#' @param long_black_candle_body_color Color for Long Black Candle Body points. Default "red".
#' @param point_size Size for the Long Candle Body points. Default 3.
#' @param long_white_candle_body_shape Shape for Long White Candle Body points. Default 20 (small filled circle).
#' @param long_black_candle_body_shape Shape for Long Black Candle Body points. Default 20 (small filled circle).
#' @param point_alpha Alpha transparency for the Long Candle Body points. Default 0.8.
#' @param mark_at_body_center Whether to mark at the center of the candle body instead of Close price. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Long Candle Body points added to the price plot
#' and the combined plot updated accordingly.
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
#' # Get CSP Long Candle Body results
#' csp_long_candle_body_data <- CSPLongCandleBody(AAPL) # This returns a data frame with LongWhiteCandleBody and LongBlackCandleBody columns
#'
#' # Add Long Candle Body points
#' result_with_long_candle_body <- addCSPLongCandleBody(result, csp_long_candle_body_data)
#'
#' # Display the combined plot with Long Candle Body points
#' print(result_with_long_candle_body$combined_plot)
#' }
addCSPLongCandleBody <- function(eCandleSticks_result, csp_long_candle_body_result,
                                 long_white_candle_body_color = "green", long_black_candle_body_color = "red",
                                 point_size = 3, long_white_candle_body_shape = 20, long_black_candle_body_shape = 20,
                                 point_alpha = 0.8, mark_at_body_center = TRUE) {
  # Validate csp_long_candle_body_result
  if (!is.data.frame(csp_long_candle_body_result) && !xts::is.xts(csp_long_candle_body_result)) {
    stop("csp_long_candle_body_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_long_candle_body_result)) {
    csp_long_candle_body_result <- data.frame(
      Date = zoo::index(csp_long_candle_body_result),
      as.data.frame(csp_long_candle_body_result)
    )
  }

  if (!"Date" %in% colnames(csp_long_candle_body_result)) {
    stop("csp_long_candle_body_result must contain a 'Date' column")
  }

  required_cols <- c("LongWhiteCandleBody", "LongBlackCandleBody")
  if (!all(required_cols %in% colnames(csp_long_candle_body_result))) {
    stop("csp_long_candle_body_result must contain 'LongWhiteCandleBody' and 'LongBlackCandleBody' columns")
  }

  # Convert Date to proper format if needed
  csp_long_candle_body_result$Date <- as.Date(csp_long_candle_body_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_long_candle_body_result, by = "Date", all.x = TRUE)

  # Extract different types of Long Candle Body points
  long_white_candle_body_points <- merged_data[merged_data$LongWhiteCandleBody == TRUE & !is.na(merged_data$LongWhiteCandleBody), ]
  long_black_candle_body_points <- merged_data[merged_data$LongBlackCandleBody == TRUE & !is.na(merged_data$LongBlackCandleBody), ]

  # Determine y-value for marking
  if (mark_at_body_center) {
    # Mark at the center of the candle body
    long_white_candle_body_points$LongCandleBodyLevel <- (long_white_candle_body_points$Open + long_white_candle_body_points$Close) / 2
    long_black_candle_body_points$LongCandleBodyLevel <- (long_black_candle_body_points$Open + long_black_candle_body_points$Close) / 2
  } else {
    # Mark at Close price
    long_white_candle_body_points$LongCandleBodyLevel <- long_white_candle_body_points$Close
    long_black_candle_body_points$LongCandleBodyLevel <- long_black_candle_body_points$Close
  }

  # Add Long Candle Body points to the price plot
  price_plot_with_long_candle_body <- eCandleSticks_result$price_plot

  # Add Long White Candle Body points (if any)
  if (nrow(long_white_candle_body_points) > 0) {
    price_plot_with_long_candle_body <- price_plot_with_long_candle_body +
      ggplot2::geom_point(
        data = long_white_candle_body_points,
        aes(x = Date, y = LongCandleBodyLevel, color = "Long White Candle Body"),
        size = point_size,
        shape = long_white_candle_body_shape,
        alpha = point_alpha
      )
  }

  # Add Long Black Candle Body points (if any)
  if (nrow(long_black_candle_body_points) > 0) {
    price_plot_with_long_candle_body <- price_plot_with_long_candle_body +
      ggplot2::geom_point(
        data = long_black_candle_body_points,
        aes(x = Date, y = LongCandleBodyLevel, color = "Long Black Candle Body"),
        size = point_size,
        shape = long_black_candle_body_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Long Candle Body points
  if (nrow(long_white_candle_body_points) > 0 || nrow(long_black_candle_body_points) > 0) {
    price_plot_with_long_candle_body <- price_plot_with_long_candle_body +
      ggplot2::scale_color_manual(
        name = "CSP Long Candle Body Patterns",
        values = c(
          "Long White Candle Body" = long_white_candle_body_color,
          "Long Black Candle Body" = long_black_candle_body_color
        ),
        breaks = c("Long White Candle Body", "Long Black Candle Body")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(long_white_candle_body_shape, long_black_candle_body_shape),
            size = rep(point_size, 2),
            alpha = rep(point_alpha, 2)
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_long_candle_body

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_long_candle_body, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_long_candle_body
  }

  # Add csp_long_candle_body_result to the output for reference
  eCandleSticks_result$csp_long_candle_body_data <- csp_long_candle_body_result
  eCandleSticks_result$long_white_candle_body_points <- long_white_candle_body_points
  eCandleSticks_result$long_black_candle_body_points <- long_black_candle_body_points

  return(eCandleSticks_result)
}
