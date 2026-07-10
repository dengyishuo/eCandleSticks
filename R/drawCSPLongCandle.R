#' Add CSP Long Candle Points to Candlestick Chart
#'
#' This function adds CSP Long Candle points (Long White Candle and Long Black Candle)
#' to a candlestick chart created by eCandleSticks using the results from a CSP Long Candle analysis,
#' and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_long_candle_result The result object from CSP Long Candle analysis, which should be a data frame
#'        containing a 'Date' column and logical columns 'LongWhiteCandle' and 'LongBlackCandle'
#' @param long_white_candle_color Color for Long White Candle points. Default "green".
#' @param long_black_candle_color Color for Long Black Candle points. Default "red".
#' @param point_size Size for the Long Candle points. Default 3.
#' @param long_white_candle_shape Shape for Long White Candle points. Default 19 (solid circle).
#' @param long_black_candle_shape Shape for Long Black Candle points. Default 19 (solid circle).
#' @param point_alpha Alpha transparency for the Long Candle points. Default 0.8.
#' @param mark_at_close Whether to mark at the close price. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Long Candle points added to the price plot
#' and the combined plot updated accordingly.
#' @family pattern-1bar
#' @family pattern-bull
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
#' # Get CSP Long Candle results
#' csp_long_candle_data <- addCSPLongCandle(AAPL)
#'
#' # Add Long Candle points
#' result_with_long_candle <- drawCSPLongCandle(result, csp_long_candle_data)
#'
#' # Display the combined plot with Long Candle points
#' print(result_with_long_candle$combined_plot)
#' }
drawCSPLongCandle <- function(eCandleSticks_result, csp_long_candle_result,
                             long_white_candle_color = "green", long_black_candle_color = "red",
                             point_size = 3, long_white_candle_shape = 19, long_black_candle_shape = 19,
                             point_alpha = 0.8, mark_at_close = TRUE) {
  # Validate csp_long_candle_result
  if (!is.data.frame(csp_long_candle_result) && !xts::is.xts(csp_long_candle_result)) {
    stop("csp_long_candle_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_long_candle_result)) {
    csp_long_candle_result <- data.frame(
      Date = zoo::index(csp_long_candle_result),
      as.data.frame(csp_long_candle_result)
    )
  }

  if (!"Date" %in% colnames(csp_long_candle_result)) {
    stop("csp_long_candle_result must contain a 'Date' column")
  }

  required_cols <- c("LongWhiteCandle", "LongBlackCandle")
  if (!all(required_cols %in% colnames(csp_long_candle_result))) {
    stop("csp_long_candle_result must contain 'LongWhiteCandle' and 'LongBlackCandle' columns")
  }

  # Convert Date to proper format if needed
  csp_long_candle_result$Date <- as.Date(csp_long_candle_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_long_candle_result, by = "Date", all.x = TRUE)

  # Extract different types of Long Candle points
  long_white_candle_points <- merged_data[merged_data$LongWhiteCandle == TRUE & !is.na(merged_data$LongWhiteCandle), ]
  long_black_candle_points <- merged_data[merged_data$LongBlackCandle == TRUE & !is.na(merged_data$LongBlackCandle), ]

  # Determine y-value for marking
  if (mark_at_close) {
    # Mark at Close price
    long_white_candle_points$LongCandleLevel <- long_white_candle_points$Close
    long_black_candle_points$LongCandleLevel <- long_black_candle_points$Close
  } else {
    # Mark at the midpoint of the candle
    long_white_candle_points$LongCandleLevel <- (long_white_candle_points$High + long_white_candle_points$Low) / 2
    long_black_candle_points$LongCandleLevel <- (long_black_candle_points$High + long_black_candle_points$Low) / 2
  }

  # Add Long Candle points to the price plot
  price_plot_with_long_candle <- eCandleSticks_result$price_plot

  # Add Long White Candle points (if any)
  if (nrow(long_white_candle_points) > 0) {
    price_plot_with_long_candle <- price_plot_with_long_candle +
      ggplot2::geom_point(
        data = long_white_candle_points,
        aes(x = Date, y = LongCandleLevel, color = "Long White Candle"),
        size = point_size,
        shape = long_white_candle_shape,
        alpha = point_alpha
      )
  }

  # Add Long Black Candle points (if any)
  if (nrow(long_black_candle_points) > 0) {
    price_plot_with_long_candle <- price_plot_with_long_candle +
      ggplot2::geom_point(
        data = long_black_candle_points,
        aes(x = Date, y = LongCandleLevel, color = "Long Black Candle"),
        size = point_size,
        shape = long_black_candle_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Long Candle points
  if (nrow(long_white_candle_points) > 0 || nrow(long_black_candle_points) > 0) {
    price_plot_with_long_candle <- price_plot_with_long_candle +
      ggplot2::scale_color_manual(
        name = "CSP Long Candle Patterns",
        values = c(
          "Long White Candle" = long_white_candle_color,
          "Long Black Candle" = long_black_candle_color
        ),
        breaks = c("Long White Candle", "Long Black Candle")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(long_white_candle_shape, long_black_candle_shape),
            size = rep(point_size, 2),
            alpha = rep(point_alpha, 2)
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_long_candle

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_long_candle, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_long_candle
  }

  # Add csp_long_candle_result to the output for reference
  eCandleSticks_result$csp_long_candle_data <- csp_long_candle_result
  eCandleSticks_result$long_white_candle_points <- long_white_candle_points
  eCandleSticks_result$long_black_candle_points <- long_black_candle_points

  return(eCandleSticks_result)
}
