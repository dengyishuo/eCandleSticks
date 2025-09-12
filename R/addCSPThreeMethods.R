#' Add CSP Three Methods Patterns to Candlestick Chart
#'
#' This function adds CSP Three Methods patterns (Rising Three Methods and Falling Three Methods)
#' to a candlestick chart created by eCandleSticks using the results from CSPThreeMethods analysis.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_three_methods_result The result object from CSPThreeMethods analysis, which should be an xts object
#'        containing logical columns 'RisingThreeMethods' and 'FallingThreeMethods'
#' @param rising_color Color for Rising Three Methods points. Default "green".
#' @param falling_color Color for Falling Three Methods points. Default "red".
#' @param point_size Size for the pattern points. Default 4.
#' @param rising_shape Shape for Rising Three Methods points. Default 24 (up triangle).
#' @param falling_shape Shape for Falling Three Methods points. Default 25 (down triangle).
#' @param point_alpha Alpha transparency for the pattern points. Default 0.8.
#' @param mark_at_close Whether to mark at the close price. If FALSE, marks at the high/low. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Three Methods patterns added to the price plot
#' and the combined plot updated accordingly.
#' @export
#' @importFrom ggplot2 geom_point aes
#' @importFrom cowplot plot_grid
#' @importFrom xts is.xts
#' @importFrom zoo index
#'
#' @examples
#' \dontrun{
#' library(quantmod)
#' getSymbols("AAPL", src = "yahoo", from = "2023-01-01", to = "2025-09-08")
#'
#' # Create candlestick chart
#' result <- eCandleSticks(AAPL)
#'
#' # Get CSP Three Methods results
#' csp_three_methods_data <- CSPThreeMethods(AAPL)
#'
#' # Add Three Methods patterns
#' result_with_patterns <- addCSPThreeMethods(result, csp_three_methods_data)
#'
#' # Display the combined plot with pattern points
#' print(result_with_patterns$combined_plot)
#' }
addCSPThreeMethods <- function(eCandleSticks_result, csp_three_methods_result,
                               rising_color = "green", falling_color = "red",
                               point_size = 4, rising_shape = 24, falling_shape = 25,
                               point_alpha = 0.8, mark_at_close = TRUE) {
  # Validate csp_three_methods_result
  if (!xts::is.xts(csp_three_methods_result)) {
    stop("csp_three_methods_result must be an xts object")
  }

  # Check if required columns exist
  required_cols <- c("RisingThreeMethods", "FallingThreeMethods")
  if (!all(required_cols %in% colnames(csp_three_methods_result))) {
    stop("csp_three_methods_result must contain 'RisingThreeMethods' and 'FallingThreeMethods' columns")
  }

  # Convert to data frame
  csp_three_methods_df <- data.frame(
    Date = zoo::index(csp_three_methods_result),
    as.data.frame(csp_three_methods_result)
  )

  # Convert Date to proper format
  csp_three_methods_df$Date <- as.Date(csp_three_methods_df$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_three_methods_df, by = "Date", all.x = TRUE)

  # Extract different types of pattern points
  rising_points <- merged_data[merged_data$RisingThreeMethods == TRUE & !is.na(merged_data$RisingThreeMethods), ]
  falling_points <- merged_data[merged_data$FallingThreeMethods == TRUE & !is.na(merged_data$FallingThreeMethods), ]

  # Determine y-value for marking
  if (mark_at_close) {
    rising_points$PatternLevel <- rising_points$Close
    falling_points$PatternLevel <- falling_points$Close
  } else {
    # For Rising Three Methods, mark at the High price
    # For Falling Three Methods, mark at the Low price
    rising_points$PatternLevel <- rising_points$High
    falling_points$PatternLevel <- falling_points$Low
  }

  # Add pattern points to the price plot
  price_plot_with_patterns <- eCandleSticks_result$price_plot

  # Add Rising Three Methods points (if any)
  if (nrow(rising_points) > 0) {
    price_plot_with_patterns <- price_plot_with_patterns +
      ggplot2::geom_point(
        data = rising_points,
        aes(x = Date, y = PatternLevel, color = "Rising Three Methods"),
        size = point_size,
        shape = rising_shape,
        alpha = point_alpha
      )
  }

  # Add Falling Three Methods points (if any)
  if (nrow(falling_points) > 0) {
    price_plot_with_patterns <- price_plot_with_patterns +
      ggplot2::geom_point(
        data = falling_points,
        aes(x = Date, y = PatternLevel, color = "Falling Three Methods"),
        size = point_size,
        shape = falling_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any pattern points
  if (nrow(rising_points) > 0 || nrow(falling_points) > 0) {
    price_plot_with_patterns <- price_plot_with_patterns +
      ggplot2::scale_color_manual(
        name = "CSP Three Methods Patterns",
        values = c(
          "Rising Three Methods" = rising_color,
          "Falling Three Methods" = falling_color
        ),
        breaks = c("Rising Three Methods", "Falling Three Methods")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(rising_shape, falling_shape),
            size = rep(point_size, 2),
            alpha = rep(point_alpha, 2)
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_patterns

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_patterns, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_patterns
  }

  # Add pattern data to the output for reference
  eCandleSticks_result$csp_three_methods_data <- csp_three_methods_df
  eCandleSticks_result$rising_points <- rising_points
  eCandleSticks_result$falling_points <- falling_points

  return(eCandleSticks_result)
}
