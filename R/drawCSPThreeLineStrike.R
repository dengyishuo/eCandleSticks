#' Add CSP Three Line Strike Patterns to Candlestick Chart
#'
#' This function adds CSP Three Line Strike patterns (Bullish and Bearish)
#' to a candlestick chart created by eCandleSticks using the results from CSPThreeLineStrike analysis.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_three_line_result The result object from CSPThreeLineStrike analysis, which should be an xts object
#'        containing logical columns 'Bull.ThreeLineStrike' and 'Bear.ThreeLineStrike'
#' @param bull_color Color for Bullish Three Line Strike points. Default "blue".
#' @param bear_color Color for Bearish Three Line Strike points. Default "purple".
#' @param point_size Size for the pattern points. Default 4.
#' @param bull_shape Shape for Bullish pattern points. Default 24 (up triangle).
#' @param bear_shape Shape for Bearish pattern points. Default 25 (down triangle).
#' @param point_alpha Alpha transparency for the pattern points. Default 0.8.
#' @param mark_at_close Whether to mark at the close price. If FALSE, marks at the high/low. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Three Line Strike patterns added to the price plot
#' and the combined plot updated accordingly.
#' @family pattern-3bar
#' @family pattern-bull
#' @family pattern-bear
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
#' # Get CSP Three Line Strike results
#' csp_three_line_data <- addCSPThreeLineStrike(AAPL)
#'
#' # Add Three Line Strike patterns
#' result_with_patterns <- drawCSPThreeLineStrike(result, csp_three_line_data)
#'
#' # Display the combined plot with pattern points
#' print(result_with_patterns$combined_plot)
#' }
drawCSPThreeLineStrike <- function(eCandleSticks_result, csp_three_line_result,
                                  bull_color = "blue", bear_color = "purple",
                                  point_size = 4, bull_shape = 24, bear_shape = 25,
                                  point_alpha = 0.8, mark_at_close = TRUE) {
  # Validate csp_three_line_result
  if (!xts::is.xts(csp_three_line_result)) {
    stop("csp_three_line_result must be an xts object")
  }

  # Check if required columns exist
  required_cols <- c("Bull.ThreeLineStrike", "Bear.ThreeLineStrike")
  if (!all(required_cols %in% colnames(csp_three_line_result))) {
    stop("csp_three_line_result must contain 'Bull.ThreeLineStrike' and 'Bear.ThreeLineStrike' columns")
  }

  # Convert to data frame
  csp_three_line_df <- data.frame(
    Date = zoo::index(csp_three_line_result),
    as.data.frame(csp_three_line_result)
  )

  # Convert Date to proper format
  csp_three_line_df$Date <- as.Date(csp_three_line_df$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_three_line_df, by = "Date", all.x = TRUE)

  # Extract different types of pattern points
  bull_points <- merged_data[merged_data$Bull.ThreeLineStrike == TRUE & !is.na(merged_data$Bull.ThreeLineStrike), ]
  bear_points <- merged_data[merged_data$Bear.ThreeLineStrike == TRUE & !is.na(merged_data$Bear.ThreeLineStrike), ]

  # Determine y-value for marking
  if (mark_at_close) {
    bull_points$PatternLevel <- bull_points$Close
    bear_points$PatternLevel <- bear_points$Close
  } else {
    # For Bullish pattern, mark at the High price
    # For Bearish pattern, mark at the Low price
    bull_points$PatternLevel <- bull_points$High
    bear_points$PatternLevel <- bear_points$Low
  }

  # Add pattern points to the price plot
  price_plot_with_patterns <- eCandleSticks_result$price_plot

  # Add Bullish Three Line Strike points (if any)
  if (nrow(bull_points) > 0) {
    price_plot_with_patterns <- price_plot_with_patterns +
      ggplot2::geom_point(
        data = bull_points,
        aes(x = Date, y = PatternLevel, color = "Bullish Three Line Strike"),
        size = point_size,
        shape = bull_shape,
        alpha = point_alpha
      )
  }

  # Add Bearish Three Line Strike points (if any)
  if (nrow(bear_points) > 0) {
    price_plot_with_patterns <- price_plot_with_patterns +
      ggplot2::geom_point(
        data = bear_points,
        aes(x = Date, y = PatternLevel, color = "Bearish Three Line Strike"),
        size = point_size,
        shape = bear_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any pattern points
  if (nrow(bull_points) > 0 || nrow(bear_points) > 0) {
    price_plot_with_patterns <- price_plot_with_patterns +
      ggplot2::scale_color_manual(
        name = "CSP Three Line Strike Patterns",
        values = c(
          "Bullish Three Line Strike" = bull_color,
          "Bearish Three Line Strike" = bear_color
        ),
        breaks = c("Bullish Three Line Strike", "Bearish Three Line Strike")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(bull_shape, bear_shape),
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
  eCandleSticks_result$csp_three_line_data <- csp_three_line_df
  eCandleSticks_result$bull_points <- bull_points
  eCandleSticks_result$bear_points <- bear_points

  return(eCandleSticks_result)
}
