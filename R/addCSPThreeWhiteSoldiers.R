#' Add CSP Three White Soldiers Pattern to Candlestick Chart
#'
#' This function adds CSP Three White Soldiers pattern to a candlestick chart
#' created by eCandleSticks using the results from CSPThreeWhiteSoldiers analysis.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_three_white_result The result object from CSPThreeWhiteSoldiers analysis,
#'        which should be an xts object containing logical column 'ThreeWhiteSoldiers'
#' @param pattern_color Color for Three White Soldiers points. Default "darkgreen".
#' @param point_size Size for the pattern points. Default 4.
#' @param pattern_shape Shape for Three White Soldiers points. Default 24 (up triangle).
#' @param point_alpha Alpha transparency for the pattern points. Default 0.8.
#' @param mark_at_close Whether to mark at the close price. If FALSE, marks at the high. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Three White Soldiers pattern added to the price plot
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
#' # Get CSP Three White Soldiers results
#' csp_three_white_data <- CSPThreeWhiteSoldiers(AAPL)
#'
#' # Add Three White Soldiers pattern
#' result_with_patterns <- addCSPThreeWhiteSoldiers(result, csp_three_white_data)
#'
#' # Display the combined plot with pattern points
#' print(result_with_patterns$combined_plot)
#' }
addCSPThreeWhiteSoldiers <- function(eCandleSticks_result, csp_three_white_result,
                                     pattern_color = "darkgreen", point_size = 4,
                                     pattern_shape = 24, point_alpha = 0.8,
                                     mark_at_close = TRUE) {
  # Validate csp_three_white_result
  if (!xts::is.xts(csp_three_white_result)) {
    stop("csp_three_white_result must be an xts object")
  }

  # Check if required column exists
  required_col <- "ThreeWhiteSoldiers"
  if (!required_col %in% colnames(csp_three_white_result)) {
    stop("csp_three_white_result must contain 'ThreeWhiteSoldiers' column")
  }

  # Convert to data frame
  csp_three_white_df <- data.frame(
    Date = zoo::index(csp_three_white_result),
    as.data.frame(csp_three_white_result)
  )

  # Convert Date to proper format
  csp_three_white_df$Date <- as.Date(csp_three_white_df$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_three_white_df, by = "Date", all.x = TRUE)

  # Extract pattern points
  pattern_points <- merged_data[merged_data$ThreeWhiteSoldiers == TRUE & !is.na(merged_data$ThreeWhiteSoldiers), ]

  # Determine y-value for marking
  if (mark_at_close) {
    pattern_points$PatternLevel <- pattern_points$Close
  } else {
    # Mark at the High price
    pattern_points$PatternLevel <- pattern_points$High
  }

  # Add pattern points to the price plot
  price_plot_with_patterns <- eCandleSticks_result$price_plot

  # Add Three White Soldiers points (if any)
  if (nrow(pattern_points) > 0) {
    price_plot_with_patterns <- price_plot_with_patterns +
      ggplot2::geom_point(
        data = pattern_points,
        aes(x = Date, y = PatternLevel, color = "Three White Soldiers"),
        size = point_size,
        shape = pattern_shape,
        alpha = point_alpha
      ) +
      ggplot2::scale_color_manual(
        name = "CSP Three White Soldiers Pattern",
        values = c("Three White Soldiers" = pattern_color),
        breaks = c("Three White Soldiers")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = pattern_shape,
            size = point_size,
            alpha = point_alpha
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
  eCandleSticks_result$csp_three_white_data <- csp_three_white_df
  eCandleSticks_result$pattern_points <- pattern_points

  return(eCandleSticks_result)
}
