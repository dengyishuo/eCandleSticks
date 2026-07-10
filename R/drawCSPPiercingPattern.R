#' Add CSP Piercing Pattern Points to Candlestick Chart
#'
#' This function adds CSP Piercing Pattern points to a candlestick chart
#' created by eCandleSticks using the results from a CSP Piercing Pattern analysis,
#' and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_piercing_pattern_result The result object from CSP Piercing Pattern analysis,
#'        which should be a data frame containing a 'Date' column and a logical column 'PiercingPattern'
#' @param piercing_pattern_color Color for Piercing Pattern points. Default "blue".
#' @param point_size Size for the Piercing Pattern points. Default 3.
#' @param piercing_pattern_shape Shape for Piercing Pattern points. Default 21 (filled circle with border).
#' @param point_alpha Alpha transparency for the Piercing Pattern points. Default 0.8.
#' @param mark_at_close Whether to mark at the close price. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Piercing Pattern points added to the price plot
#' and the combined plot updated accordingly.
#' @family pattern-2bar
#' @family pattern-bull
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
#' # Get CSP Piercing Pattern results
#' csp_piercing_pattern_data <- addCSPPiercingPattern(AAPL)
#'
#' # Add Piercing Pattern points
#' result_with_piercing_pattern <- drawCSPPiercingPattern(result, csp_piercing_pattern_data)
#'
#' # Display the combined plot with Piercing Pattern points
#' print(result_with_piercing_pattern$combined_plot)
#' }
drawCSPPiercingPattern <- function(eCandleSticks_result, csp_piercing_pattern_result,
                                  piercing_pattern_color = "blue", point_size = 3,
                                  piercing_pattern_shape = 21, point_alpha = 0.8,
                                  mark_at_close = TRUE) {
  # Validate csp_piercing_pattern_result
  if (!is.data.frame(csp_piercing_pattern_result) && !xts::is.xts(csp_piercing_pattern_result)) {
    stop("csp_piercing_pattern_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_piercing_pattern_result)) {
    csp_piercing_pattern_result <- data.frame(
      Date = zoo::index(csp_piercing_pattern_result),
      as.data.frame(csp_piercing_pattern_result)
    )
  }

  if (!"Date" %in% colnames(csp_piercing_pattern_result)) {
    stop("csp_piercing_pattern_result must contain a 'Date' column")
  }

  if (!"PiercingPattern" %in% colnames(csp_piercing_pattern_result)) {
    stop("csp_piercing_pattern_result must contain a 'PiercingPattern' column")
  }

  # Convert Date to proper format if needed
  csp_piercing_pattern_result$Date <- as.Date(csp_piercing_pattern_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_piercing_pattern_result, by = "Date", all.x = TRUE)

  # Extract Piercing Pattern points
  piercing_pattern_points <- merged_data[merged_data$PiercingPattern == TRUE & !is.na(merged_data$PiercingPattern), ]

  # Determine y-value for marking
  if (mark_at_close) {
    # Mark at Close price
    piercing_pattern_points$PiercingPatternLevel <- piercing_pattern_points$Close
  } else {
    # Mark at the midpoint of the candle
    piercing_pattern_points$PiercingPatternLevel <- (piercing_pattern_points$High + piercing_pattern_points$Low) / 2
  }

  # Add Piercing Pattern points to the price plot
  price_plot_with_piercing_pattern <- eCandleSticks_result$price_plot

  # Add Piercing Pattern points (if any)
  if (nrow(piercing_pattern_points) > 0) {
    price_plot_with_piercing_pattern <- price_plot_with_piercing_pattern +
      ggplot2::geom_point(
        data = piercing_pattern_points,
        aes(x = Date, y = PiercingPatternLevel, color = "Piercing Pattern"),
        size = point_size,
        shape = piercing_pattern_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Piercing Pattern points
  if (nrow(piercing_pattern_points) > 0) {
    price_plot_with_piercing_pattern <- price_plot_with_piercing_pattern +
      ggplot2::scale_color_manual(
        name = "CSP Patterns",
        values = c("Piercing Pattern" = piercing_pattern_color),
        breaks = c("Piercing Pattern")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = piercing_pattern_shape,
            size = point_size,
            alpha = point_alpha
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_piercing_pattern

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_piercing_pattern, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_piercing_pattern
  }

  # Add csp_piercing_pattern_result to the output for reference
  eCandleSticks_result$csp_piercing_pattern_data <- csp_piercing_pattern_result
  eCandleSticks_result$piercing_pattern_points <- piercing_pattern_points

  return(eCandleSticks_result)
}
