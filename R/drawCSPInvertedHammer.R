#' Add CSP Inverted Hammer Points to Candlestick Chart
#'
#' This function adds CSP Inverted Hammer points to a candlestick chart created by eCandleSticks
#' using the results from a CSP Inverted Hammer analysis, and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_inverted_hammer_result The result object from CSP Inverted Hammer analysis, which should be a data frame
#'        containing a 'Date' column and a logical column 'InvertedHammer'
#' @param inverted_hammer_color Color for Inverted Hammer points. Default "orange".
#' @param point_size Size for the Inverted Hammer points. Default 3.
#' @param inverted_hammer_shape Shape for Inverted Hammer points. Default 11 (filled star).
#' @param point_alpha Alpha transparency for the Inverted Hammer points. Default 0.8.
#' @param mark_at_high_level Whether to mark at the high price (inverted hammer's highest point) instead of Close price. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Inverted Hammer points added to the price plot
#' and the combined plot updated accordingly.
#' @family pattern-1bar
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
#' # Get CSP Inverted Hammer results
#' csp_inverted_hammer_data <- addCSPInvertedHammer(AAPL)
#'
#' # Add Inverted Hammer points
#' result_with_inverted_hammer <- drawCSPInvertedHammer(result, csp_inverted_hammer_data)
#'
#' # Display the combined plot with Inverted Hammer points
#' print(result_with_inverted_hammer$combined_plot)
#' }
drawCSPInvertedHammer <- function(eCandleSticks_result, csp_inverted_hammer_result,
                                 inverted_hammer_color = "orange", point_size = 3,
                                 inverted_hammer_shape = 11, point_alpha = 0.8,
                                 mark_at_high_level = TRUE) {
  # Validate csp_inverted_hammer_result
  if (!is.data.frame(csp_inverted_hammer_result) && !xts::is.xts(csp_inverted_hammer_result)) {
    stop("csp_inverted_hammer_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_inverted_hammer_result)) {
    csp_inverted_hammer_result <- data.frame(
      Date = zoo::index(csp_inverted_hammer_result),
      as.data.frame(csp_inverted_hammer_result)
    )
  }

  if (!"Date" %in% colnames(csp_inverted_hammer_result)) {
    stop("csp_inverted_hammer_result must contain a 'Date' column")
  }

  if (!"InvertedHammer" %in% colnames(csp_inverted_hammer_result)) {
    stop("csp_inverted_hammer_result must contain an 'InvertedHammer' column")
  }

  # Convert Date to proper format if needed
  csp_inverted_hammer_result$Date <- as.Date(csp_inverted_hammer_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_inverted_hammer_result, by = "Date", all.x = TRUE)

  # Extract Inverted Hammer points
  inverted_hammer_points <- merged_data[merged_data$InvertedHammer == TRUE & !is.na(merged_data$InvertedHammer), ]

  # Determine y-value for marking
  if (mark_at_high_level) {
    # For Inverted Hammer, mark at the High price (inverted hammer's highest point)
    inverted_hammer_points$InvertedHammerLevel <- inverted_hammer_points$High
  } else {
    # Mark at Close price
    inverted_hammer_points$InvertedHammerLevel <- inverted_hammer_points$Close
  }

  # Add Inverted Hammer points to the price plot
  price_plot_with_inverted_hammer <- eCandleSticks_result$price_plot

  # Add Inverted Hammer points (if any)
  if (nrow(inverted_hammer_points) > 0) {
    price_plot_with_inverted_hammer <- price_plot_with_inverted_hammer +
      ggplot2::geom_point(
        data = inverted_hammer_points,
        aes(x = Date, y = InvertedHammerLevel, color = "Inverted Hammer"),
        size = point_size,
        shape = inverted_hammer_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Inverted Hammer points
  if (nrow(inverted_hammer_points) > 0) {
    price_plot_with_inverted_hammer <- price_plot_with_inverted_hammer +
      ggplot2::scale_color_manual(
        name = "CSP Patterns",
        values = c("Inverted Hammer" = inverted_hammer_color),
        breaks = c("Inverted Hammer")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = inverted_hammer_shape,
            size = point_size,
            alpha = point_alpha
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_inverted_hammer

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_inverted_hammer, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_inverted_hammer
  }

  # Add csp_inverted_hammer_result to the output for reference
  eCandleSticks_result$csp_inverted_hammer_data <- csp_inverted_hammer_result
  eCandleSticks_result$inverted_hammer_points <- inverted_hammer_points

  return(eCandleSticks_result)
}
