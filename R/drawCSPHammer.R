#' Add CSP Hammer Points to Candlestick Chart
#'
#' This function adds CSP Hammer points to a candlestick chart created by eCandleSticks
#' using the results from a CSP Hammer analysis, and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_hammer_result The result object from CSP Hammer analysis, which should be a data frame
#'        containing a 'Date' column and a logical column 'Hammer'
#' @param hammer_color Color for Hammer points. Default "blue".
#' @param point_size Size for the Hammer points. Default 3.
#' @param hammer_shape Shape for Hammer points. Default 18 (diamond).
#' @param point_alpha Alpha transparency for the Hammer points. Default 0.8.
#' @param mark_at_low_level Whether to mark at the low price (hammer's lowest point) instead of Close price. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Hammer points added to the price plot
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
#' # Get CSP Hammer results
#' csp_hammer_data <- addCSPHammer(AAPL)
#' # This returns a data frame with Hammer column
#'
#' # Add Hammer points
#' result_with_hammer <- drawCSPHammer(result, csp_hammer_data)
#'
#' # Display the combined plot with Hammer points
#' print(result_with_hammer$combined_plot)
#' }
drawCSPHammer <- function(eCandleSticks_result, csp_hammer_result,
                         hammer_color = "blue", point_size = 3,
                         hammer_shape = 18, point_alpha = 0.8,
                         mark_at_low_level = TRUE) {
  # Validate csp_hammer_result
  if (!is.data.frame(csp_hammer_result) && !xts::is.xts(csp_hammer_result)) {
    stop("csp_hammer_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_hammer_result)) {
    csp_hammer_result <- data.frame(
      Date = zoo::index(csp_hammer_result),
      as.data.frame(csp_hammer_result)
    )
  }

  if (!"Date" %in% colnames(csp_hammer_result)) {
    stop("csp_hammer_result must contain a 'Date' column")
  }

  if (!"Hammer" %in% colnames(csp_hammer_result)) {
    stop("csp_hammer_result must contain a 'Hammer' column")
  }

  # Convert Date to proper format if needed
  csp_hammer_result$Date <- as.Date(csp_hammer_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_hammer_result, by = "Date", all.x = TRUE)

  # Extract Hammer points
  hammer_points <- merged_data[merged_data$Hammer == TRUE & !is.na(merged_data$Hammer), ]

  # Determine y-value for marking
  if (mark_at_low_level) {
    # For Hammer, mark at the Low price (hammer's lowest point)
    hammer_points$HammerLevel <- hammer_points$Low
  } else {
    # Mark at Close price
    hammer_points$HammerLevel <- hammer_points$Close
  }

  # Add Hammer points to the price plot
  price_plot_with_hammer <- eCandleSticks_result$price_plot

  # Add Hammer points (if any)
  if (nrow(hammer_points) > 0) {
    price_plot_with_hammer <- price_plot_with_hammer +
      ggplot2::geom_point(
        data = hammer_points,
        aes(x = Date, y = HammerLevel, color = "Hammer"),
        size = point_size,
        shape = hammer_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Hammer points
  if (nrow(hammer_points) > 0) {
    price_plot_with_hammer <- price_plot_with_hammer +
      ggplot2::scale_color_manual(
        name = "CSP Patterns",
        values = c("Hammer" = hammer_color),
        breaks = c("Hammer")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = hammer_shape,
            size = point_size,
            alpha = point_alpha
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_hammer

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_hammer, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_hammer
  }

  # Add csp_hammer_result to the output for reference
  eCandleSticks_result$csp_hammer_data <- csp_hammer_result
  eCandleSticks_result$hammer_points <- hammer_points

  return(eCandleSticks_result)
}
