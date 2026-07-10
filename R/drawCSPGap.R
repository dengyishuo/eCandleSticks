#' Add CSP Gap Points to Candlestick Chart
#'
#' This function adds CSP Gap points (Gap Up and Gap Down)
#' to a candlestick chart created by eCandleSticks using the results from a CSP Gap analysis,
#' and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_gap_result The result object from CSP Gap analysis, which should be a data frame
#'        containing a 'Date' column and logical columns 'GapUp' and 'GapDown'
#' @param gap_up_color Color for Gap Up points. Default "green".
#' @param gap_down_color Color for Gap Down points. Default "red".
#' @param point_size Size for the Gap points. Default 3.
#' @param gap_up_shape Shape for Gap Up points. Default 24 (up triangle).
#' @param gap_down_shape Shape for Gap Down points. Default 25 (down triangle).
#' @param point_alpha Alpha transparency for the Gap points. Default 0.8.
#' @param mark_at_gap_level Whether to mark at the actual gap level (High/Low) instead of Close price. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Gap points added to the price plot
#' and the combined plot updated accordingly.
#' @family pattern-2bar
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
#' # Get CSP Gap results
#' csp_gap_data <- addCSPGap(AAPL)
#' # This returns a data frame with GapUp and GapDown columns
#'
#' # Add Gap points
#' result_with_gap <- drawCSPGap(result, csp_gap_data)
#'
#' # Display the combined plot with Gap points
#' print(result_with_gap$combined_plot)
#' }
drawCSPGap <- function(eCandleSticks_result, csp_gap_result,
                      gap_up_color = "green", gap_down_color = "red",
                      point_size = 3, gap_up_shape = 24, gap_down_shape = 25,
                      point_alpha = 0.8, mark_at_gap_level = TRUE) {
  # Validate csp_gap_result
  if (!is.data.frame(csp_gap_result) && !xts::is.xts(csp_gap_result)) {
    stop("csp_gap_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_gap_result)) {
    csp_gap_result <- data.frame(
      Date = zoo::index(csp_gap_result),
      as.data.frame(csp_gap_result)
    )
  }

  if (!"Date" %in% colnames(csp_gap_result)) {
    stop("csp_gap_result must contain a 'Date' column")
  }

  required_cols <- c("GapUp", "GapDown")
  if (!all(required_cols %in% colnames(csp_gap_result))) {
    stop("csp_gap_result must contain 'GapUp' and 'GapDown' columns")
  }

  # Convert Date to proper format if needed
  csp_gap_result$Date <- as.Date(csp_gap_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_gap_result, by = "Date", all.x = TRUE)

  # Extract different types of Gap points
  gap_up_points <- merged_data[merged_data$GapUp == TRUE & !is.na(merged_data$GapUp), ]
  gap_down_points <- merged_data[merged_data$GapDown == TRUE & !is.na(merged_data$GapDown), ]

  # Determine y-value for marking
  if (mark_at_gap_level) {
    # For Gap Up, mark at the Low price (gap bottom)
    # For Gap Down, mark at the High price (gap top)
    gap_up_points$GapLevel <- gap_up_points$Low
    gap_down_points$GapLevel <- gap_down_points$High
  } else {
    # Mark at Close price
    gap_up_points$GapLevel <- gap_up_points$Close
    gap_down_points$GapLevel <- gap_down_points$Close
  }

  # Add Gap points to the price plot
  price_plot_with_gap <- eCandleSticks_result$price_plot

  # Add Gap Up points (if any)
  if (nrow(gap_up_points) > 0) {
    price_plot_with_gap <- price_plot_with_gap +
      ggplot2::geom_point(
        data = gap_up_points,
        aes(x = Date, y = GapLevel, color = "Gap Up"),
        size = point_size,
        shape = gap_up_shape,
        alpha = point_alpha
      )
  }

  # Add Gap Down points (if any)
  if (nrow(gap_down_points) > 0) {
    price_plot_with_gap <- price_plot_with_gap +
      ggplot2::geom_point(
        data = gap_down_points,
        aes(x = Date, y = GapLevel, color = "Gap Down"),
        size = point_size,
        shape = gap_down_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Gap points
  if (nrow(gap_up_points) > 0 || nrow(gap_down_points) > 0) {
    price_plot_with_gap <- price_plot_with_gap +
      ggplot2::scale_color_manual(
        name = "CSP Gap Patterns",
        values = c(
          "Gap Up" = gap_up_color,
          "Gap Down" = gap_down_color
        ),
        breaks = c("Gap Up", "Gap Down")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(gap_up_shape, gap_down_shape),
            size = rep(point_size, 2),
            alpha = rep(point_alpha, 2)
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_gap

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_gap, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_gap
  }

  # Add csp_gap_result to the output for reference
  eCandleSticks_result$csp_gap_data <- csp_gap_result
  eCandleSticks_result$gap_up_points <- gap_up_points
  eCandleSticks_result$gap_down_points <- gap_down_points

  return(eCandleSticks_result)
}
