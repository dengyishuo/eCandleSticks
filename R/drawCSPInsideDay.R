#' Add CSP Inside Day Points to Candlestick Chart
#'
#' This function adds CSP Inside Day points to a candlestick chart created by eCandleSticks
#' using the results from a CSP Inside Day analysis, and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_inside_day_result The result object from CSP Inside Day analysis, which should be a data frame
#'        containing a 'Date' column and a logical column 'InsideDay'
#' @param inside_day_color Color for Inside Day points. Default "purple".
#' @param point_size Size for the Inside Day points. Default 3.
#' @param inside_day_shape Shape for Inside Day points. Default 23 (diamond).
#' @param point_alpha Alpha transparency for the Inside Day points. Default 0.8.
#' @param mark_at_midpoint Whether to mark at the midpoint of the candle instead of Close price. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Inside Day points added to the price plot
#' and the combined plot updated accordingly.
#' @family pattern-2bar
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
#' # Get CSP Inside Day results
#' csp_inside_day_data <- addCSPInsideDay(AAPL)
#' # This returns a data frame with InsideDay column
#'
#' # Add Inside Day points
#' result_with_inside_day <- drawCSPInsideDay(result, csp_inside_day_data)
#'
#' # Display the combined plot with Inside Day points
#' print(result_with_inside_day$combined_plot)
#' }
drawCSPInsideDay <- function(eCandleSticks_result, csp_inside_day_result,
                            inside_day_color = "purple", point_size = 3,
                            inside_day_shape = 23, point_alpha = 0.8,
                            mark_at_midpoint = TRUE) {
  # Validate csp_inside_day_result
  if (!is.data.frame(csp_inside_day_result) && !xts::is.xts(csp_inside_day_result)) {
    stop("csp_inside_day_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_inside_day_result)) {
    csp_inside_day_result <- data.frame(
      Date = zoo::index(csp_inside_day_result),
      as.data.frame(csp_inside_day_result)
    )
  }

  if (!"Date" %in% colnames(csp_inside_day_result)) {
    stop("csp_inside_day_result must contain a 'Date' column")
  }

  if (!"InsideDay" %in% colnames(csp_inside_day_result)) {
    stop("csp_inside_day_result must contain an 'InsideDay' column")
  }

  # Convert Date to proper format if needed
  csp_inside_day_result$Date <- as.Date(csp_inside_day_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_inside_day_result, by = "Date", all.x = TRUE)

  # Extract Inside Day points
  inside_day_points <- merged_data[merged_data$InsideDay == TRUE & !is.na(merged_data$InsideDay), ]

  # Determine y-value for marking
  if (mark_at_midpoint) {
    # Mark at the midpoint of the candle (average of High and Low)
    inside_day_points$InsideDayLevel <- (inside_day_points$High + inside_day_points$Low) / 2
  } else {
    # Mark at Close price
    inside_day_points$InsideDayLevel <- inside_day_points$Close
  }

  # Add Inside Day points to the price plot
  price_plot_with_inside_day <- eCandleSticks_result$price_plot

  # Add Inside Day points (if any)
  if (nrow(inside_day_points) > 0) {
    price_plot_with_inside_day <- price_plot_with_inside_day +
      ggplot2::geom_point(
        data = inside_day_points,
        aes(x = Date, y = InsideDayLevel, color = "Inside Day"),
        size = point_size,
        shape = inside_day_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Inside Day points
  if (nrow(inside_day_points) > 0) {
    price_plot_with_inside_day <- price_plot_with_inside_day +
      ggplot2::scale_color_manual(
        name = "CSP Patterns",
        values = c("Inside Day" = inside_day_color),
        breaks = c("Inside Day")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = inside_day_shape,
            size = point_size,
            alpha = point_alpha
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_inside_day

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_inside_day, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_inside_day
  }

  # Add csp_inside_day_result to the output for reference
  eCandleSticks_result$csp_inside_day_data <- csp_inside_day_result
  eCandleSticks_result$inside_day_points <- inside_day_points

  return(eCandleSticks_result)
}
