#' Add CSP Outside Day Points to Candlestick Chart
#'
#' This function adds CSP Outside Day points to a candlestick chart
#' created by eCandleSticks using the results from a CSP Outside Day analysis,
#' and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_outside_day_result The result object from CSP Outside Day analysis,
#'        which should be a data frame containing a 'Date' column and a logical column 'OutsideDay'
#' @param outside_day_color Color for Outside Day points. Default "purple".
#' @param point_size Size for the Outside Day points. Default 3.
#' @param outside_day_shape Shape for Outside Day points. Default 23 (diamond).
#' @param point_alpha Alpha transparency for the Outside Day points. Default 0.8.
#' @param mark_at_midpoint Whether to mark at the midpoint of the candle instead of Close price. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Outside Day points added to the price plot
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
#' # Get CSP Outside Day results
#' csp_outside_day_data <- addCSPOutsideDay(AAPL)
#' # This returns a data frame with OutsideDay column
#'
#' # Add Outside Day points
#' result_with_outside_day <- drawCSPOutsideDay(result, csp_outside_day_data)
#'
#' # Display the combined plot with Outside Day points
#' print(result_with_outside_day$combined_plot)
#' }
drawCSPOutsideDay <- function(eCandleSticks_result, csp_outside_day_result,
                             outside_day_color = "purple", point_size = 3,
                             outside_day_shape = 23, point_alpha = 0.8,
                             mark_at_midpoint = TRUE) {
  # Validate csp_outside_day_result
  if (!is.data.frame(csp_outside_day_result) && !xts::is.xts(csp_outside_day_result)) {
    stop("csp_outside_day_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_outside_day_result)) {
    csp_outside_day_result <- data.frame(
      Date = zoo::index(csp_outside_day_result),
      as.data.frame(csp_outside_day_result)
    )
  }

  if (!"Date" %in% colnames(csp_outside_day_result)) {
    stop("csp_outside_day_result must contain a 'Date' column")
  }

  if (!"OutsideDay" %in% colnames(csp_outside_day_result)) {
    stop("csp_outside_day_result must contain an 'OutsideDay' column")
  }

  # Convert Date to proper format if needed
  csp_outside_day_result$Date <- as.Date(csp_outside_day_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_outside_day_result, by = "Date", all.x = TRUE)

  # Extract Outside Day points
  outside_day_points <- merged_data[merged_data$OutsideDay == TRUE & !is.na(merged_data$OutsideDay), ]

  # Determine y-value for marking
  if (mark_at_midpoint) {
    # Mark at the midpoint of the candle
    outside_day_points$OutsideDayLevel <- (outside_day_points$High + outside_day_points$Low) / 2
  } else {
    # Mark at Close price
    outside_day_points$OutsideDayLevel <- outside_day_points$Close
  }

  # Add Outside Day points to the price plot
  price_plot_with_outside_day <- eCandleSticks_result$price_plot

  # Add Outside Day points (if any)
  if (nrow(outside_day_points) > 0) {
    price_plot_with_outside_day <- price_plot_with_outside_day +
      ggplot2::geom_point(
        data = outside_day_points,
        aes(x = Date, y = OutsideDayLevel, color = "Outside Day"),
        size = point_size,
        shape = outside_day_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Outside Day points
  if (nrow(outside_day_points) > 0) {
    price_plot_with_outside_day <- price_plot_with_outside_day +
      ggplot2::scale_color_manual(
        name = "CSP Patterns",
        values = c("Outside Day" = outside_day_color),
        breaks = c("Outside Day")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = outside_day_shape,
            size = point_size,
            alpha = point_alpha
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_outside_day

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_outside_day, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_outside_day
  }

  # Add csp_outside_day_result to the output for reference
  eCandleSticks_result$csp_outside_day_data <- csp_outside_day_result
  eCandleSticks_result$outside_day_points <- outside_day_points

  return(eCandleSticks_result)
}
