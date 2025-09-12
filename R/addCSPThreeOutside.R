#' Add CSP Three Outside Points to Candlestick Chart
#'
#' This function adds CSP Three Outside points (Three Outside Up and Three Outside Down)
#' to a candlestick chart created by eCandleSticks using the results from a CSP Three Outside analysis,
#' and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_three_outside_result The result object from CSP Three Outside analysis,
#'        which should be a data frame containing a 'Date' column and logical columns
#'        'ThreeOutsideUp' and 'ThreeOutsideDown'
#' @param three_outside_up_color Color for Three Outside Up points. Default "green".
#' @param three_outside_down_color Color for Three Outside Down points. Default "red".
#' @param point_size Size for the Three Outside points. Default 3.
#' @param three_outside_up_shape Shape for Three Outside Up points. Default 24 (up triangle).
#' @param three_outside_down_shape Shape for Three Outside Down points. Default 25 (down triangle).
#' @param point_alpha Alpha transparency for the Three Outside points. Default 0.8.
#' @param mark_at_close Whether to mark at the close price. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Three Outside points added to the price plot
#' and the combined plot updated accordingly.
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
#' # Get CSP Three Outside results
#' csp_three_outside_data <- CSPThreeOutside(AAPL) # This returns a data frame with ThreeOutsideUp and ThreeOutsideDown columns
#'
#' # Add Three Outside points
#' result_with_three_outside <- addCSPThreeOutside(result, csp_three_outside_data)
#'
#' # Display the combined plot with Three Outside points
#' print(result_with_three_outside$combined_plot)
#' }
addCSPThreeOutside <- function(eCandleSticks_result, csp_three_outside_result,
                               three_outside_up_color = "green", three_outside_down_color = "red",
                               point_size = 3, three_outside_up_shape = 24, three_outside_down_shape = 25,
                               point_alpha = 0.8, mark_at_close = TRUE) {
  # Validate csp_three_outside_result
  if (!is.data.frame(csp_three_outside_result) && !xts::is.xts(csp_three_outside_result)) {
    stop("csp_three_outside_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_three_outside_result)) {
    csp_three_outside_result <- data.frame(
      Date = zoo::index(csp_three_outside_result),
      as.data.frame(csp_three_outside_result)
    )
  }

  if (!"Date" %in% colnames(csp_three_outside_result)) {
    stop("csp_three_outside_result must contain a 'Date' column")
  }

  required_cols <- c("ThreeOutsideUp", "ThreeOutsideDown")
  if (!all(required_cols %in% colnames(csp_three_outside_result))) {
    stop("csp_three_outside_result must contain 'ThreeOutsideUp' and 'ThreeOutsideDown' columns")
  }

  # Convert Date to proper format if needed
  csp_three_outside_result$Date <- as.Date(csp_three_outside_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_three_outside_result, by = "Date", all.x = TRUE)

  # Extract different types of Three Outside points
  three_outside_up_points <- merged_data[merged_data$ThreeOutsideUp == TRUE & !is.na(merged_data$ThreeOutsideUp), ]
  three_outside_down_points <- merged_data[merged_data$ThreeOutsideDown == TRUE & !is.na(merged_data$ThreeOutsideDown), ]

  # Determine y-value for marking
  if (mark_at_close) {
    # Mark at Close price
    three_outside_up_points$ThreeOutsideLevel <- three_outside_up_points$Close
    three_outside_down_points$ThreeOutsideLevel <- three_outside_down_points$Close
  } else {
    # Mark at the midpoint of the candle
    three_outside_up_points$ThreeOutsideLevel <- (three_outside_up_points$High + three_outside_up_points$Low) / 2
    three_outside_down_points$ThreeOutsideLevel <- (three_outside_down_points$High + three_outside_down_points$Low) / 2
  }

  # Add Three Outside points to the price plot
  price_plot_with_three_outside <- eCandleSticks_result$price_plot

  # Add Three Outside Up points (if any)
  if (nrow(three_outside_up_points) > 0) {
    price_plot_with_three_outside <- price_plot_with_three_outside +
      ggplot2::geom_point(
        data = three_outside_up_points,
        aes(x = Date, y = ThreeOutsideLevel, color = "Three Outside Up"),
        size = point_size,
        shape = three_outside_up_shape,
        alpha = point_alpha
      )
  }

  # Add Three Outside Down points (if any)
  if (nrow(three_outside_down_points) > 0) {
    price_plot_with_three_outside <- price_plot_with_three_outside +
      ggplot2::geom_point(
        data = three_outside_down_points,
        aes(x = Date, y = ThreeOutsideLevel, color = "Three Outside Down"),
        size = point_size,
        shape = three_outside_down_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Three Outside points
  if (nrow(three_outside_up_points) > 0 || nrow(three_outside_down_points) > 0) {
    price_plot_with_three_outside <- price_plot_with_three_outside +
      ggplot2::scale_color_manual(
        name = "CSP Three Outside Patterns",
        values = c(
          "Three Outside Up" = three_outside_up_color,
          "Three Outside Down" = three_outside_down_color
        ),
        breaks = c("Three Outside Up", "Three Outside Down")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(three_outside_up_shape, three_outside_down_shape),
            size = rep(point_size, 2),
            alpha = rep(point_alpha, 2)
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_three_outside

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_three_outside, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_three_outside
  }

  # Add csp_three_outside_result to the output for reference
  eCandleSticks_result$csp_three_outside_data <- csp_three_outside_result
  eCandleSticks_result$three_outside_up_points <- three_outside_up_points
  eCandleSticks_result$three_outside_down_points <- three_outside_down_points

  return(eCandleSticks_result)
}
