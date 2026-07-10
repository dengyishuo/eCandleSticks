#' Add CSP Three Inside Points to Candlestick Chart
#'
#' This function adds CSP Three Inside points (Three Inside Up and Three Inside Down)
#' to a candlestick chart created by eCandleSticks using the results from a CSP Three Inside analysis,
#' and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_three_inside_result The result object from CSP Three Inside analysis,
#'        which should be a data frame containing a 'Date' column and logical columns
#'        'ThreeInsideUp' and 'ThreeInsideDown'
#' @param three_inside_up_color Color for Three Inside Up points. Default "green".
#' @param three_inside_down_color Color for Three Inside Down points. Default "red".
#' @param point_size Size for the Three Inside points. Default 3.
#' @param three_inside_up_shape Shape for Three Inside Up points. Default 24 (up triangle).
#' @param three_inside_down_shape Shape for Three Inside Down points. Default 25 (down triangle).
#' @param point_alpha Alpha transparency for the Three Inside points. Default 0.8.
#' @param mark_at_close Whether to mark at the close price. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Three Inside points added to the price plot
#' and the combined plot updated accordingly.
#' @family pattern-3bar
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
#' # Get CSP Three Inside results
#' csp_three_inside_data <- addCSPThreeInside(AAPL)
#'
#' # Add Three Inside points
#' result_with_three_inside <- drawCSPThreeInside(result, csp_three_inside_data)
#'
#' # Display the combined plot with Three Inside points
#' print(result_with_three_inside$combined_plot)
#' }
drawCSPThreeInside <- function(eCandleSticks_result, csp_three_inside_result,
                              three_inside_up_color = "green", three_inside_down_color = "red",
                              point_size = 3, three_inside_up_shape = 24, three_inside_down_shape = 25,
                              point_alpha = 0.8, mark_at_close = TRUE) {
  # Validate csp_three_inside_result
  if (!is.data.frame(csp_three_inside_result) && !xts::is.xts(csp_three_inside_result)) {
    stop("csp_three_inside_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_three_inside_result)) {
    csp_three_inside_result <- data.frame(
      Date = zoo::index(csp_three_inside_result),
      as.data.frame(csp_three_inside_result)
    )
  }

  if (!"Date" %in% colnames(csp_three_inside_result)) {
    stop("csp_three_inside_result must contain a 'Date' column")
  }

  required_cols <- c("ThreeInsideUp", "ThreeInsideDown")
  if (!all(required_cols %in% colnames(csp_three_inside_result))) {
    stop("csp_three_inside_result must contain 'ThreeInsideUp' and 'ThreeInsideDown' columns")
  }

  # Convert Date to proper format if needed
  csp_three_inside_result$Date <- as.Date(csp_three_inside_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_three_inside_result, by = "Date", all.x = TRUE)

  # Extract different types of Three Inside points
  three_inside_up_points <- merged_data[merged_data$ThreeInsideUp == TRUE & !is.na(merged_data$ThreeInsideUp), ]
  three_inside_down_points <- merged_data[merged_data$ThreeInsideDown == TRUE & !is.na(merged_data$ThreeInsideDown), ]

  # Determine y-value for marking
  if (mark_at_close) {
    # Mark at Close price
    three_inside_up_points$ThreeInsideLevel <- three_inside_up_points$Close
    three_inside_down_points$ThreeInsideLevel <- three_inside_down_points$Close
  } else {
    # Mark at the midpoint of the candle
    three_inside_up_points$ThreeInsideLevel <- (three_inside_up_points$High + three_inside_up_points$Low) / 2
    three_inside_down_points$ThreeInsideLevel <- (three_inside_down_points$High + three_inside_down_points$Low) / 2
  }

  # Add Three Inside points to the price plot
  price_plot_with_three_inside <- eCandleSticks_result$price_plot

  # Add Three Inside Up points (if any)
  if (nrow(three_inside_up_points) > 0) {
    price_plot_with_three_inside <- price_plot_with_three_inside +
      ggplot2::geom_point(
        data = three_inside_up_points,
        aes(x = Date, y = ThreeInsideLevel, color = "Three Inside Up"),
        size = point_size,
        shape = three_inside_up_shape,
        alpha = point_alpha
      )
  }

  # Add Three Inside Down points (if any)
  if (nrow(three_inside_down_points) > 0) {
    price_plot_with_three_inside <- price_plot_with_three_inside +
      ggplot2::geom_point(
        data = three_inside_down_points,
        aes(x = Date, y = ThreeInsideLevel, color = "Three Inside Down"),
        size = point_size,
        shape = three_inside_down_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Three Inside points
  if (nrow(three_inside_up_points) > 0 || nrow(three_inside_down_points) > 0) {
    price_plot_with_three_inside <- price_plot_with_three_inside +
      ggplot2::scale_color_manual(
        name = "CSP Three Inside Patterns",
        values = c(
          "Three Inside Up" = three_inside_up_color,
          "Three Inside Down" = three_inside_down_color
        ),
        breaks = c("Three Inside Up", "Three Inside Down")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(three_inside_up_shape, three_inside_down_shape),
            size = rep(point_size, 2),
            alpha = rep(point_alpha, 2)
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_three_inside

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_three_inside, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_three_inside
  }

  # Add csp_three_inside_result to the output for reference
  eCandleSticks_result$csp_three_inside_data <- csp_three_inside_result
  eCandleSticks_result$three_inside_up_points <- three_inside_up_points
  eCandleSticks_result$three_inside_down_points <- three_inside_down_points

  return(eCandleSticks_result)
}
