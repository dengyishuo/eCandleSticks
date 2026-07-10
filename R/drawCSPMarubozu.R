#' Add CSP Marubozu Points to Candlestick Chart
#'
#' This function adds CSP Marubozu points (White Marubozu and Black Marubozu)
#' to a candlestick chart created by eCandleSticks using the results from a CSP Marubozu analysis,
#' and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_marubozu_result The result object from CSP Marubozu analysis, which should be a data frame
#'        containing a 'Date' column and logical columns 'WhiteMarubozu' and 'BlackMarubozu'
#' @param white_marubozu_color Color for White Marubozu points. Default "green".
#' @param black_marubozu_color Color for Black Marubozu points. Default "red".
#' @param point_size Size for the Marubozu points. Default 3.
#' @param white_marubozu_shape Shape for White Marubozu points. Default 15 (filled square).
#' @param black_marubozu_shape Shape for Black Marubozu points. Default 15 (filled square).
#' @param point_alpha Alpha transparency for the Marubozu points. Default 0.8.
#' @param mark_at_body_center Whether to mark at the center of the candle body instead of Close price. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Marubozu points added to the price plot
#' and the combined plot updated accordingly.
#' @family pattern-1bar
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
#' # Get CSP Marubozu results
#' csp_marubozu_data <- addCSPMarubozu(AAPL)
#'
#' # Add Marubozu points
#' result_with_marubozu <- drawCSPMarubozu(result, csp_marubozu_data)
#'
#' # Display the combined plot with Marubozu points
#' print(result_with_marubozu$combined_plot)
#' }
drawCSPMarubozu <- function(eCandleSticks_result, csp_marubozu_result,
                           white_marubozu_color = "green", black_marubozu_color = "red",
                           point_size = 3, white_marubozu_shape = 15, black_marubozu_shape = 15,
                           point_alpha = 0.8, mark_at_body_center = TRUE) {
  # Validate csp_marubozu_result
  if (!is.data.frame(csp_marubozu_result) && !xts::is.xts(csp_marubozu_result)) {
    stop("csp_marubozu_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_marubozu_result)) {
    csp_marubozu_result <- data.frame(
      Date = zoo::index(csp_marubozu_result),
      as.data.frame(csp_marubozu_result)
    )
  }

  if (!"Date" %in% colnames(csp_marubozu_result)) {
    stop("csp_marubozu_result must contain a 'Date' column")
  }

  required_cols <- c("WhiteMarubozu", "BlackMarubozu")
  if (!all(required_cols %in% colnames(csp_marubozu_result))) {
    stop("csp_marubozu_result must contain 'WhiteMarubozu' and 'BlackMarubozu' columns")
  }

  # Convert Date to proper format if needed
  csp_marubozu_result$Date <- as.Date(csp_marubozu_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_marubozu_result, by = "Date", all.x = TRUE)

  # Extract different types of Marubozu points
  white_marubozu_points <- merged_data[merged_data$WhiteMarubozu == TRUE & !is.na(merged_data$WhiteMarubozu), ]
  black_marubozu_points <- merged_data[merged_data$BlackMarubozu == TRUE & !is.na(merged_data$BlackMarubozu), ]

  # Determine y-value for marking
  if (mark_at_body_center) {
    # Mark at the center of the candle body
    white_marubozu_points$MarubozuLevel <- (white_marubozu_points$Open + white_marubozu_points$Close) / 2
    black_marubozu_points$MarubozuLevel <- (black_marubozu_points$Open + black_marubozu_points$Close) / 2
  } else {
    # Mark at Close price
    white_marubozu_points$MarubozuLevel <- white_marubozu_points$Close
    black_marubozu_points$MarubozuLevel <- black_marubozu_points$Close
  }

  # Add Marubozu points to the price plot
  price_plot_with_marubozu <- eCandleSticks_result$price_plot

  # Add White Marubozu points (if any)
  if (nrow(white_marubozu_points) > 0) {
    price_plot_with_marubozu <- price_plot_with_marubozu +
      ggplot2::geom_point(
        data = white_marubozu_points,
        aes(x = Date, y = MarubozuLevel, color = "White Marubozu"),
        size = point_size,
        shape = white_marubozu_shape,
        alpha = point_alpha
      )
  }

  # Add Black Marubozu points (if any)
  if (nrow(black_marubozu_points) > 0) {
    price_plot_with_marubozu <- price_plot_with_marubozu +
      ggplot2::geom_point(
        data = black_marubozu_points,
        aes(x = Date, y = MarubozuLevel, color = "Black Marubozu"),
        size = point_size,
        shape = black_marubozu_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Marubozu points
  if (nrow(white_marubozu_points) > 0 || nrow(black_marubozu_points) > 0) {
    price_plot_with_marubozu <- price_plot_with_marubozu +
      ggplot2::scale_color_manual(
        name = "CSP Marubozu Patterns",
        values = c(
          "White Marubozu" = white_marubozu_color,
          "Black Marubozu" = black_marubozu_color
        ),
        breaks = c("White Marubozu", "Black Marubozu")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(white_marubozu_shape, black_marubozu_shape),
            size = rep(point_size, 2),
            alpha = rep(point_alpha, 2)
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_marubozu

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_marubozu, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_marubozu
  }

  # Add csp_marubozu_result to the output for reference
  eCandleSticks_result$csp_marubozu_data <- csp_marubozu_result
  eCandleSticks_result$white_marubozu_points <- white_marubozu_points
  eCandleSticks_result$black_marubozu_points <- black_marubozu_points

  return(eCandleSticks_result)
}
