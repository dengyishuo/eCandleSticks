#' Add CSP Harami Points to Candlestick Chart
#'
#' This function adds CSP Harami points (Bullish Harami and Bearish Harami)
#' to a candlestick chart created by eCandleSticks using the results from a CSP Harami analysis,
#' and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_harami_result The result object from CSP Harami analysis, which should be a data frame
#'        containing a 'Date' column and logical columns 'Bull.Harami' and 'Bear.Harami'
#' @param bullish_harami_color Color for Bullish Harami points. Default "green".
#' @param bearish_harami_color Color for Bearish Harami points. Default "red".
#' @param point_size Size for the Harami points. Default 3.
#' @param bullish_harami_shape Shape for Bullish Harami points. Default 21 (filled circle).
#' @param bearish_harami_shape Shape for Bearish Harami points. Default 22 (filled square).
#' @param point_alpha Alpha transparency for the Harami points. Default 0.8.
#' @param mark_at_close Whether to mark at the close price instead of the midpoint. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Harami points added to the price plot
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
#' # Get CSP Harami results
#' csp_harami_data <- CSPHarami(AAPL) # This returns a data frame with Bull.Harami and Bear.Harami columns
#'
#' # Add Harami points
#' result_with_harami <- addCSPHarami(result, csp_harami_data)
#'
#' # Display the combined plot with Harami points
#' print(result_with_harami$combined_plot)
#' }
addCSPHarami <- function(eCandleSticks_result, csp_harami_result,
                         bullish_harami_color = "green", bearish_harami_color = "red",
                         point_size = 3, bullish_harami_shape = 21, bearish_harami_shape = 22,
                         point_alpha = 0.8, mark_at_close = TRUE) {
  # Validate csp_harami_result
  if (!is.data.frame(csp_harami_result) && !xts::is.xts(csp_harami_result)) {
    stop("csp_harami_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_harami_result)) {
    csp_harami_result <- data.frame(
      Date = zoo::index(csp_harami_result),
      as.data.frame(csp_harami_result)
    )
  }

  if (!"Date" %in% colnames(csp_harami_result)) {
    stop("csp_harami_result must contain a 'Date' column")
  }

  required_cols <- c("Bull.Harami", "Bear.Harami")
  if (!all(required_cols %in% colnames(csp_harami_result))) {
    stop("csp_harami_result must contain 'Bull.Harami' and 'Bear.Harami' columns")
  }

  # Convert Date to proper format if needed
  csp_harami_result$Date <- as.Date(csp_harami_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_harami_result, by = "Date", all.x = TRUE)

  # Extract different types of Harami points
  bullish_harami_points <- merged_data[merged_data$Bull.Harami == TRUE & !is.na(merged_data$Bull.Harami), ]
  bearish_harami_points <- merged_data[merged_data$Bear.Harami == TRUE & !is.na(merged_data$Bear.Harami), ]

  # Determine y-value for marking
  if (mark_at_close) {
    # Mark at Close price
    bullish_harami_points$HaramiLevel <- bullish_harami_points$Close
    bearish_harami_points$HaramiLevel <- bearish_harami_points$Close
  } else {
    # Mark at the midpoint of the candle
    bullish_harami_points$HaramiLevel <- (bullish_harami_points$High + bullish_harami_points$Low) / 2
    bearish_harami_points$HaramiLevel <- (bearish_harami_points$High + bearish_harami_points$Low) / 2
  }

  # Add Harami points to the price plot
  price_plot_with_harami <- eCandleSticks_result$price_plot

  # Add Bullish Harami points (if any)
  if (nrow(bullish_harami_points) > 0) {
    price_plot_with_harami <- price_plot_with_harami +
      ggplot2::geom_point(
        data = bullish_harami_points,
        aes(x = Date, y = HaramiLevel, color = "Bullish Harami"),
        size = point_size,
        shape = bullish_harami_shape,
        alpha = point_alpha
      )
  }

  # Add Bearish Harami points (if any)
  if (nrow(bearish_harami_points) > 0) {
    price_plot_with_harami <- price_plot_with_harami +
      ggplot2::geom_point(
        data = bearish_harami_points,
        aes(x = Date, y = HaramiLevel, color = "Bearish Harami"),
        size = point_size,
        shape = bearish_harami_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Harami points
  if (nrow(bullish_harami_points) > 0 || nrow(bearish_harami_points) > 0) {
    price_plot_with_harami <- price_plot_with_harami +
      ggplot2::scale_color_manual(
        name = "CSP Harami Patterns",
        values = c(
          "Bullish Harami" = bullish_harami_color,
          "Bearish Harami" = bearish_harami_color
        ),
        breaks = c("Bullish Harami", "Bearish Harami")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(bullish_harami_shape, bearish_harami_shape),
            size = rep(point_size, 2),
            alpha = rep(point_alpha, 2)
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_harami

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_harami, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_harami
  }

  # Add csp_harami_result to the output for reference
  eCandleSticks_result$csp_harami_data <- csp_harami_result
  eCandleSticks_result$bullish_harami_points <- bullish_harami_points
  eCandleSticks_result$bearish_harami_points <- bearish_harami_points

  return(eCandleSticks_result)
}
