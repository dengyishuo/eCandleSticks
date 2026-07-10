#' Add CSP Kicking Points to Candlestick Chart
#'
#' This function adds CSP Kicking points (Bullish Kicking and Bearish Kicking)
#' to a candlestick chart created by eCandleSticks using the results from a CSP Kicking analysis,
#' and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_kicking_result The result object from CSP Kicking analysis, which should be a data frame
#'        containing a 'Date' column and logical columns 'Bull.Kicking' and 'Bear.Kicking'
#' @param bullish_kicking_color Color for Bullish Kicking points. Default "green".
#' @param bearish_kicking_color Color for Bearish Kicking points. Default "red".
#' @param point_size Size for the Kicking points. Default 3.
#' @param bullish_kicking_shape Shape for Bullish Kicking points. Default 10 (plus sign).
#' @param bearish_kicking_shape Shape for Bearish Kicking points. Default 12 (square with cross).
#' @param point_alpha Alpha transparency for the Kicking points. Default 0.8.
#' @param mark_at_close Whether to mark at the close price. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Kicking points added to the price plot
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
#' # Get CSP Kicking results
#' csp_kicking_data <- addCSPKicking(AAPL)
#'
#' # Add Kicking points
#' result_with_kicking <- drawCSPKicking(result, csp_kicking_data)
#'
#' # Display the combined plot with Kicking points
#' print(result_with_kicking$combined_plot)
#' }
drawCSPKicking <- function(eCandleSticks_result, csp_kicking_result,
                          bullish_kicking_color = "green", bearish_kicking_color = "red",
                          point_size = 3, bullish_kicking_shape = 10, bearish_kicking_shape = 12,
                          point_alpha = 0.8, mark_at_close = TRUE) {
  # Validate csp_kicking_result
  if (!is.data.frame(csp_kicking_result) && !xts::is.xts(csp_kicking_result)) {
    stop("csp_kicking_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_kicking_result)) {
    csp_kicking_result <- data.frame(
      Date = zoo::index(csp_kicking_result),
      as.data.frame(csp_kicking_result)
    )
  }

  if (!"Date" %in% colnames(csp_kicking_result)) {
    stop("csp_kicking_result must contain a 'Date' column")
  }

  required_cols <- c("Bull.Kicking", "Bear.Kicking")
  if (!all(required_cols %in% colnames(csp_kicking_result))) {
    stop("csp_kicking_result must contain 'Bull.Kicking' and 'Bear.Kicking' columns")
  }

  # Convert Date to proper format if needed
  csp_kicking_result$Date <- as.Date(csp_kicking_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_kicking_result, by = "Date", all.x = TRUE)

  # Extract different types of Kicking points
  bullish_kicking_points <- merged_data[merged_data$Bull.Kicking == TRUE & !is.na(merged_data$Bull.Kicking), ]
  bearish_kicking_points <- merged_data[merged_data$Bear.Kicking == TRUE & !is.na(merged_data$Bear.Kicking), ]

  # Determine y-value for marking
  if (mark_at_close) {
    # Mark at Close price
    bullish_kicking_points$KickingLevel <- bullish_kicking_points$Close
    bearish_kicking_points$KickingLevel <- bearish_kicking_points$Close
  } else {
    # Mark at the midpoint of the candle
    bullish_kicking_points$KickingLevel <- (bullish_kicking_points$High + bullish_kicking_points$Low) / 2
    bearish_kicking_points$KickingLevel <- (bearish_kicking_points$High + bearish_kicking_points$Low) / 2
  }

  # Add Kicking points to the price plot
  price_plot_with_kicking <- eCandleSticks_result$price_plot

  # Add Bullish Kicking points (if any)
  if (nrow(bullish_kicking_points) > 0) {
    price_plot_with_kicking <- price_plot_with_kicking +
      ggplot2::geom_point(
        data = bullish_kicking_points,
        aes(x = Date, y = KickingLevel, color = "Bullish Kicking"),
        size = point_size,
        shape = bullish_kicking_shape,
        alpha = point_alpha
      )
  }

  # Add Bearish Kicking points (if any)
  if (nrow(bearish_kicking_points) > 0) {
    price_plot_with_kicking <- price_plot_with_kicking +
      ggplot2::geom_point(
        data = bearish_kicking_points,
        aes(x = Date, y = KickingLevel, color = "Bearish Kicking"),
        size = point_size,
        shape = bearish_kicking_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Kicking points
  if (nrow(bullish_kicking_points) > 0 || nrow(bearish_kicking_points) > 0) {
    price_plot_with_kicking <- price_plot_with_kicking +
      ggplot2::scale_color_manual(
        name = "CSP Kicking Patterns",
        values = c(
          "Bullish Kicking" = bullish_kicking_color,
          "Bearish Kicking" = bearish_kicking_color
        ),
        breaks = c("Bullish Kicking", "Bearish Kicking")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(bullish_kicking_shape, bearish_kicking_shape),
            size = rep(point_size, 2),
            alpha = rep(point_alpha, 2)
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_kicking

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_kicking, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_kicking
  }

  # Add csp_kicking_result to the output for reference
  eCandleSticks_result$csp_kicking_data <- csp_kicking_result
  eCandleSticks_result$bullish_kicking_points <- bullish_kicking_points
  eCandleSticks_result$bearish_kicking_points <- bearish_kicking_points

  return(eCandleSticks_result)
}
