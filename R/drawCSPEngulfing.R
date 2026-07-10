#' Add CSP Engulfing Points to Candlestick Chart
#'
#' This function adds CSP Engulfing points (Bullish Engulfing and Bearish Engulfing)
#' to a candlestick chart created by eCandleSticks using the results from a CSP Engulfing analysis,
#' and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_engulfing_result The result object from CSP Engulfing analysis, which should be a data frame
#'        containing a 'Date' column and logical columns 'Bull.Engulfing' and 'Bear.Engulfing'
#' @param bullish_engulfing_color Color for Bullish Engulfing points. Default "green".
#' @param bearish_engulfing_color Color for Bearish Engulfing points. Default "red".
#' @param point_size Size for the Engulfing points. Default 3.
#' @param bullish_engulfing_shape Shape for Bullish Engulfing points. Default 24 (up triangle).
#' @param bearish_engulfing_shape Shape for Bearish Engulfing points. Default 25 (down triangle).
#' @param point_alpha Alpha transparency for the Engulfing points. Default 0.8.
#'
#' @return A modified eCandleSticks result list with Engulfing points added to the price plot
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
#' # Get CSP Engulfing results
#' csp_engulfing_data <- addCSPEngulfing(AAPL)
#'
#' # Add Engulfing points
#' result_with_engulfing <- drawCSPEngulfing(result, csp_engulfing_data)
#'
#' # Display the combined plot with Engulfing points
#' print(result_with_engulfing$combined_plot)
#' }
drawCSPEngulfing <- function(eCandleSticks_result, csp_engulfing_result,
                            bullish_engulfing_color = "green", bearish_engulfing_color = "red",
                            point_size = 3, bullish_engulfing_shape = 24, bearish_engulfing_shape = 25,
                            point_alpha = 0.8) {
  # Validate csp_engulfing_result
  if (!is.data.frame(csp_engulfing_result) && !xts::is.xts(csp_engulfing_result)) {
    stop("csp_engulfing_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_engulfing_result)) {
    csp_engulfing_result <- data.frame(
      Date = zoo::index(csp_engulfing_result),
      as.data.frame(csp_engulfing_result)
    )
  }

  if (!"Date" %in% colnames(csp_engulfing_result)) {
    stop("csp_engulfing_result must contain a 'Date' column")
  }

  required_cols <- c("Bull.Engulfing", "Bear.Engulfing")
  if (!all(required_cols %in% colnames(csp_engulfing_result))) {
    stop("csp_engulfing_result must contain 'Bull.Engulfing' and 'Bear.Engulfing' columns")
  }

  # Convert Date to proper format if needed
  csp_engulfing_result$Date <- as.Date(csp_engulfing_result$Date)

  # Merge with the original data to get the Close prices
  merged_data <- merge(eCandleSticks_result$data, csp_engulfing_result, by = "Date", all.x = TRUE)

  # Extract different types of Engulfing points
  bullish_engulfing_points <- merged_data[merged_data$Bull.Engulfing == TRUE & !is.na(merged_data$Bull.Engulfing), ]
  bearish_engulfing_points <- merged_data[merged_data$Bear.Engulfing == TRUE & !is.na(merged_data$Bear.Engulfing), ]

  # Add Engulfing points to the price plot
  price_plot_with_engulfing <- eCandleSticks_result$price_plot

  # Add Bullish Engulfing points (if any)
  if (nrow(bullish_engulfing_points) > 0) {
    price_plot_with_engulfing <- price_plot_with_engulfing +
      ggplot2::geom_point(
        data = bullish_engulfing_points,
        aes(x = Date, y = Close, color = "Bullish Engulfing"),
        size = point_size,
        shape = bullish_engulfing_shape,
        alpha = point_alpha
      )
  }

  # Add Bearish Engulfing points (if any)
  if (nrow(bearish_engulfing_points) > 0) {
    price_plot_with_engulfing <- price_plot_with_engulfing +
      ggplot2::geom_point(
        data = bearish_engulfing_points,
        aes(x = Date, y = Close, color = "Bearish Engulfing"),
        size = point_size,
        shape = bearish_engulfing_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Engulfing points
  if (nrow(bullish_engulfing_points) > 0 || nrow(bearish_engulfing_points) > 0) {
    price_plot_with_engulfing <- price_plot_with_engulfing +
      ggplot2::scale_color_manual(
        name = "CSP Engulfing Patterns",
        values = c(
          "Bullish Engulfing" = bullish_engulfing_color,
          "Bearish Engulfing" = bearish_engulfing_color
        ),
        breaks = c("Bullish Engulfing", "Bearish Engulfing")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(bullish_engulfing_shape, bearish_engulfing_shape),
            size = rep(point_size, 2),
            alpha = rep(point_alpha, 2)
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_engulfing

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_engulfing, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_engulfing
  }

  # Add csp_engulfing_result to the output for reference
  eCandleSticks_result$csp_engulfing_data <- csp_engulfing_result
  eCandleSticks_result$bullish_engulfing_points <- bullish_engulfing_points
  eCandleSticks_result$bearish_engulfing_points <- bearish_engulfing_points

  return(eCandleSticks_result)
}
