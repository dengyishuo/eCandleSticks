#' Add CSP Short Candle Body Points to Candlestick Chart
#'
#' This function adds CSP Short Candle Body points (Short White Candle Body and Short Black Candle Body)
#' to a candlestick chart created by eCandleSticks using the results from a CSP Short Candle Body analysis,
#' and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_short_candle_body_result The result object from CSP Short Candle Body analysis,
#'        which should be a data frame containing a 'Date' column and logical columns
#'        'ShortWhiteCandleBody' and 'ShortBlackCandleBody'
#' @param short_white_candle_body_color Color for Short White Candle Body points. Default "lightgreen".
#' @param short_black_candle_body_color Color for Short Black Candle Body points. Default "pink".
#' @param point_size Size for the Short Candle Body points. Default 3.
#' @param short_white_candle_body_shape Shape for Short White Candle Body points. Default 20 (small filled circle).
#' @param short_black_candle_body_shape Shape for Short Black Candle Body points. Default 20 (small filled circle).
#' @param point_alpha Alpha transparency for the Short Candle Body points. Default 0.8.
#' @param mark_at_body_center Whether to mark at the center of the candle body instead of Close price. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Short Candle Body points added to the price plot
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
#' # Get CSP Short Candle Body results
#' csp_short_candle_body_data <- CSPShortCandleBody(AAPL) # This returns a data frame with ShortWhiteCandleBody and ShortBlackCandleBody columns
#'
#' # Add Short Candle Body points
#' result_with_short_candle_body <- addCSPShortCandleBody(result, csp_short_candle_body_data)
#'
#' # Display the combined plot with Short Candle Body points
#' print(result_with_short_candle_body$combined_plot)
#' }
addCSPShortCandleBody <- function(eCandleSticks_result, csp_short_candle_body_result,
                                  short_white_candle_body_color = "lightgreen", short_black_candle_body_color = "pink",
                                  point_size = 3, short_white_candle_body_shape = 20, short_black_candle_body_shape = 20,
                                  point_alpha = 0.8, mark_at_body_center = TRUE) {
  # Validate csp_short_candle_body_result
  if (!is.data.frame(csp_short_candle_body_result) && !xts::is.xts(csp_short_candle_body_result)) {
    stop("csp_short_candle_body_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_short_candle_body_result)) {
    csp_short_candle_body_result <- data.frame(
      Date = zoo::index(csp_short_candle_body_result),
      as.data.frame(csp_short_candle_body_result)
    )
  }

  if (!"Date" %in% colnames(csp_short_candle_body_result)) {
    stop("csp_short_candle_body_result must contain a 'Date' column")
  }

  required_cols <- c("ShortWhiteCandleBody", "ShortBlackCandleBody")
  if (!all(required_cols %in% colnames(csp_short_candle_body_result))) {
    stop("csp_short_candle_body_result must contain 'ShortWhiteCandleBody' and 'ShortBlackCandleBody' columns")
  }

  # Convert Date to proper format if needed
  csp_short_candle_body_result$Date <- as.Date(csp_short_candle_body_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_short_candle_body_result, by = "Date", all.x = TRUE)

  # Extract different types of Short Candle Body points
  short_white_candle_body_points <- merged_data[merged_data$ShortWhiteCandleBody == TRUE & !is.na(merged_data$ShortWhiteCandleBody), ]
  short_black_candle_body_points <- merged_data[merged_data$ShortBlackCandleBody == TRUE & !is.na(merged_data$ShortBlackCandleBody), ]

  # Determine y-value for marking
  if (mark_at_body_center) {
    # Mark at the center of the candle body
    short_white_candle_body_points$ShortCandleBodyLevel <- (short_white_candle_body_points$Open + short_white_candle_body_points$Close) / 2
    short_black_candle_body_points$ShortCandleBodyLevel <- (short_black_candle_body_points$Open + short_black_candle_body_points$Close) / 2
  } else {
    # Mark at Close price
    short_white_candle_body_points$ShortCandleBodyLevel <- short_white_candle_body_points$Close
    short_black_candle_body_points$ShortCandleBodyLevel <- short_black_candle_body_points$Close
  }

  # Add Short Candle Body points to the price plot
  price_plot_with_short_candle_body <- eCandleSticks_result$price_plot

  # Add Short White Candle Body points (if any)
  if (nrow(short_white_candle_body_points) > 0) {
    price_plot_with_short_candle_body <- price_plot_with_short_candle_body +
      ggplot2::geom_point(
        data = short_white_candle_body_points,
        aes(x = Date, y = ShortCandleBodyLevel, color = "Short White Candle Body"),
        size = point_size,
        shape = short_white_candle_body_shape,
        alpha = point_alpha
      )
  }

  # Add Short Black Candle Body points (if any)
  if (nrow(short_black_candle_body_points) > 0) {
    price_plot_with_short_candle_body <- price_plot_with_short_candle_body +
      ggplot2::geom_point(
        data = short_black_candle_body_points,
        aes(x = Date, y = ShortCandleBodyLevel, color = "Short Black Candle Body"),
        size = point_size,
        shape = short_black_candle_body_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Short Candle Body points
  if (nrow(short_white_candle_body_points) > 0 || nrow(short_black_candle_body_points) > 0) {
    price_plot_with_short_candle_body <- price_plot_with_short_candle_body +
      ggplot2::scale_color_manual(
        name = "CSP Short Candle Body Patterns",
        values = c(
          "Short White Candle Body" = short_white_candle_body_color,
          "Short Black Candle Body" = short_black_candle_body_color
        ),
        breaks = c("Short White Candle Body", "Short Black Candle Body")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(short_white_candle_body_shape, short_black_candle_body_shape),
            size = rep(point_size, 2),
            alpha = rep(point_alpha, 2)
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_short_candle_body

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_short_candle_body, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_short_candle_body
  }

  # Add csp_short_candle_body_result to the output for reference
  eCandleSticks_result$csp_short_candle_body_data <- csp_short_candle_body_result
  eCandleSticks_result$short_white_candle_body_points <- short_white_candle_body_points
  eCandleSticks_result$short_black_candle_body_points <- short_black_candle_body_points

  return(eCandleSticks_result)
}
