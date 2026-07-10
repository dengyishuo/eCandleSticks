#' Add CSP Short Candle Points to Candlestick Chart
#'
#' This function adds CSP Short Candle points (Short White Candle and Short Black Candle)
#' to a candlestick chart created by eCandleSticks using the results from a CSP Short Candle analysis,
#' and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_short_candle_result The result object from CSP Short Candle analysis,
#'        which should be a data frame containing a 'Date' column and logical columns
#'        'ShortWhiteCandle' and 'ShortBlackCandle'
#' @param short_white_candle_color Color for Short White Candle points. Default "lightgreen".
#' @param short_black_candle_color Color for Short Black Candle points. Default "pink".
#' @param point_size Size for the Short Candle points. Default 3.
#' @param short_white_candle_shape Shape for Short White Candle points. Default 20 (small filled circle).
#' @param short_black_candle_shape Shape for Short Black Candle points. Default 20 (small filled circle).
#' @param point_alpha Alpha transparency for the Short Candle points. Default 0.8.
#' @param mark_at_midpoint Whether to mark at the midpoint of the candle instead of Close price. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Short Candle points added to the price plot
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
#' # Get CSP Short Candle results
#' csp_short_candle_data <- addCSPShortCandle(AAPL)
#'
#' # Add Short Candle points
#' result_with_short_candle <- drawCSPShortCandle(result, csp_short_candle_data)
#'
#' # Display the combined plot with Short Candle points
#' print(result_with_short_candle$combined_plot)
#' }
drawCSPShortCandle <- function(eCandleSticks_result, csp_short_candle_result,
                              short_white_candle_color = "lightgreen", short_black_candle_color = "pink",
                              point_size = 3, short_white_candle_shape = 20, short_black_candle_shape = 20,
                              point_alpha = 0.8, mark_at_midpoint = TRUE) {
  # Validate csp_short_candle_result
  if (!is.data.frame(csp_short_candle_result) && !xts::is.xts(csp_short_candle_result)) {
    stop("csp_short_candle_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_short_candle_result)) {
    csp_short_candle_result <- data.frame(
      Date = zoo::index(csp_short_candle_result),
      as.data.frame(csp_short_candle_result)
    )
  }

  if (!"Date" %in% colnames(csp_short_candle_result)) {
    stop("csp_short_candle_result must contain a 'Date' column")
  }

  required_cols <- c("ShortWhiteCandle", "ShortBlackCandle")
  if (!all(required_cols %in% colnames(csp_short_candle_result))) {
    stop("csp_short_candle_result must contain 'ShortWhiteCandle' and 'ShortBlackCandle' columns")
  }

  # Convert Date to proper format if needed
  csp_short_candle_result$Date <- as.Date(csp_short_candle_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_short_candle_result, by = "Date", all.x = TRUE)

  # Extract different types of Short Candle points
  short_white_candle_points <- merged_data[merged_data$ShortWhiteCandle == TRUE & !is.na(merged_data$ShortWhiteCandle), ]
  short_black_candle_points <- merged_data[merged_data$ShortBlackCandle == TRUE & !is.na(merged_data$ShortBlackCandle), ]

  # Determine y-value for marking
  if (mark_at_midpoint) {
    # Mark at the midpoint of the candle
    short_white_candle_points$ShortCandleLevel <- (short_white_candle_points$High + short_white_candle_points$Low) / 2
    short_black_candle_points$ShortCandleLevel <- (short_black_candle_points$High + short_black_candle_points$Low) / 2
  } else {
    # Mark at Close price
    short_white_candle_points$ShortCandleLevel <- short_white_candle_points$Close
    short_black_candle_points$ShortCandleLevel <- short_black_candle_points$Close
  }

  # Add Short Candle points to the price plot
  price_plot_with_short_candle <- eCandleSticks_result$price_plot

  # Add Short White Candle points (if any)
  if (nrow(short_white_candle_points) > 0) {
    price_plot_with_short_candle <- price_plot_with_short_candle +
      ggplot2::geom_point(
        data = short_white_candle_points,
        aes(x = Date, y = ShortCandleLevel, color = "Short White Candle"),
        size = point_size,
        shape = short_white_candle_shape,
        alpha = point_alpha
      )
  }

  # Add Short Black Candle points (if any)
  if (nrow(short_black_candle_points) > 0) {
    price_plot_with_short_candle <- price_plot_with_short_candle +
      ggplot2::geom_point(
        data = short_black_candle_points,
        aes(x = Date, y = ShortCandleLevel, color = "Short Black Candle"),
        size = point_size,
        shape = short_black_candle_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Short Candle points
  if (nrow(short_white_candle_points) > 0 || nrow(short_black_candle_points) > 0) {
    price_plot_with_short_candle <- price_plot_with_short_candle +
      ggplot2::scale_color_manual(
        name = "CSP Short Candle Patterns",
        values = c(
          "Short White Candle" = short_white_candle_color,
          "Short Black Candle" = short_black_candle_color
        ),
        breaks = c("Short White Candle", "Short Black Candle")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(short_white_candle_shape, short_black_candle_shape),
            size = rep(point_size, 2),
            alpha = rep(point_alpha, 2)
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_short_candle

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_short_candle, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_short_candle
  }

  # Add csp_short_candle_result to the output for reference
  eCandleSticks_result$csp_short_candle_data <- csp_short_candle_result
  eCandleSticks_result$short_white_candle_points <- short_white_candle_points
  eCandleSticks_result$short_black_candle_points <- short_black_candle_points

  return(eCandleSticks_result)
}
