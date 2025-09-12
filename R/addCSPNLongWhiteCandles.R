#' Add CSP N Long White Candles Points to Candlestick Chart
#'
#' This function adds CSP N Long White Candles points to a candlestick chart
#' created by eCandleSticks using the results from a CSP N Long White Candles analysis,
#' and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_n_long_white_result The result object from CSP N Long White Candles analysis,
#'        which should be a data frame containing a 'Date' column and a logical column
#'        'X2LongWhiteCandles' (or similar pattern for different N values)
#' @param n_long_white_color Color for N Long White Candles points. Default "green".
#' @param point_size Size for the N Long White Candles points. Default 3.
#' @param n_long_white_shape Shape for N Long White Candles points. Default 16 (filled circle).
#' @param point_alpha Alpha transparency for the N Long White Candles points. Default 0.8.
#' @param mark_at_close Whether to mark at the close price. Default TRUE.
#'
#' @return A modified eCandleSticks result list with N Long White Candles points added to the price plot
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
#' # Get CSP N Long White Candles results
#' csp_n_long_white_data <- CSPNLongWhiteCandles(AAPL) # This returns a data frame with X2LongWhiteCandles column
#'
#' # Add N Long White Candles points
#' result_with_n_long_white <- addCSPNLongWhiteCandles(result, csp_n_long_white_data)
#'
#' # Display the combined plot with N Long White Candles points
#' print(result_with_n_long_white$combined_plot)
#' }
addCSPNLongWhiteCandles <- function(eCandleSticks_result, csp_n_long_white_result,
                                    n_long_white_color = "green", point_size = 3,
                                    n_long_white_shape = 16, point_alpha = 0.8,
                                    mark_at_close = TRUE) {
  # Validate csp_n_long_white_result
  if (!is.data.frame(csp_n_long_white_result) && !xts::is.xts(csp_n_long_white_result)) {
    stop("csp_n_long_white_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_n_long_white_result)) {
    csp_n_long_white_result <- data.frame(
      Date = zoo::index(csp_n_long_white_result),
      as.data.frame(csp_n_long_white_result)
    )
  }

  if (!"Date" %in% colnames(csp_n_long_white_result)) {
    stop("csp_n_long_white_result must contain a 'Date' column")
  }

  # Find the column name that contains "LongWhiteCandles"
  pattern_col <- grep("LongWhiteCandles", colnames(csp_n_long_white_result), value = TRUE)
  if (length(pattern_col) == 0) {
    stop("csp_n_long_white_result must contain a column with 'LongWhiteCandles' in its name")
  }

  # Use the first matching column
  pattern_col_name <- pattern_col[1]

  # Extract the N value from the column name (e.g., "X2" from "X2LongWhiteCandles")
  n_value <- gsub("LongWhiteCandles", "", pattern_col_name)
  n_value <- gsub("X", "", n_value)
  pattern_name <- paste0(n_value, " Long White Candles")

  # Convert Date to proper format if needed
  csp_n_long_white_result$Date <- as.Date(csp_n_long_white_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_n_long_white_result, by = "Date", all.x = TRUE)

  # Extract N Long White Candles points
  n_long_white_points <- merged_data[merged_data[[pattern_col_name]] == TRUE & !is.na(merged_data[[pattern_col_name]]), ]

  # Determine y-value for marking
  if (mark_at_close) {
    # Mark at Close price
    n_long_white_points$NLongWhiteLevel <- n_long_white_points$Close
  } else {
    # Mark at the midpoint of the candle
    n_long_white_points$NLongWhiteLevel <- (n_long_white_points$High + n_long_white_points$Low) / 2
  }

  # Add N Long White Candles points to the price plot
  price_plot_with_n_long_white <- eCandleSticks_result$price_plot

  # Add N Long White Candles points (if any)
  if (nrow(n_long_white_points) > 0) {
    price_plot_with_n_long_white <- price_plot_with_n_long_white +
      ggplot2::geom_point(
        data = n_long_white_points,
        aes(x = Date, y = NLongWhiteLevel, color = pattern_name),
        size = point_size,
        shape = n_long_white_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any N Long White Candles points
  if (nrow(n_long_white_points) > 0) {
    price_plot_with_n_long_white <- price_plot_with_n_long_white +
      ggplot2::scale_color_manual(
        name = "CSP Patterns",
        values = setNames(n_long_white_color, pattern_name),
        breaks = pattern_name
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = n_long_white_shape,
            size = point_size,
            alpha = point_alpha
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_n_long_white

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_n_long_white, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_n_long_white
  }

  # Add csp_n_long_white_result to the output for reference
  eCandleSticks_result$csp_n_long_white_data <- csp_n_long_white_result
  eCandleSticks_result$n_long_white_points <- n_long_white_points
  eCandleSticks_result$pattern_name <- pattern_name

  return(eCandleSticks_result)
}
