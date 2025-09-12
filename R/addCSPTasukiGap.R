#' Add CSP Tasuki Gap Points to Candlestick Chart
#'
#' This function adds CSP Tasuki Gap points (Upside Tasuki Gap and Downside Tasuki Gap)
#' to a candlestick chart created by eCandleSticks using the results from a CSP Tasuki Gap analysis,
#' and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_tasuki_gap_result The result object from CSP Tasuki Gap analysis,
#'        which should be a data frame containing a 'Date' column and logical columns
#'        'UpsideTasukiGap' and 'DownsideTasukiGap'
#' @param upside_tasuki_gap_color Color for Upside Tasuki Gap points. Default "green".
#' @param downside_tasuki_gap_color Color for Downside Tasuki Gap points. Default "red".
#' @param point_size Size for the Tasuki Gap points. Default 3.
#' @param upside_tasuki_gap_shape Shape for Upside Tasuki Gap points. Default 24 (up triangle).
#' @param downside_tasuki_gap_shape Shape for Downside Tasuki Gap points. Default 25 (down triangle).
#' @param point_alpha Alpha transparency for the Tasuki Gap points. Default 0.8.
#' @param mark_at_close Whether to mark at the close price. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Tasuki Gap points added to the price plot
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
#' # Get CSP Tasuki Gap results
#' csp_tasuki_gap_data <- CSPTasukiGap(AAPL) # This returns a data frame with UpsideTasukiGap and DownsideTasukiGap columns
#'
#' # Add Tasuki Gap points
#' result_with_tasuki_gap <- addCSPTasukiGap(result, csp_tasuki_gap_data)
#'
#' # Display the combined plot with Tasuki Gap points
#' print(result_with_tasuki_gap$combined_plot)
#' }
addCSPTasukiGap <- function(eCandleSticks_result, csp_tasuki_gap_result,
                            upside_tasuki_gap_color = "green", downside_tasuki_gap_color = "red",
                            point_size = 3, upside_tasuki_gap_shape = 24, downside_tasuki_gap_shape = 25,
                            point_alpha = 0.8, mark_at_close = TRUE) {
  # Validate csp_tasuki_gap_result
  if (!is.data.frame(csp_tasuki_gap_result) && !xts::is.xts(csp_tasuki_gap_result)) {
    stop("csp_tasuki_gap_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_tasuki_gap_result)) {
    csp_tasuki_gap_result <- data.frame(
      Date = zoo::index(csp_tasuki_gap_result),
      as.data.frame(csp_tasuki_gap_result)
    )
  }

  if (!"Date" %in% colnames(csp_tasuki_gap_result)) {
    stop("csp_tasuki_gap_result must contain a 'Date' column")
  }

  required_cols <- c("UpsideTasukiGap", "DownsideTasukiGap")
  if (!all(required_cols %in% colnames(csp_tasuki_gap_result))) {
    stop("csp_tasuki_gap_result must contain 'UpsideTasukiGap' and 'DownsideTasukiGap' columns")
  }

  # Convert Date to proper format if needed
  csp_tasuki_gap_result$Date <- as.Date(csp_tasuki_gap_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_tasuki_gap_result, by = "Date", all.x = TRUE)

  # Extract different types of Tasuki Gap points
  upside_tasuki_gap_points <- merged_data[merged_data$UpsideTasukiGap == TRUE & !is.na(merged_data$UpsideTasukiGap), ]
  downside_tasuki_gap_points <- merged_data[merged_data$DownsideTasukiGap == TRUE & !is.na(merged_data$DownsideTasukiGap), ]

  # Determine y-value for marking
  if (mark_at_close) {
    # Mark at Close price
    upside_tasuki_gap_points$TasukiGapLevel <- upside_tasuki_gap_points$Close
    downside_tasuki_gap_points$TasukiGapLevel <- downside_tasuki_gap_points$Close
  } else {
    # Mark at the midpoint of the candle
    upside_tasuki_gap_points$TasukiGapLevel <- (upside_tasuki_gap_points$High + upside_tasuki_gap_points$Low) / 2
    downside_tasuki_gap_points$TasukiGapLevel <- (downside_tasuki_gap_points$High + downside_tasuki_gap_points$Low) / 2
  }

  # Add Tasuki Gap points to the price plot
  price_plot_with_tasuki_gap <- eCandleSticks_result$price_plot

  # Add Upside Tasuki Gap points (if any)
  if (nrow(upside_tasuki_gap_points) > 0) {
    price_plot_with_tasuki_gap <- price_plot_with_tasuki_gap +
      ggplot2::geom_point(
        data = upside_tasuki_gap_points,
        aes(x = Date, y = TasukiGapLevel, color = "Upside Tasuki Gap"),
        size = point_size,
        shape = upside_tasuki_gap_shape,
        alpha = point_alpha
      )
  }

  # Add Downside Tasuki Gap points (if any)
  if (nrow(downside_tasuki_gap_points) > 0) {
    price_plot_with_tasuki_gap <- price_plot_with_tasuki_gap +
      ggplot2::geom_point(
        data = downside_tasuki_gap_points,
        aes(x = Date, y = TasukiGapLevel, color = "Downside Tasuki Gap"),
        size = point_size,
        shape = downside_tasuki_gap_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Tasuki Gap points
  if (nrow(upside_tasuki_gap_points) > 0 || nrow(downside_tasuki_gap_points) > 0) {
    price_plot_with_tasuki_gap <- price_plot_with_tasuki_gap +
      ggplot2::scale_color_manual(
        name = "CSP Tasuki Gap Patterns",
        values = c(
          "Upside Tasuki Gap" = upside_tasuki_gap_color,
          "Downside Tasuki Gap" = downside_tasuki_gap_color
        ),
        breaks = c("Upside Tasuki Gap", "Downside Tasuki Gap")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(upside_tasuki_gap_shape, downside_tasuki_gap_shape),
            size = rep(point_size, 2),
            alpha = rep(point_alpha, 2)
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_tasuki_gap

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_tasuki_gap, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_tasuki_gap
  }

  # Add csp_tasuki_gap_result to the output for reference
  eCandleSticks_result$csp_tasuki_gap_data <- csp_tasuki_gap_result
  eCandleSticks_result$upside_tasuki_gap_points <- upside_tasuki_gap_points
  eCandleSticks_result$downside_tasuki_gap_points <- downside_tasuki_gap_points

  return(eCandleSticks_result)
}
