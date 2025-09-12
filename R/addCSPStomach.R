#' Add CSP Stomach Points to Candlestick Chart
#'
#' This function adds CSP Stomach points (Above The Stomach and Below The Stomach)
#' to a candlestick chart created by eCandleSticks using the results from a CSP Stomach analysis,
#' and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_stomach_result The result object from CSP Stomach analysis,
#'        which should be a data frame containing a 'Date' column and logical columns
#'        'AboveTheStomach' and 'BelowTheStomach'
#' @param above_stomach_color Color for Above The Stomach points. Default "blue".
#' @param below_stomach_color Color for Below The Stomach points. Default "orange".
#' @param point_size Size for the Stomach points. Default 3.
#' @param above_stomach_shape Shape for Above The Stomach points. Default 24 (up triangle).
#' @param below_stomach_shape Shape for Below The Stomach points. Default 25 (down triangle).
#' @param point_alpha Alpha transparency for the Stomach points. Default 0.8.
#' @param mark_at_close Whether to mark at the close price. Default TRUE.
#'
#' @return A modified eCandleSticks result list with Stomach points added to the price plot
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
#' # Get CSP Stomach results
#' csp_stomach_data <- CSPStomach(AAPL) # This returns a data frame with AboveTheStomach and BelowTheStomach columns
#'
#' # Add Stomach points
#' result_with_stomach <- addCSPStomach(result, csp_stomach_data)
#'
#' # Display the combined plot with Stomach points
#' print(result_with_stomach$combined_plot)
#' }
addCSPStomach <- function(eCandleSticks_result, csp_stomach_result,
                          above_stomach_color = "blue", below_stomach_color = "orange",
                          point_size = 3, above_stomach_shape = 24, below_stomach_shape = 25,
                          point_alpha = 0.8, mark_at_close = TRUE) {
  # Validate csp_stomach_result
  if (!is.data.frame(csp_stomach_result) && !xts::is.xts(csp_stomach_result)) {
    stop("csp_stomach_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_stomach_result)) {
    csp_stomach_result <- data.frame(
      Date = zoo::index(csp_stomach_result),
      as.data.frame(csp_stomach_result)
    )
  }

  if (!"Date" %in% colnames(csp_stomach_result)) {
    stop("csp_stomach_result must contain a 'Date' column")
  }

  required_cols <- c("AboveTheStomach", "BelowTheStomach")
  if (!all(required_cols %in% colnames(csp_stomach_result))) {
    stop("csp_stomach_result must contain 'AboveTheStomach' and 'BelowTheStomach' columns")
  }

  # Convert Date to proper format if needed
  csp_stomach_result$Date <- as.Date(csp_stomach_result$Date)

  # Merge with the original data to get the OHLC prices
  merged_data <- merge(eCandleSticks_result$data, csp_stomach_result, by = "Date", all.x = TRUE)

  # Extract different types of Stomach points
  above_stomach_points <- merged_data[merged_data$AboveTheStomach == TRUE & !is.na(merged_data$AboveTheStomach), ]
  below_stomach_points <- merged_data[merged_data$BelowTheStomach == TRUE & !is.na(merged_data$BelowTheStomach), ]

  # Determine y-value for marking
  if (mark_at_close) {
    # Mark at Close price
    above_stomach_points$StomachLevel <- above_stomach_points$Close
    below_stomach_points$StomachLevel <- below_stomach_points$Close
  } else {
    # Mark at the midpoint of the candle
    above_stomach_points$StomachLevel <- (above_stomach_points$High + above_stomach_points$Low) / 2
    below_stomach_points$StomachLevel <- (below_stomach_points$High + below_stomach_points$Low) / 2
  }

  # Add Stomach points to the price plot
  price_plot_with_stomach <- eCandleSticks_result$price_plot

  # Add Above The Stomach points (if any)
  if (nrow(above_stomach_points) > 0) {
    price_plot_with_stomach <- price_plot_with_stomach +
      ggplot2::geom_point(
        data = above_stomach_points,
        aes(x = Date, y = StomachLevel, color = "Above The Stomach"),
        size = point_size,
        shape = above_stomach_shape,
        alpha = point_alpha
      )
  }

  # Add Below The Stomach points (if any)
  if (nrow(below_stomach_points) > 0) {
    price_plot_with_stomach <- price_plot_with_stomach +
      ggplot2::geom_point(
        data = below_stomach_points,
        aes(x = Date, y = StomachLevel, color = "Below The Stomach"),
        size = point_size,
        shape = below_stomach_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Stomach points
  if (nrow(above_stomach_points) > 0 || nrow(below_stomach_points) > 0) {
    price_plot_with_stomach <- price_plot_with_stomach +
      ggplot2::scale_color_manual(
        name = "CSP Stomach Patterns",
        values = c(
          "Above The Stomach" = above_stomach_color,
          "Below The Stomach" = below_stomach_color
        ),
        breaks = c("Above The Stomach", "Below The Stomach")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(above_stomach_shape, below_stomach_shape),
            size = rep(point_size, 2),
            alpha = rep(point_alpha, 2)
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_stomach

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_stomach, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_stomach
  }

  # Add csp_stomach_result to the output for reference
  eCandleSticks_result$csp_stomach_data <- csp_stomach_result
  eCandleSticks_result$above_stomach_points <- above_stomach_points
  eCandleSticks_result$below_stomach_points <- below_stomach_points

  return(eCandleSticks_result)
}
