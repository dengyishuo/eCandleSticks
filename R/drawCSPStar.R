#' Add CSP Star Points to Candlestick Chart
#'
#' This function adds CSP Star points (MorningStar and EveningStar patterns) to a candlestick chart
#' created by eCandleSticks, and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_star_result The result object from CSP Star analysis, which should be a data frame
#'        containing a 'Date' column and logical columns 'MorningStar' and 'EveningStar'
#' @param morning_star_color Color for the Morning Star points. Default "green".
#' @param evening_star_color Color for the Evening Star points. Default "red".
#' @param point_size Size for the CSP Star points. Default 3.
#' @param point_shape Shape for the CSP Star points. Default 8 (asterisk).
#' @param point_alpha Alpha transparency for the CSP Star points. Default 0.8.
#'
#' @return A modified eCandleSticks result list with CSP Star points added to the price plot
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
#' # Get CSP Star results (this would be the actual output from CSPStar function)
#' csp_star_data <- addCSPStar(AAPL)
#'
#' # Add CSP Star points
#' result_with_stars <- drawCSPStar(result, csp_star_data)
#'
#' # Display the combined plot with stars
#' print(result_with_stars$combined_plot)
#' }
drawCSPStar <- function(eCandleSticks_result, csp_star_result,
                       morning_star_color = "green", evening_star_color = "red",
                       point_size = 3, point_shape = 8, point_alpha = 0.8) {
  # Validate csp_star_result
  if (!is.data.frame(csp_star_result) && !xts::is.xts(csp_star_result)) {
    stop("csp_star_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_star_result)) {
    csp_star_result <- data.frame(
      Date = zoo::index(csp_star_result),
      as.data.frame(csp_star_result)
    )
  }

  if (!"Date" %in% colnames(csp_star_result)) {
    stop("csp_star_result must contain a 'Date' column")
  }

  if (!"MorningStar" %in% colnames(csp_star_result) ||
    !"EveningStar" %in% colnames(csp_star_result)) {
    stop("csp_star_result must contain 'MorningStar' and 'EveningStar' columns")
  }

  # Convert Date to proper format if needed
  csp_star_result$Date <- as.Date(csp_star_result$Date)

  # Merge with the original data to get the Close prices
  merged_data <- merge(eCandleSticks_result$data, csp_star_result, by = "Date", all.x = TRUE)

  # Extract Morning Star points
  morning_star_points <- merged_data[merged_data$MorningStar == TRUE & !is.na(merged_data$MorningStar), ]

  # Extract Evening Star points
  evening_star_points <- merged_data[merged_data$EveningStar == TRUE & !is.na(merged_data$EveningStar), ]

  # Add CSP Star points to the price plot
  price_plot_with_stars <- eCandleSticks_result$price_plot

  # Add Morning Star points (if any)
  if (nrow(morning_star_points) > 0) {
    price_plot_with_stars <- price_plot_with_stars +
      ggplot2::geom_point(
        data = morning_star_points,
        aes(x = Date, y = Close, color = "Morning Star"),
        size = point_size,
        shape = point_shape,
        alpha = point_alpha
      )
  }

  # Add Evening Star points (if any)
  if (nrow(evening_star_points) > 0) {
    price_plot_with_stars <- price_plot_with_stars +
      ggplot2::geom_point(
        data = evening_star_points,
        aes(x = Date, y = Close, color = "Evening Star"),
        size = point_size,
        shape = point_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any star points
  if (nrow(morning_star_points) > 0 || nrow(evening_star_points) > 0) {
    price_plot_with_stars <- price_plot_with_stars +
      ggplot2::scale_color_manual(
        name = "CSP Star Patterns",
        values = c("Morning Star" = morning_star_color, "Evening Star" = evening_star_color),
        breaks = c("Morning Star", "Evening Star")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = point_shape,
            size = point_size,
            alpha = point_alpha
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_stars

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_stars, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_stars
  }

  # Add csp_star_result to the output for reference
  eCandleSticks_result$csp_star_data <- csp_star_result
  eCandleSticks_result$morning_star_points <- morning_star_points
  eCandleSticks_result$evening_star_points <- evening_star_points

  return(eCandleSticks_result)
}
