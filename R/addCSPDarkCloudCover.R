#' Add CSP Dark Cloud Cover Points to Candlestick Chart
#'
#' This function adds CSP Dark Cloud Cover points to a candlestick chart created by eCandleSticks
#' using the results from a CSP Dark Cloud Cover analysis, and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_dark_cloud_result The result object from CSP Dark Cloud Cover analysis, which should be a data frame
#'        containing a 'Date' column and a logical column 'DarkCloudCover'
#' @param point_color Color for the Dark Cloud Cover points. Default "purple".
#' @param point_size Size for the Dark Cloud Cover points. Default 3.
#' @param point_shape Shape for the Dark Cloud Cover points. Default 17 (triangle).
#' @param point_alpha Alpha transparency for the Dark Cloud Cover points. Default 0.8.
#'
#' @return A modified eCandleSticks result list with Dark Cloud Cover points added to the price plot
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
#' # Get CSP Dark Cloud Cover results
#' # This returns a data frame with DarkCloudCover column
#' csp_dark_cloud_data <- CSPDarkCloudCover(AAPL)
#'
#' # Add Dark Cloud Cover points
#' result_with_dark_cloud <- addCSPDarkCloudCover(result, csp_dark_cloud_data)
#'
#' # Display the combined plot with Dark Cloud Cover points
#' print(result_with_dark_cloud$combined_plot)
#' }
addCSPDarkCloudCover <- function(eCandleSticks_result, csp_dark_cloud_result,
                                 point_color = "purple", point_size = 3,
                                 point_shape = 17, point_alpha = 0.8) {
  # Validate csp_dark_cloud_result
  if (!is.data.frame(csp_dark_cloud_result) && !xts::is.xts(csp_dark_cloud_result)) {
    stop("csp_dark_cloud_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_dark_cloud_result)) {
    csp_dark_cloud_result <- data.frame(
      Date = zoo::index(csp_dark_cloud_result),
      as.data.frame(csp_dark_cloud_result)
    )
  }

  if (!"Date" %in% colnames(csp_dark_cloud_result)) {
    stop("csp_dark_cloud_result must contain a 'Date' column")
  }

  if (!"DarkCloudCover" %in% colnames(csp_dark_cloud_result)) {
    stop("csp_dark_cloud_result must contain a 'DarkCloudCover' column")
  }

  # Convert Date to proper format if needed
  csp_dark_cloud_result$Date <- as.Date(csp_dark_cloud_result$Date)

  # Merge with the original data to get the Close prices
  merged_data <- merge(eCandleSticks_result$data, csp_dark_cloud_result, by = "Date", all.x = TRUE)

  # Extract Dark Cloud Cover points
  dark_cloud_points <- merged_data[merged_data$DarkCloudCover == TRUE & !is.na(merged_data$DarkCloudCover), ]

  # Add Dark Cloud Cover points to the price plot
  price_plot_with_dark_cloud <- eCandleSticks_result$price_plot

  # Add Dark Cloud Cover points (if any)
  if (nrow(dark_cloud_points) > 0) {
    price_plot_with_dark_cloud <- price_plot_with_dark_cloud +
      ggplot2::geom_point(
        data = dark_cloud_points,
        aes(x = Date, y = Close, color = "Dark Cloud Cover"),
        size = point_size,
        shape = point_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Dark Cloud Cover points
  if (nrow(dark_cloud_points) > 0) {
    price_plot_with_dark_cloud <- price_plot_with_dark_cloud +
      ggplot2::scale_color_manual(
        name = "CSP Patterns",
        values = c("Dark Cloud Cover" = point_color),
        breaks = c("Dark Cloud Cover")
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
  eCandleSticks_result$price_plot <- price_plot_with_dark_cloud

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_dark_cloud, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_dark_cloud
  }

  # Add csp_dark_cloud_result to the output for reference
  eCandleSticks_result$csp_dark_cloud_data <- csp_dark_cloud_result
  eCandleSticks_result$dark_cloud_points <- dark_cloud_points

  return(eCandleSticks_result)
}
