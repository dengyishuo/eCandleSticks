#' Add CSP Doji Points to Candlestick Chart
#'
#' This function adds CSP Doji points (including Dragonfly Doji and Gravestone Doji)
#' to a candlestick chart created by eCandleSticks using the results from a CSP Doji analysis,
#' and recombines it with the volume subplot if it exists.
#'
#' @param eCandleSticks_result The result object returned by eCandleSticks function
#' @param csp_doji_result The result object from CSP Doji analysis, which should be a data frame
#'        containing a 'Date' column and logical columns 'Doji', 'DragonflyDoji', and 'GravestoneDoji'
#' @param doji_color Color for regular Doji points. Default "blue".
#' @param dragonfly_doji_color Color for Dragonfly Doji points. Default "green".
#' @param gravestone_doji_color Color for Gravestone Doji points. Default "orange".
#' @param point_size Size for the Doji points. Default 3.
#' @param doji_shape Shape for regular Doji points. Default 16 (circle).
#' @param dragonfly_doji_shape Shape for Dragonfly Doji points. Default 15 (square).
#' @param gravestone_doji_shape Shape for Gravestone Doji points. Default 17 (triangle).
#' @param point_alpha Alpha transparency for the Doji points. Default 0.8.
#'
#' @return A modified eCandleSticks result list with Doji points added to the price plot
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
#' # Get CSP Doji results
#' csp_doji_data <- addCSPDoji(AAPL)
#'
#' # Add Doji points
#' result_with_doji <- drawCSPDoji(result, csp_doji_data)
#'
#' # Display the combined plot with Doji points
#' print(result_with_doji$combined_plot)
#' }
drawCSPDoji <- function(eCandleSticks_result, csp_doji_result,
                       doji_color = "blue", dragonfly_doji_color = "green", gravestone_doji_color = "orange",
                       point_size = 3, doji_shape = 16, dragonfly_doji_shape = 15, gravestone_doji_shape = 17,
                       point_alpha = 0.8) {
  # Validate csp_doji_result
  if (!is.data.frame(csp_doji_result) && !xts::is.xts(csp_doji_result)) {
    stop("csp_doji_result must be a data frame or xts object")
  }

  # Convert to data frame if it's an xts object
  if (xts::is.xts(csp_doji_result)) {
    csp_doji_result <- data.frame(
      Date = zoo::index(csp_doji_result),
      as.data.frame(csp_doji_result)
    )
  }

  if (!"Date" %in% colnames(csp_doji_result)) {
    stop("csp_doji_result must contain a 'Date' column")
  }

  required_cols <- c("Doji", "DragonflyDoji", "GravestoneDoji")
  if (!all(required_cols %in% colnames(csp_doji_result))) {
    stop("csp_doji_result must contain 'Doji', 'DragonflyDoji', and 'GravestoneDoji' columns")
  }

  # Convert Date to proper format if needed
  csp_doji_result$Date <- as.Date(csp_doji_result$Date)

  # Merge with the original data to get the Close prices
  merged_data <- merge(eCandleSticks_result$data, csp_doji_result, by = "Date", all.x = TRUE)

  # Extract different types of Doji points
  doji_points <- merged_data[merged_data$Doji == TRUE & !is.na(merged_data$Doji), ]
  dragonfly_doji_points <- merged_data[merged_data$DragonflyDoji == TRUE & !is.na(merged_data$DragonflyDoji), ]
  gravestone_doji_points <- merged_data[merged_data$GravestoneDoji == TRUE & !is.na(merged_data$GravestoneDoji), ]

  # Add Doji points to the price plot
  price_plot_with_doji <- eCandleSticks_result$price_plot

  # Add regular Doji points (if any)
  if (nrow(doji_points) > 0) {
    price_plot_with_doji <- price_plot_with_doji +
      ggplot2::geom_point(
        data = doji_points,
        aes(x = Date, y = Close, color = "Doji"),
        size = point_size,
        shape = doji_shape,
        alpha = point_alpha
      )
  }

  # Add Dragonfly Doji points (if any)
  if (nrow(dragonfly_doji_points) > 0) {
    price_plot_with_doji <- price_plot_with_doji +
      ggplot2::geom_point(
        data = dragonfly_doji_points,
        aes(x = Date, y = Close, color = "Dragonfly Doji"),
        size = point_size,
        shape = dragonfly_doji_shape,
        alpha = point_alpha
      )
  }

  # Add Gravestone Doji points (if any)
  if (nrow(gravestone_doji_points) > 0) {
    price_plot_with_doji <- price_plot_with_doji +
      ggplot2::geom_point(
        data = gravestone_doji_points,
        aes(x = Date, y = Close, color = "Gravestone Doji"),
        size = point_size,
        shape = gravestone_doji_shape,
        alpha = point_alpha
      )
  }

  # Add color scale and legend only if there are any Doji points
  if (nrow(doji_points) > 0 || nrow(dragonfly_doji_points) > 0 || nrow(gravestone_doji_points) > 0) {
    price_plot_with_doji <- price_plot_with_doji +
      ggplot2::scale_color_manual(
        name = "CSP Doji Patterns",
        values = c(
          "Doji" = doji_color,
          "Dragonfly Doji" = dragonfly_doji_color,
          "Gravestone Doji" = gravestone_doji_color
        ),
        breaks = c("Doji", "Dragonfly Doji", "Gravestone Doji")
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(doji_shape, dragonfly_doji_shape, gravestone_doji_shape),
            size = rep(point_size, 3),
            alpha = rep(point_alpha, 3)
          )
        )
      )
  }

  # Update the result with the modified price plot
  eCandleSticks_result$price_plot <- price_plot_with_doji

  # Recombine with volume plot if it exists
  if (!is.null(eCandleSticks_result$volume_plot)) {
    eCandleSticks_result$combined_plot <- cowplot::plot_grid(
      price_plot_with_doji, eCandleSticks_result$volume_plot,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    eCandleSticks_result$combined_plot <- price_plot_with_doji
  }

  # Add csp_doji_result to the output for reference
  eCandleSticks_result$csp_doji_data <- csp_doji_result
  eCandleSticks_result$doji_points <- doji_points
  eCandleSticks_result$dragonfly_doji_points <- dragonfly_doji_points
  eCandleSticks_result$gravestone_doji_points <- gravestone_doji_points

  return(eCandleSticks_result)
}
