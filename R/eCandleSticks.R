#' Enhanced Candlestick Chart Plotting Function (based on ggplot2)
#'
#' Plots highly customizable candlestick charts for stock data downloaded by quantmod (containing OHLCVA columns),
#' with support for custom colors, date ranges, volume subplots, and technical indicators (e.g., MA moving averages).
#'
#' @param data Stock data object obtained by quantmod::getSymbols(), must be in xts/zoo format with OHLC columns
#' (Open, High, Low, Close), and can include Volume and Adjusted columns (OHLCVA).
#' @param start_date Start date (character/Date), e.g., "2023-01-01". Default NULL (uses earliest data date).
#' @param end_date End date (character/Date), e.g., "2023-12-31". Default NULL (uses latest data date).
#' @param color_up Color for rising candles (close >= open). Default "#FF4444" (red).
#' @param color_down Color for falling candles (close < open). Default "#44AA99" (green).
#' @param color_shadow Color for shadows (high-low). Default "#666666" (dark gray). Supports transparency (e.g., "#66666680").
#' @param show_volume Whether to plot volume subplot. Default TRUE; automatically disabled if no Volume column.
#' @param color_volume_up Volume color for rising days. Default "#FF444480" (semi-transparent red).
#' @param color_volume_down Volume color for falling days. Default "#44AA9980" (semi-transparent green).
#' @param show_ma Whether to plot moving averages (MA). Default FALSE.
#' @param ma_periods Vector of MA periods. Default c(20, 50) (20-day and 50-day MA).
#' @param color_ma Vector of MA colors, must match length of ma_periods. Default c("#FF8800", "#9900FF").
#' @param title Main chart title. Default auto-generated (e.g., "AAPL Stock Price (2023-01-01 to 2023-12-31)").
#' @param subtitle Chart subtitle. Default NULL.
#' @param theme_type Chart theme. Options: "minimal", "classic", "dark" (dark mode). Default "minimal".
#' @param axis_text_angle Rotation angle for x-axis date labels. Default 45 (degrees); 0 for horizontal.
#' @param return_components Whether to return the individual components as a list. Default TRUE.
#'
#' @return A list containing:
#'   - price_plot: The candlestick chart (ggplot object)
#'   - volume_plot: The volume subplot (ggplot object, or NULL if show_volume=FALSE)
#'   - data: The processed data used for plotting
#'   - combined_plot: The combined plot (if show_volume=TRUE, a cowplot combined object)
#' @export
#' @importFrom quantmod Op Hi Lo Cl Vo Ad is.OHLC
#' @importFrom grid unit
#' @importFrom ggplot2 ggplot aes geom_segment geom_rect geom_line
#' @importFrom grid unit
#' @importFrom ggplot2 scale_fill_manual scale_color_manual labs theme_bw
#' @importFrom grid unit
#' @importFrom ggplot2 theme element_text element_rect guide_legend guides element_blank
#' @importFrom dplyr mutate filter
#' @importFrom lubridate as_date
#' @importFrom cowplot plot_grid
#' @importFrom zoo rollmean
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(ggplot2)
#' library(quantmod)
#' getSymbols("AAPL", src = "yahoo", from = "2023-01-01", to = "2025-09-08") # Contains OHLCVA columns
#'
#' # Basic usage (returns list with components)
#' result <- eCandleSticks(AAPL)
#' print(result$combined_plot)
#'
#' # Access individual components
#' print(result$price_plot)
#' print(result$volume_plot)
#' head(result$data)
#' }
eCandleSticks <- function(
    data,
    start_date = NULL,
    end_date = NULL,
    color_up = "#FF4444",
    color_down = "#44AA99",
    color_shadow = "#666666",
    show_volume = TRUE,
    color_volume_up = "#FF444480",
    color_volume_down = "#44AA9980",
    show_ma = FALSE,
    ma_periods = c(20, 50),
    color_ma = c("#FF8800", "#9900FF"),
    title = NULL,
    subtitle = NULL,
    theme_type = c("minimal", "classic", "dark"),
    axis_text_angle = 45,
    return_components = TRUE) {
  # 声明全局变量以避免检查警告
  Date <- Open <- High <- Low <- Close <- Volume <- Direction <- NULL

  # Input validation: Support OHLCVA (ensure at least OHLC)
  if (!quantmod::is.OHLC(data)) {
    stop("Data must be an OHLC object with Open/High/Low/Close columns")
  }

  # Validate MA parameter length matching
  if (show_ma && length(ma_periods) != length(color_ma)) {
    stop("ma_periods and color_ma must have the same length")
  }
  theme_type <- match.arg(theme_type)

  # Date processing
  data_dates <- lubridate::as_date(zoo::index(data))
  start_date <- if (is.null(start_date)) min(data_dates) else as_date(start_date)
  end_date <- if (is.null(end_date)) max(data_dates) else as_date(end_date)
  if (start_date > end_date) stop("Start date cannot be later than end date")

  # Data preprocessing: Support OHLCVA columns (extract required fields)
  df <- data.frame(
    Date = data_dates,
    Open = as.numeric(quantmod::Op(data)),
    High = as.numeric(quantmod::Hi(data)),
    Low = as.numeric(quantmod::Lo(data)),
    Close = as.numeric(quantmod::Cl(data)),
    Volume = as.numeric(quantmod::Vo(data)),
    Adjusted = as.numeric(quantmod::Ad(data))
  )

  # Filter by date range
  df <- df[df$Date >= start_date & df$Date <= end_date, ]

  # Add Direction column
  df$Direction <- ifelse(df$Close >= df$Open, "Up", "Down")

  # Automatically disable volume if no volume data
  if (show_volume && all(is.na(df$Volume))) {
    message("No Volume column in data, automatically disabling volume subplot")
    show_volume <- FALSE
  }

  # Calculate moving averages (only when show_ma=TRUE)
  if (show_ma) {
    for (i in seq_along(ma_periods)) {
      period <- ma_periods[i]
      col_name <- paste0("MA", period)
      df[[col_name]] <- zoo::rollmean(df$Close, k = period, align = "right", fill = NA)
    }
  }

  # Build price candlestick chart
  p_price <- ggplot2::ggplot(df, ggplot2::aes(x = Date)) +
    # Draw shadows
    ggplot2::geom_segment(
      ggplot2::aes(xend = Date, y = Low, yend = High),
      color = color_shadow, linewidth = 0.4
    ) +
    # Draw candle bodies
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = Date - 0.4, xmax = Date + 0.4,
        ymin = pmin(Open, Close), ymax = pmax(Open, Close),
        fill = Direction
      ),
      color = "black", linewidth = 0.1
    ) +
    # Add moving averages (if enabled)
    {
      if (show_ma) {
        ma_lines <- list()
        for (i in seq_along(ma_periods)) {
          col_name <- paste0("MA", ma_periods[i])
          ma_name <- paste0("MA", ma_periods[i])
          ma_lines[[i]] <- ggplot2::geom_line(
            ggplot2::aes_string(y = col_name, color = shQuote(ma_name)),
            linewidth = 0.8, alpha = 0.8
          )
        }
        ma_lines
      }
    } +
    # Color settings
    ggplot2::scale_fill_manual(values = c(Up = color_up, Down = color_down), name = "Trend") +
    {
      if (show_ma) {
        ma_names <- paste0("MA", ma_periods)
        ma_colors <- stats::setNames(color_ma, ma_names)
        ggplot2::scale_color_manual(values = ma_colors, name = "Moving Average")
      }
    } +
    # Title and labels
    ggplot2::labs(
      title = if (is.null(title)) {
        paste0(colnames(data)[1], " Stock Price (", start_date, " to ", end_date, ")")
      } else {
        title
      },
      subtitle = subtitle,
      x = NULL, # Remove x-axis title for price chart
      y = "Price (USD)"
    ) +
    # Apply theme_bw as base theme
    ggplot2::theme_bw() +
    # Additional theme customization
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
      axis.text.x = ggplot2::element_blank(), # Remove x-axis text for price chart
      axis.ticks.x = ggplot2::element_blank(), # Remove x-axis ticks for price chart
      legend.position = "inside",
      legend.position.inside = c(0.02, 0.98), # Top-left position
      legend.justification = c(0, 1), # Align legend to top-left
      legend.direction = "horizontal", # Horizontal legend layout
      legend.box = "horizontal", # Horizontal legend box
      legend.background = ggplot2::element_rect(fill = "white", color = NA), # Legend background
      legend.margin = ggplot2::margin(t = 2, r = 5, b = 2, l = 5), # Adjust legend margins
      legend.key.size = unit(0.3, "cm")
    )

  # Initialize volume plot as NULL
  p_volume <- NULL
  p_combined <- NULL

  # Build volume subplot if required
  if (show_volume) {
    p_volume <- ggplot2::ggplot(df, ggplot2::aes(x = Date)) +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = Date - 0.4, xmax = Date + 0.4,
          ymin = 0, ymax = Volume,
          fill = Direction
        ),
        alpha = 1
      ) +
      ggplot2::scale_fill_manual(values = c(Up = color_volume_up, Down = color_volume_down)) +
      ggplot2::labs(x = "Date", y = "Volume") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = axis_text_angle, hjust = 1, vjust = 1),
        legend.position = "none" # Hide legend in volume subplot
      )

    # Combine price and volume plots using cowplot
    p_combined <- cowplot::plot_grid(
      p_price, p_volume,
      ncol = 1, align = "v", axis = "lr",
      rel_heights = c(2, 1)
    )
  } else {
    p_combined <- p_price
  }

  # Return components as a list
  result <- list(
    price_plot = p_price,
    volume_plot = p_volume,
    data = df,
    combined_plot = p_combined
  )

  return(result)
}
