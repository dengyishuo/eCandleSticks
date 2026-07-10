#' Draw N Blended Candle Pattern on Candlestick Chart
#'
#' Adds markers for N-blended candle patterns to a candlestick chart created by
#' \code{eCandleSticks()}, using results from \code{addCSPNBlended()}.
#'
#' @param eCandleSticks_result Result object returned by \code{eCandleSticks()}.
#' @param csp_result Result from \code{addCSPNBlended()} — xts or data frame.
#'   Column names follow the pattern \code{paste0(N, c(".Blended.Open", ".Blended.High",
#'   ".Blended.Low", ".Blended.Close"))}.
#' @param N Integer. Must match the \code{N} used in \code{addCSPNBlended()}.
#' @param color Point colour. Default \code{"purple"}.
#' @param point_size Point size. Default \code{3}.
#' @param shape Point shape. Default \code{23} (filled diamond).
#' @param point_alpha Alpha transparency. Default \code{0.8}.
#'
#' @return Modified \code{eCandleSticks_result} with pattern markers added.
#' @family pattern-nbar
#' @export
#' @importFrom stats setNames
#' @importFrom ggplot2 geom_point scale_color_manual guides guide_legend aes
#' @importFrom cowplot plot_grid
#'
#' @examples
#' \dontrun{
#' library(quantmod)
#' getSymbols("AAPL", src = "yahoo", from = "2023-01-01", to = "2025-01-01")
#' result <- eCandleSticks(AAPL)
#' pat    <- addCSPNBlended(AAPL, N = 3)
#' result <- drawCSPNBlended(result, pat, N = 3)
#' print(result$combined_plot)
#' }
drawCSPNBlended <- function(eCandleSticks_result, csp_result, N,
                             color = "purple", point_size = 3,
                             shape = 23, point_alpha = 0.8) {
  if (!is.data.frame(csp_result) && !xts::is.xts(csp_result))
    stop("csp_result must be a data frame or xts object.")
  if (xts::is.xts(csp_result))
    csp_result <- data.frame(Date = zoo::index(csp_result),
                              as.data.frame(csp_result), row.names = NULL)
  if (!"Date" %in% colnames(csp_result))
    stop("csp_result must contain a 'Date' column.")

  close_col <- paste0(N, ".Blended.Close")
  if (!close_col %in% colnames(csp_result))
    stop("csp_result must contain column '", close_col, "'. Check that N matches addCSPNBlended().")

  csp_result$Date <- as.Date(csp_result$Date)
  merged <- merge(eCandleSticks_result$data, csp_result, by = "Date", all.x = TRUE)
  # Mark rows where a blended close exists (non-NA)
  pts   <- merged[!is.na(merged[[close_col]]), ]
  label <- paste0(N, "-Blended Candle")

  pp <- eCandleSticks_result$price_plot
  if (nrow(pts) > 0) {
    pp <- pp +
      ggplot2::geom_point(data = pts,
                          ggplot2::aes(x = Date, y = .data[[close_col]], color = label),
                          size = point_size, shape = shape, alpha = point_alpha) +
      ggplot2::scale_color_manual(name = "CSP Patterns", values = setNames(color, label)) +
      ggplot2::guides(color = ggplot2::guide_legend(
        override.aes = list(shape = shape, size = point_size, alpha = point_alpha)))
  }

  eCandleSticks_result$price_plot <- pp
  eCandleSticks_result$combined_plot <- if (!is.null(eCandleSticks_result$volume_plot))
    cowplot::plot_grid(pp, eCandleSticks_result$volume_plot,
                       ncol = 1, align = "v", axis = "lr", rel_heights = c(2, 1))
  else pp

  eCandleSticks_result$pattern_data   <- csp_result
  eCandleSticks_result$pattern_points <- pts
  eCandleSticks_result
}
