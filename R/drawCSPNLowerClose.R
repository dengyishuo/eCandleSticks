#' Draw N Consecutive Lower Close Pattern on Candlestick Chart
#'
#' Adds markers for N consecutive lower close prices to a candlestick chart
#' created by \code{eCandleSticks()}, using results from \code{addCSPNLowerClose()}.
#'
#' @param eCandleSticks_result Result object returned by \code{eCandleSticks()}.
#' @param csp_result Result from \code{addCSPNLowerClose()} — xts or data frame.
#'   The pattern column name is \code{paste0(N, "LowerClose")} (e.g. \code{"3LowerClose"}).
#' @param N Integer. Must match the \code{N} used in \code{addCSPNLowerClose()}.
#' @param color Point colour. Default \code{"firebrick"}.
#' @param point_size Point size. Default \code{3}.
#' @param shape Point shape. Default \code{25} (filled down-triangle).
#' @param point_alpha Alpha transparency. Default \code{0.8}.
#'
#' @return Modified \code{eCandleSticks_result} with pattern markers added.
#' @family pattern-nbar
#' @family pattern-bear
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
#' pat    <- addCSPNLowerClose(AAPL, N = 3)
#' result <- drawCSPNLowerClose(result, pat, N = 3)
#' print(result$combined_plot)
#' }
drawCSPNLowerClose <- function(eCandleSticks_result, csp_result, N,
                                color = "firebrick", point_size = 3,
                                shape = 25, point_alpha = 0.8) {
  if (!is.data.frame(csp_result) && !xts::is.xts(csp_result))
    stop("csp_result must be a data frame or xts object.")
  if (xts::is.xts(csp_result))
    csp_result <- data.frame(Date = zoo::index(csp_result),
                              as.data.frame(csp_result), row.names = NULL)
  if (!"Date" %in% colnames(csp_result))
    stop("csp_result must contain a 'Date' column.")

  pat_col <- paste0(N, "LowerClose")
  if (!pat_col %in% colnames(csp_result))
    stop("csp_result must contain column '", pat_col, "'. Check that N matches addCSPNLowerClose().")

  csp_result$Date <- as.Date(csp_result$Date)
  merged <- merge(eCandleSticks_result$data, csp_result, by = "Date", all.x = TRUE)
  pts    <- merged[!is.na(merged[[pat_col]]) & merged[[pat_col]], ]
  label  <- paste0(N, " Lower Close")

  pp <- eCandleSticks_result$price_plot
  if (nrow(pts) > 0) {
    pp <- pp +
      ggplot2::geom_point(data = pts, ggplot2::aes(x = Date, y = Close, color = label),
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
