#' Draw Shooting Star Pattern on Candlestick Chart
#'
#' Adds Shooting Star pattern markers to a candlestick chart created by
#' \code{eCandleSticks()}, using the results from \code{addCSPShootingStar()}.
#'
#' @param eCandleSticks_result Result object returned by \code{eCandleSticks()}.
#' @param csp_result Result from \code{addCSPShootingStar()} — xts or data frame
#'   with a \code{Date} column and a \code{ShootingStar} logical column.
#' @param color Point colour. Default \code{"darkorange"}.
#' @param point_size Point size. Default \code{3}.
#' @param shape Point shape. Default \code{25} (filled down-triangle).
#' @param point_alpha Alpha transparency. Default \code{0.8}.
#' @param mark_at_high Whether to mark at the High price instead of Close. Default \code{TRUE}.
#'
#' @return Modified \code{eCandleSticks_result} with pattern markers added.
#' @family pattern-1bar
#' @family pattern-bear
#' @export
#' @importFrom ggplot2 geom_point scale_color_manual guides guide_legend aes
#' @importFrom cowplot plot_grid
#'
#' @examples
#' \dontrun{
#' library(quantmod)
#' getSymbols("AAPL", src = "yahoo", from = "2023-01-01", to = "2025-01-01")
#' result <- eCandleSticks(AAPL)
#' pat    <- addCSPShootingStar(AAPL)
#' result <- drawCSPShootingStar(result, pat)
#' print(result$combined_plot)
#' }
drawCSPShootingStar <- function(eCandleSticks_result, csp_result,
                                 color = "darkorange", point_size = 3,
                                 shape = 25, point_alpha = 0.8,
                                 mark_at_high = TRUE) {
  if (!is.data.frame(csp_result) && !xts::is.xts(csp_result))
    stop("csp_result must be a data frame or xts object.")
  if (xts::is.xts(csp_result))
    csp_result <- data.frame(Date = zoo::index(csp_result),
                              as.data.frame(csp_result), row.names = NULL)
  if (!"Date" %in% colnames(csp_result))
    stop("csp_result must contain a 'Date' column.")
  if (!"ShootingStar" %in% colnames(csp_result))
    stop("csp_result must contain a 'ShootingStar' column.")

  csp_result$Date <- as.Date(csp_result$Date)
  merged <- merge(eCandleSticks_result$data, csp_result, by = "Date", all.x = TRUE)
  pts    <- merged[!is.na(merged$ShootingStar) & merged$ShootingStar, ]
  pts$MarkLevel <- if (mark_at_high) pts$High else pts$Close

  pp <- eCandleSticks_result$price_plot
  if (nrow(pts) > 0) {
    pp <- pp +
      ggplot2::geom_point(data = pts, ggplot2::aes(x = Date, y = MarkLevel, color = "Shooting Star"),
                          size = point_size, shape = shape, alpha = point_alpha) +
      ggplot2::scale_color_manual(name = "CSP Patterns", values = c("Shooting Star" = color)) +
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
