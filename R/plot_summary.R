#' Plot summaries as bar chart
#'
#' @param vec A named vector with summary statistics.
#'
#' @returns A \code{ggplot2} object.
#'
#' @export
#'
plot_summary <- function(vec) {
  plot_df <- data.frame(x = names(vec), y = vec)

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "Variable", y = "Summary") +
    ggplot2::theme_bw()

  return(p)
}
