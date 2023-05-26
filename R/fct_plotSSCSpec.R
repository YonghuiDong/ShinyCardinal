#' @title Plot SSCC Spectra
#' @description Plot SSCC Spectra
#' @param getSSCC the result from getSSC function.
#' @param r The spatial neighborhood radius of nearby pixels to consider.
#' @param s The sparsity thresholding parameter by which to shrink the t-statistics.
#' @param k The maximum number of segments.
#' @param clusters select clusters to visualize.
#' @param msiRun MSI runs.
#' @return a list, including a t-statistics table and plot.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' x <- simulateImage(preset = 1, nruns = 2, npeaks = 10)
#' res <- getSSC(x, r = 1, k = c(5, 7), s = c(0, 3), msiRun = "run0")
#' plotSSCSpec(getSSC = res, r = 1, k = 5, clusters = c(1, 3), s = 0, msiRun = "run0")

plotSSCSpec <- function(getSSC, r, k, s, clusters, msiRun){
  #(1) Format input ------------------------------------------------------------
  tStatistics <- vector(mode = "list", length = 2)
  DF <- as.data.frame(
    Cardinal::topFeatures(
      object = getSSC,
      model = list(r = r, s = s, k = k),
      n = Inf
    )
  ) |>
    transform(mz = round(mz, 4))

  #(2) Plot --------------------------------------------------------------------
  #classFactor <- levels(as.factor(DF$class))
  cols <- Cardinal::discrete.colors(length(clusters))
  plot_list <- vector(mode = "list", length = length(clusters))
  for(i in seq_along(clusters)){
    df <- DF[DF$class == clusters[i], ]
    p <- plotly::plot_ly(data = df) %>%
      plotly::add_segments(x = ~ mz,
                           xend = ~ mz,
                           y = 0,
                           yend = ~ statistic,
                           name = paste0("Segment", clusters[i]),
                           line = list(color = cols[i])
                           ) %>%
      plotly::layout(xaxis = list(title = 'm/z'),
                     yaxis = list(title = 't-statistic')
                     )
    plot_list[[i]] = p
  }
  plot <- plotly::subplot(plot_list,
                          nrows = ceiling(length(clusters)/2),
                          shareX = TRUE,
                          shareY = TRUE,
                          titleX = TRUE,
                          titleY = TRUE) %>%
    plotly::layout(title = paste0(msiRun,": ", "r = ", r, ", s = ", s, ", k = ", k)) %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "sscStatisticSpec"))
  tStatistics$specTable <- DF
  tStatistics$specPlot <- plot
  return(tStatistics)
}


