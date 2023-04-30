#' @title Plot SSCC Spectra
#' @description Plot SSCC Spectra
#' @param getSSCC the result from getSSCC function.
#' @param msiRun MSI runs.
#' @return a list, including a t-statistics table and plot.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' x <- simulateImage(preset = 1, nruns = 2, npeaks = 10)
#' res <- getSSCC(x, r = 1, k = 2, s = 0, msiRun = "run0")
#' plotSSCCSpec(getSSCC = res, msiRun = "run0")

plotSSCCSpec <- function(getSSCC, msiRun){
  #(1) Format input ------------------------------------------------------------
  tStatistics <- vector(mode = "list", length = 2)
  DF <- as.data.frame(Cardinal::topFeatures(object = getSSCC, n = Inf)) |>
    transform(mz = round(mz, 4))

  #(2) Plot --------------------------------------------------------------------
  classFactor <- levels(as.factor(DF$class))
  N <- length(classFactor)
  plot_list <- vector("list", length = N)
  for(i in 1:N){
    df <- DF[DF$class == classFactor[i], ]
    p <- plotly::plot_ly(data = df) %>%
      plotly::add_segments(x = ~ mz,
                           xend = ~ mz,
                           y = 0,
                           yend = ~ statistic,
                           name = paste0("Segment", i),
                           line = list(color = Cardinal::discrete.colors(N)[i])
                           ) %>%
      plotly::layout(xaxis = list(title = 'm/z'),
                     yaxis = list(title = 't-statistic')
                     )
    plot_list[[i]] = p
  }
  plot <- plotly::subplot(plot_list,
                          nrows = ceiling(N/2),
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


