#' @title Plot SSCC Spectra
#' @description Plot SSCC Spectra
#' @param getSSCC
#' @param r
#' @param s description
#' @param k description
#' @return The return value, if any, from executing the function.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' x <- simulateImage(preset = 1, nruns = 2, npeaks = 10)
#' res <- getSSCC(x, r = 1, k = 2, s = 0, msiRun = "run0")
#' plotSSCCSpec(getSSCC = res, r = 1, k = 2, s = 0, msiRun = "run0")

plotSSCCSpec <- function(getSSCC, r, s, k, msiRun){
  #(1) Format input ------------------------------------------------------------
  tStatistics <- vector(mode = "list", length = 2)
  DF <- as.data.frame(Cardinal::subset(Cardinal::topFeatures(
    object = getSSCC,
    model = list(r = r, s = s, k = k),
    n = Inf))
    )

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
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "ssccStatisticSpec"))
  tStatistics$specTable <- DF
  tStatistics$specPlot <- plot
  return(tStatistics)
}


