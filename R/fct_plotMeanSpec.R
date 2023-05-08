#' @title Plot mean spectrum
#' @description Plot mean specrum
#' @param meanSpec An object of class 'MSImagingSummary' obtained from getMeanSpec function.
#' @param nth Every nth pixel
#' @importFrom plotly %>%
#' @return The return value, if any, from executing the function.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 1, baseline = 1)
#' meanSpec <- getMeanSpec(msiData = mse)
#' plotMeanSpec(meanSpec, nth = 2)

plotMeanSpec <- function(meanSpec, nth = 1){
  data <- data.frame(mz = meanSpec@featureData@mz, mean = Cardinal::iData(meanSpec)[,1])
  ## define title
  if(nth == 1){
    title = ""
  } else {
    title = paste("mean spectrum for MSI data subsetted by every", nth, "pixels", sep = " ")
  }
  if(!isTRUE(Cardinal::centroided(meanSpec))){
    p <- plotly::plot_ly(data, x = ~ mz, y = ~ mean, type = 'scatter', mode = 'lines')
  } else {
    p <- plotly::plot_ly(data = data) %>%
      plotly::add_segments(x = ~ mz, xend = ~ mz, y = 0, yend = ~ mean)
  }
  p %>%
    plotly::layout(xaxis = list(title = 'm/z'),
                   yaxis = list(title = 'Intensity'),
                   title = title
                   )
}

