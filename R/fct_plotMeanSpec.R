#' @title plotMeanSpec
#' @description A fct function
#' @param meanSpec An object of class 'MSImagingSummary' obtained from summarizeFeatures function.
#' @param nth Every nth pixel
#' @importFrom plotly %>%
#' @return The return value, if any, from executing the function.
#' @noRd
#'
plotMeanSpec <- function(meanSpec, nth = 1){
  data <- data.frame(Cardinal::featureData(meanSpec))
  plotly::plot_ly(data = data) %>%
    plotly::add_segments(x = ~ mz,
                         xend = ~ mz,
                         y = 0,
                         yend = ~ mean,
                         line = list(color = "#03946d")) %>%
    plotly::layout(xaxis = list(title = 'm/z'),
                   yaxis = list(title = 'Intensity'),
                   title = paste("mean spectrum for MSI data subsetted by every", nth, "pixel(s)", sep = " ")
                   )
}
