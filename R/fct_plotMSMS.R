#' @title plot pseudo MS/MS spectrum
#' @description plot pseudo MS/MS spectrum
#' @param msiData MSI data set.
#' @param PCC pearson correlation coefficient obtained from getPCC function.
#' @param threshold PCC threshold.
#' @return a ployly object.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 1, baseline = 1)
#' PCC <- getPCC(mse, mz = 493.3798)
#' plotMSMS(msiData = mse, PCC = PCC)

plotMSMS <- function(msiData, PCC, threshold = 0.9){
  PCC <- subset(PCC, correlation >= threshold)
  Int <- as.vector(rep(NA, dim(PCC)[1]))
  for (i in 1:length(PCC$mz)) {
    Int[i] <- sum(Cardinal::spectra(msiData)[Cardinal::features(msiData,  mz = PCC$mz[i]), ])
  }
  DF <- cbind.data.frame(mz = PCC$mz, Intensity = Int)
  plotly::plot_ly(data = DF) %>%
    plotly::add_segments(x = ~ mz,
                         xend = ~ mz,
                         y = 0,
                         yend = ~ Intensity) %>%
    plotly::layout(xaxis = list(title = 'm/z', range = c(min(DF$mz) - 3, max(DF$mz) + 3)),
                   yaxis = list(title = 'Intensity'),
                   title = "Pseudo MS/MS spectrum") %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "pseudoMSMS"))

}
