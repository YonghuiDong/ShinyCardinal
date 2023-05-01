#' @title Calculate Pearson correlation coefficient.
#' @description Calculate Pearson correlation coefficient.
#' @param msiData MSI data set.
#' @param nth subset MSI data by selecting every nth pixel.
#' @param msiRun MSI runs.
#' @return an igraph object ready for network visualization
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 1, baseline = 1)
#' getPCC(mse)

getPCC <- function(msiData, nth = 1, msiRun = "All"){
  if(msiRun != "All"){
    msiData <- msiData[Cardinal::run(msiData) == msiRun]
  }
  nth <- ifelse(nth > max(Cardinal::pixels(msiData)), 1, nth) # make sure nth is smaller than max pixels.
  msiData <- msiData[, seq(1, max(Cardinal::pixels(msiData)), by = nth)]
  specData <- Cardinal::spectra(msiData)
  rownames(specData) <- round(Cardinal::mz(msiData), 4)
  cor(t(specData))
}
