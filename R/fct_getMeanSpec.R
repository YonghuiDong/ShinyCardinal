#' @title Calculate mean spectrum
#' @description Calculate mean spectrum of MSI data.
#' @param msiData MSI dataset, an object of class 'MSContinuousImagingExperiment'.
#' @param workers Number of workers used for parallel computation.
#' @return an object of class 'MSContinuousImagingExperiment' containing the mean features and 1 pixel.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 1, baseline = 1)
#' getMeanSpec(msiData = mse, nth = 5)

getMeanSpec <- function(msiData, nth = 1, workers = 1){
  #(1) subset by pixel ---------------------------------------------------------
  nth <- ifelse(nth > max(Cardinal::pixels(msiData)), 1, nth)
  if(nth > 1){
    msiData <- msiData[, seq(1, max(Cardinal::pixels(msiData)), by = nth)]
  }
  #(2) calculate mean ----------------------------------------------------------
  Cardinal::summarizeFeatures(x = msiData,
                              FUN = "mean",
                              BPPARAM = BiocParallel::SnowParam(workers = workers, progressbar = T)
                              )
}
