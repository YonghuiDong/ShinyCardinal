#' @title Calculate mean spectrum
#' @description Calculate mean spectrum of MSI data.
#' @param msiData MSI dataset, an object of class 'MSContinuousImagingExperiment'.
#' @param worker number of workers used for parallel computation.
#' @return an object of class 'MSContinuousImagingExperiment' containing the mean features and 1 pixel.
#' @noRd
#' @examples
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 1, baseline = 1)
#' getMeanSpec(msiData = mse)


getMeanSpec <- function(msiData, worker = 1){
  Cardinal::summarizeFeatures(x = msiData,
                              FUN = "mean",
                              BPPARAM = BiocParallel::SnowParam(workers = worker, progressbar = T)
                              )
}
