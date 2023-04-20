#' @title Remove background noise and/or matrix peaks.
#' @description Remove background noise and/or matrix peaks.
#' @param msiData MSI dataset, an object of class 'MSContinuousImagingExperiment'.
#' @param subDF A dataframe obtained from colocAnalysis.
#' @return The return value, if any, from executing the function.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 1, baseline = 1)
#' subDF <- colocAnalysis(msiData = mse, precursor = 773.4709, nth = 1)
#' subDF <- subDF[subDF$correlation >= 0.8, ]
#' removeNoise(msiData = mse, subDF = subDF)

removeNoise <- function(msiData, subDF){
  msiData[-which(msiData@featureData@mz %in% subDF$mz), ]
}

