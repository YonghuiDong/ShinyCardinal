#' @title Perform colocalization analysis
#' @description Perform colocalization analysis on MSI dataset.
#' @param msiData MSI dataset, an object of class 'MSContinuousImagingExperiment'.
#' @param precursor m/z value of the precursor ion used for colocalization analysis.
#' @param nth every nth pixel will be used for colocalization analysis. This could improve the speed.
#' @param workers number of workers used for parallel computation.
#' @return a dataframe
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 1, baseline = 1)
#' colocAnalysis(msiData = mse, precursor = 773.4709, nth = 1)

colocAnalysis <- function(msiData, precursor, nth = 1,  workers = 1){
  #(1) Get colocalized features ------------------------------------------------
  msiData[, seq(1, max(Cardinal::pixels(msiData)), by = nth)] |>
    Cardinal::colocalized(object = _,
                          mz = precursor,
                          n = 100,
                          BPPARAM = BiocParallel::SnowParam(workers = workers, progressbar = T)
                          )
}
