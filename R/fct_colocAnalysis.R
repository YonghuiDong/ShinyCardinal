#' @title Perform colocalization analysis
#' @description A fct function
#' @return The return value, if any, from executing the function.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 1, baseline = 1)
#' colocAnalysis(msiData = mse, precursor = 773.4709, nth = 1)

colocAnalysis <- function(msiData, precursor, nth = 1,  worker = 1){
  #(1) Get colocalized features ------------------------------------------------
  msiData[, seq(1, max(Cardinal::pixels(msiData)), by = nth)] |>
    Cardinal::colocalized(object = _,
                          mz = precursor,
                          n = 100,
                          BPPARAM = BiocParallel::SnowParam(workers = worker, progressbar = T)
                          )
}
