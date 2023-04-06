#' @title processMSIData
#' @description A fct function
#' @param msiData uploaded msi data.
#' @param red reference peaks
#' @return The return value, if any, from executing the function.
#' @noRd

processMSIData <- function(msiData, method = "tic", ref, tolerance = 15, workers = 6){
  msiData |>
    Cardinal::normalize(method = method) |>
    Cardinal::peakBin(ref = Cardinal::mz(ref), tolerance = tolerance, units = "ppm") |>
    Cardinal::process(BPPARAM = BiocParallel::SnowParam(workers = workers, progressbar = T))
}
