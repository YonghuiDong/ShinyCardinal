#' @title processMSIData
#' @description A fct function
#' @param msiData uploaded msi data.
#' @param red reference peaks
#' @return The return value, if any, from executing the function.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2)
#' mse <- simulateImage(preset=1, npeaks=10, dim=c(3,3), baseline=3)
#' ref <- summarizeFeatures(mse, FUN="mean")
#' processMSIData(msiData = mse, ref = ref, ifSmoothing = TRUE, ifBLReduction = TRUE)

processMSIData <- function(msiData, normMethod = "tic", ref, ifSmoothing = FALSE, smoothMethod = "gaussian",
                           ifBLReduction = FALSE, blReductionMethod = "locmin", tolerance = 15, workers = 1){
  if(!ifSmoothing & !ifBLReduction){
    msiData |>
      Cardinal::normalize(method = normMethod) |>
      Cardinal::peakBin(ref = Cardinal::mz(ref), tolerance = tolerance, units = "ppm") |>
      Cardinal::process(BPPARAM = BiocParallel::SnowParam(workers = workers, progressbar = FALSE))
  } else if(ifSmoothing & !ifBLReduction){
    msiData |>
      Cardinal::normalize(method = normMethod) |>
      Cardinal::smoothSignal(method = smoothMethod) |>
      Cardinal::peakBin(ref = Cardinal::mz(ref), tolerance = tolerance, units = "ppm") |>
      Cardinal::process(BPPARAM = BiocParallel::SnowParam(workers = workers, progressbar = FALSE))
  } else if(!ifSmoothing & ifBLReduction){
    msiData |>
      Cardinal::normalize(method = normMethod) |>
      Cardinal::reduceBaseline(method = blReductionMethod) |>
      Cardinal::peakBin(ref = Cardinal::mz(ref), tolerance = tolerance, units = "ppm") |>
      Cardinal::process(BPPARAM = BiocParallel::SnowParam(workers = workers, progressbar = FALSE))
  } else{
    msiData |>
      Cardinal::normalize(method = normMethod) |>
      Cardinal::smoothSignal(method = smoothMethod) |>
      Cardinal::reduceBaseline(method = blReductionMethod) |>
      Cardinal::peakBin(ref = Cardinal::mz(ref), tolerance = tolerance, units = "ppm") |>
      Cardinal::process(BPPARAM = BiocParallel::SnowParam(workers = workers, progressbar = FALSE))
  }

}
