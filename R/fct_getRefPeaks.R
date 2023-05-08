#' @title Calculate reference peaks.
#' @description Get reference peaks from mean spectrum object.
#' @param meanSpec An object of class 'MSImagingSummary', obtained from getMeanSpec/summarizeFeatures function.
#' @param method Peak picking method.
#' @param SNR The minimum signal-to-noise ratio to be considered a peak in peak picking method.
#' @param tolerance The tolerance to be used when aligning detected peaks to the reference.
#' @param freq.min Minimum frequency; peaks that occur in the dataset in lesser proportion than this will be dropped.
#' @param workers Number of workers used for parallel computation.
#' @return The return value, if any, from executing the function.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 1, baseline = 1)
#' meanSpec <- getMeanSpec(msiData = mse)
#' refPeaks <- getRefPeaks(meanSpec = meanSpec, method = "mad", SNR = 3, tolerance = 10)

getRefPeaks <- function(meanSpec, method = "mad", SNR = 10, tolerance = 10, freq.min = 0.01, workers = 1){
  meanSpec |>
    Cardinal::peakPick(method = method, SNR = SNR) |>
    Cardinal::peakAlign(ref = "mean", tolerance = tolerance, units = "ppm") |>
    Cardinal::peakFilter(freq.min = freq.min) |>
    Cardinal::process(BPPARAM = BiocParallel::SnowParam(workers = workers, progressbar = FALSE))
}
