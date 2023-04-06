#' @title getRefPeaks
#' @description Get reference peaks from mean spectrum
#' @param meanSpec An object of class 'MSImagingSummary' obtained from summarizeFeatures function.
#' @return The return value, if any, from executing the function.
#' @noRd

getRefPeaks <- function(meanSpec, method = "mad", SNR = 10, tolerance = 10, freq.min = 0.01, workers = 1){
  meanSpec |>
    Cardinal::peakPick(method = method, SNR = SNR) |>
    Cardinal::peakAlign(ref = "mean", tolerance = tolerance, units = "ppm") |>
    Cardinal::peakFilter(freq.min = freq.min) |>
    Cardinal::process(BPPARAM = BiocParallel::SnowParam(workers = workers, progressbar = T))
}
