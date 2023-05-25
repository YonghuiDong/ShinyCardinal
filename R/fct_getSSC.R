#' @title Performs spatially-aware nearest shrunken centroid clustering
#' @description Performs spatially-aware nearest shrunken centroid clustering. See Cardinal::spatialShrunkenCentroids.
#' @param msiData MSI data set.
#' @param r The spatial neighborhood radius of nearby pixels to consider. This can be a vector of multiple radii values.
#' @param s The sparsity thresholding parameter by which to shrink the t-statistics.
#' @param k The maximum number of segments (clusters). This can be a vector to try initializing the clustering with different
#'  numbers of maximum segments. The final number of segments may differ.
#' @param method The method to use to calculate the spatial smoothing weights. The 'gaussian' method refers to spatially-aware (SA) weights,
#'  and 'adaptive' refers to spatially-aware structurally-adaptive (SASA) weights.
#' @param dist The type of distance metric to use when calculating neighboring pixels based on r.
#'  The options are ‘radial’, ‘manhattan’, ‘minkowski’, and ‘chebyshev’ (the default).
#' @param msiRUN MSI run.
#' @return The return value, if any, from executing the function.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' x <- simulateImage(preset = 1, nruns = 2, npeaks = 10)
#' res <- getSSC(x, r = 1, k = 2, s = 0, msiRun = "run0")

getSSC <- function(msiData, r = 1, s = 0, k = 2, method = "adaptive",
                    dist = "chebyshev", msiRun = "All"){
  if(msiRun == "All"){
    Cardinal::spatialShrunkenCentroids(
      x = msiData,
      r = r,
      s = s,
      k = k,
      method = method,
      dist = dist
      )
  } else {
    msiData <- msiData[Cardinal::run(msiData) == msiRun]
    Cardinal::spatialShrunkenCentroids(
      x = msiData,
      r = r,
      s = s,
      k = k,
      method = method,
      dist = dist
    )
  }
}
