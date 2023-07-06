#' @title Deisotoping
#' @description Deisotoping of MSI data.
#' @param msiData MSI data set.
#' @param tol mass accuracy tolerance, unit in Da.
#' @param PCC Pearson correlation coefficient threshould (pseudo colocalization).
#' @return deisotoped MSI data set.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 2, baseline = 1)
#' deisotoping(msiData = mse)

deisotoping <- function(msiData, tol = 0.006, PCC = 0.85) {
  idx_iso = c()
  lb <- 1.003355 - tol
  ub <- 1.003355 + tol
  mzs <- Cardinal::mz(msiData)
  for (i in 1:length(mzs)) {
    m <- mzs [i]
    idx <- c(which((mzs >= (m + lb)) & (mzs <= (m + ub))))
    if(length(idx) > 0){
      idx <- idx[sapply(idx, function(j) stats::cor(t(Cardinal::iData(msiData[i, ])), t(Cardinal::iData(msiData[j, ])))) >= PCC]
    }
    idx_iso <- c(idx_iso, idx)
  }
  if(length(idx_iso) > 0){
    idx_iso <- unique(idx_iso[!is.na(idx_iso)])
    msiData <- msiData[-idx_iso, ]
  }
  return(msiData)
}
