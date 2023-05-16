#' @title Deisotoping
#' @description Deisotoping of MSI data.
#' @param msiData MSI data set.
#' @param ncomp number of PCA components.
#' @param tol mass accuracy tolerance, unit in Da.
#' @param PCC Pearson correlation coefficient threshould (pseudo colocalization).
#' @return deisotoped MSI data set.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 2, baseline = 1)
#' deisotoping(msiData = mse)

deisotoping <- function(msiData, tol = 0.005, PCC = 0.8) {
  idx_iso = c()
  lb <- 1.003355 - tol
  ub <- 1.003355 + tol
  ims_mass <- Cardinal::mz(msiData)
  for (i in 1:length(ims_mass)) {
    m <- ims_mass[i]
    idx <- c(which((ims_mass >= (m + lb)) & (ims_mass <= (m + ub))))
    if(length(idx) > 0){
      idx <- idx[sapply(idx, function(j) cor(t(Cardinal::iData(msiData[i, ])), t(Cardinal::iData(msiData[j, ])))) >= PCC]
    }
    idx_iso <- c(idx_iso, idx)
  }
  if(length(idx_iso) > 0){
    msiData <- msiData[-idx_iso, ]
  }
  return(msiData)
}
