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

deisotoping <- function(msiData, ncomp = 30, tol = 0.005, PCC = 0.9) {
  pcaResult <- Cardinal::PCA(x = msiData, ncomp = min(ncomp, length(Cardinal::mz(msiData))))
  pcaLoadings <- t(data.frame(as.data.frame(Cardinal::resultData(pcaResult, 1, "loadings"))))
  idx_iso = c()
  lb <- 1.003355 - tol
  ub <- 1.003355 + tol
  ims_mass <- Cardinal::mz(msiData)
  for (i in 1:length(ims_mass)) {
    m <- ims_mass[i]
    idx <- c(which((ims_mass >= (m + lb)) & (ims_mass <= (m + ub))))
    if(length(idx) > 0){
      sub <- pcaLoadings[, idx, drop = FALSE]
      idx <- idx[apply(sub, 2, function(x) cor(pcaLoadings[, i], x)) >= PCC]
    }
    idx_iso <- c(idx_iso, idx)
  }
  if(length(idx_iso) > 0){
    msiData <- msiData[-idx_iso, ]
  }
  return(msiData)
}
