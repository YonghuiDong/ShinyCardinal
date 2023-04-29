#' @title Principal components analysis
#' @description Principal components analysis. See Cardinal::PCA.
#' @param msiData MSI data set.
#' @param ncomp The number of principal components to calculate.
#' @param center Should the data be centered first? This is passed to scale.
#' @param scale Shoud the data be scaled first? This is passed to scale.
#' @param msiRun Which MSI run to display?
#' @param workers number of workers for parallel computation.
#' @return An object of class PCA2. See Cardinal::PCA.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 2, baseline = 1)
#' getPCA(msiData = mse, msiRun = "run0")

getPCA <- function(msiData, ncomp = 3, center = TRUE, scale = FALSE, msiRun = "All", workers = 1){
  if(msiRun == "All"){
    Cardinal::PCA(msiData,
                  ncomp = ncomp,
                  center = center,
                  scale = scale,
                  BPPARAM = BiocParallel::SnowParam(workers = workers, progressbar = FALSE)
                  )
  } else {
    msiData <- msiData[Cardinal::run(msiData) == msiRun]
    Cardinal::PCA(msiData,
                  ncomp = ncomp,
                  center = center,
                  scale = scale,
                  BPPARAM = BiocParallel::SnowParam(workers = workers, progressbar = FALSE)
                  )
  }
}
