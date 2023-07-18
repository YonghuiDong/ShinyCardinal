#' @title Export MSI data as data frame
#' @description Export MSI data as data frame.
#' @param msiData MSI data set.
#' @return a data frame.
#' @noRd
#' @examples
#' library(Cardinal)
#' mse <- simulateImage(preset=2, nruns = 3, npeaks=10, dim=c(10,10))
#' df <- exportData(msiData = mse)

exportData <- function(msiData){
  n <- length(levels(Cardinal::run(msiData)))
  DF <- data.frame(mz = Cardinal::mz(msiData), Cardinal::spectra(msiData))
  if(n == 1){
    colnames(DF) <- c("mz", paste0("Pixel", Cardinal::pixels(msiData)))
  } else{
    levels(Cardinal::run(msiData)) <- paste0("Sample", 1:n) # rename runs
    colnames(DF) <- c("mz", paste0(Cardinal::run(msiData), "_", "Pixel", Cardinal::pixels(msiData)))
  }
  return(DF)
}
