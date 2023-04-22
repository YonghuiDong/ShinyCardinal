#' @title Get regions of interest
#' @description Calculate the regions of interest based on users selection on the MSI image.
#' @param msiData MSI dataset, an object of class 'MSContinuousImagingExperiment'.
#' @param roiDF a region of interest dataframe. The data frame contains two columns, x and y, which records the x and y coordinates of user defined ROI.
#' @param selectedRun the selected msi run.
#' @return a vector of logical values, indicating which pixels fall in ROI.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 2, baseline = 1)
#' roiDF <- data.frame(x = c(1, 1, 5, 5), y = c(1, 5, 1, 5))
#' getROI(msiData = mse, selectedRun = "run1", roiDF)

getROI <- function(msiData, selectedRun, roiDF){
  pdata <- Cardinal::pData(msiData)
  pdata <- data.frame(rows = 1:nrow(pdata), pdata)
  roi <- rep(FALSE, nrow(pdata))
  coord <- subset(pdata, run == selectedRun)
  selected <- sp::point.in.polygon(coord[, "x"], coord[, "y"], roiDF$x, roiDF$y) > 0
  roi[coord$rows] <- selected
  roi
}
