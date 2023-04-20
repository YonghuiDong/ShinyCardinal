#' @title Get regions of interest
#' @description Calculate the regions of interest based on users selection on the MSI image.
#' @param msiData MSI dataset, an object of class 'MSContinuousImagingExperiment'.
#' @param roiDF a region of interest dataframe. The data frame contains two columns, x and y, which records the x and y coordinates of user defined ROI.
#' @return a vector of logical values, indicating which pixels fall in ROI.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 1, baseline = 1)
#' roiDF <- data.frame(x = c(1, 1, 5, 5), y = c(1, 5, 1, 5))
#' getROI(msiData = mse, roiDF)

getROI <- function(msiData, roiDF){
  coord <- Cardinal::coord(msiData)
  sp::point.in.polygon(coord[,1], coord[,2], roiDF$x, roiDF$y) > 0
}
