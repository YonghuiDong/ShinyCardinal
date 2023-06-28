#' @title Crop MSI Data
#' @description Crop MSI Data based on defined ROIs
#' @param msiData MSI data set.
#' @param ROIs selected ROIs, list type.
#' @param cropType to crop inside or outside ROIs, list type.
#' Accepted value inside or outside. The length should be the same as ROIs.
#' @return The return value, if any, from executing the function.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 2, baseline = 1)
#' roiDF <- data.frame(x = c(1, 1, 5, 5), y = c(1, 5, 1, 5))
#' ROI0 <- getROI(msiData = mse, selectedRun = "run0", roiDF)
#' ROI1 <- getROI(msiData = mse, selectedRun = "run1", roiDF)
#' ROIs <- list(ROI0, ROI1)
#' cropData(msiData = mse, ROIs = ROIs, cropType = list("inside", "outside"))

cropData <- function(msiData, ROIs = NULL, cropType = NULL){
  subData <- NULL
  for (i in seq_along(ROIs)){
    if(cropType[[i]] == "inside"){
      croppedData <- msiData[ROIs[[i]]]
    } else{
      croppedData <- msiData[!ROIs[[i]]]
    }
    subData <- Cardinal::combine(subData, croppedData)
  }
  return(subData)
}

