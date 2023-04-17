#' @title subsetMSIData
#' @description A fct function
#' @param msiData An object of class 'MSContinuousImagingExperiment
#' @param roiDF pixelDF from user defined region of interest. It contains two columns, x is the x locaition, y is the y location.
#' @return The return value, if any, from executing the function.
#'
#' @noRd

subsetMSIData <- function(msiData, mzValues = NULL, roiDF = NULL) {
  #(1) Check Input -------------------------------------------------------------

  #(2) Subset by m/z Values ----------------------------------------------------
  if(is.null(mzValues)){
    msiDataFsub <- msiData
  } else {
    fid <- Cardinal::features(msiData, mz = mzVlues)
    msiDataFsub <- msiData[fid, ]
  }
  #(2) Subset by pixels --------------------------------------------------------
  if(nrow(roiDF) == 0){
    msiDataPsub <- msiDataFsub
  } else {
    coord <- Cardinal::coord(msiData)
    selected <- sp::point.in.polygon(coord[,1], coord[,2], roiDF$x, roiDF$y) > 0
    msiDataPsub <- msiDataFsub[, selected]
  }
  return(msiDataPsub)
}



