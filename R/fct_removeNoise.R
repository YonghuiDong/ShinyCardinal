#' @title removeNoise
#' @description A fct function
#' @return The return value, if any, from executing the function.
#' @noRd

removeNoise <- function(msiData, subDF){
  msiData[-which(msiData@featureData@mz %in% subDF$mz), ]
}

