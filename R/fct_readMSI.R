#' @title readMSI
#' @description read multiple imzML file and combine them into one
#' @importFrom Cardinal readMSIData
#' @importFrom BiocParallel SnowParam
#' @return The return value, if any, from executing the function.
#' @noRd

readMSI <- function(path, massResolution = 10, massRange = NULL, workers = 6){
  #(1) Read and combine files in case multiple files are loaded ----------------
  msiData <- lapply(path, function(x)
    Cardinal::readMSIData(x,
                          resolution = massResolution,
                          units = "ppm",
                          mass.range = massRange,
                          attach.only = TRUE,
                          BPPARAM = BiocParallel::SnowParam(workers = workers, progressbar = FALSE)
                          )
  )

  #(2) make centroid information consistent among different files --------------
  for(i in 1:length(msiData)){
    Cardinal::centroided(msiData[[i]]) <- FALSE
  }

  #(3) Combine multiple files --------------------------------------------------
  msiCombined <- Cardinal::combine(msiData[1:length(msiData)])
  Cardinal::coord(msiCombined)["z"] <- NULL
  return(msiCombined)
}
