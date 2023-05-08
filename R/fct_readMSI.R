#' @title readMSI
#' @description read multiple imzML file and combine them.
#' @importFrom Cardinal readMSIData
#' @importFrom BiocParallel SnowParam
#' @return The return value, if any, from executing the function.
#' @noRd

readMSI <- function(path, massResolution = 10, massRange = NULL, dataCentroid = TRUE, workers = 1){
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

  #(2.1) Single MSI data -------------------------------------------------------
  if(length(msiData) == 1){
    return(msiData[[1]])
  } else {
    #(2.2) Multiple MSI data ---------------------------------------------------
    modes <- unlist(lapply(msiData, Cardinal::centroided))
    ##(2.2.1) make centroid TRUE if different in different files
    if(length(unique(modes)) != 1 | all(is.na(modes))){
      for(i in 1:length(msiData)){
        Cardinal::centroided(msiData[[i]]) <- dataCentroid
      }
    }
    ##(2.2.2) Combine multiple files
    msiCombined <- Cardinal::combine(msiData[1:length(msiData)])
    Cardinal::coord(msiCombined)["z"] <- NULL
    return(msiCombined)
  }

}
