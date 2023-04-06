#' @title getMeanSpec
#' @description A fct function
#' @return The return value, if any, from executing the function.
#' @noRd

getMeanSpec <- function(msiData, worker = 1){
  Cardinal::summarizeFeatures(x = msiData,
                              FUN = "mean",
                              BPPARAM = BiocParallel::SnowParam(workers = worker, progressbar = T)
                              )
}
