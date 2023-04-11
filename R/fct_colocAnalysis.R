#' @title Perform colocalization analysis
#' @description A fct function
#' @return The return value, if any, from executing the function.
#' @noRd

colocAnalysis <- function(msiData, precursor, nth = nth,  worker = 1){
  #(1) Get colocalized features ------------------------------------------------
  msiData[, seq(1, max(Cardinal::pixels(msiData)), by = nth)] |>
    Cardinal::colocalized(object = _,
                          mz = precursor,
                          n = 100,
                          BPPARAM = BiocParallel::SnowParam(workers = worker, progressbar = T)
                          )
}
