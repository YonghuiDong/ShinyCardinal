#' @title Get the number of CPU cores
#' @description Get the number of CPU Cores and use it (n-1) for parallel computation
#' @return a numeric value
#' @noRd
#' @example
#' maxCores()

maxCores <- function(){
  return(ifelse(parallel::detectCores() == 1, 1, parallel::detectCores() -1))
}
