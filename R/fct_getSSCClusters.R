#' @title Get SSC clusters.
#' @description Get SSC clusters based on defined r, k, s.
#' @param sscResult sscResult obtained from getSSC function.
#' @param r r.
#' @param k k.
#' @param s s.
#' @return a integer number.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' x <- simulateImage(preset = 1, nruns = 2, npeaks = 10)
#' sscResult <- getSSCC(x, r = 1, k = 2, s = 0, msiRun = "run0")
#' getSSCClusters(sscResult, r = 1, k = 2, s = 0)

getSSCClusters <- function(sscResult, r, k, s){
  sscSummary <- Cardinal::summary(sscResult)
  colnames(sscSummary)[1:3] <- c("r", "k", "s")
  sscSummary[c(sscSummary$r == r & sscSummary$k == k & sscSummary$s == s),]$Classes
}
