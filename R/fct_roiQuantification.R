#' roiQuantification
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(1)
#' x <- simulateImage(preset=1, nruns=2, npeaks=10, dim=c(2,2))
#' a <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
#' b <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE, TRUE)
#' c <- c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
#' l <- list(a, b, c)
#' names(l) <- c("a_0.1", "b_1", "c_10")
#' Result1 <- combine2(msiData = x, roiList = l)
#' roiQuantification(roiMSIData = Result1, mz = 870.9207)


roiQuantification <- function(roiMSIData, mz){
  #(1) Filter by mz ------------------------------------------------------------
  fid <- Cardinal::features(roiMSIData, mz = mz)
  subROIMSIData <- roiMSIData[fid, ]
  DF <- data.frame(Group = Cardinal::run(subROIMSIData), Intensity = as.vector(Cardinal::iData(subROIMSIData)))
  #(2) Get mean intensity ------------------------------------------------------
  roiMean <- aggregate(Intensity ~ Group, data = DF, FUN = mean)
  #(3) Format data -------------------------------------------------------------
  if(any(roiMean$Intensity >= 1)){
    roiMean$Intensity <- round(roiMean$Intensity, 2)
  } else{
    roiMean$Intensity <- round(roiMean$Intensity, 4)
  }
  roiMean <- data.frame(roiMean, Concentration = as.numeric(NA))
  return(roiMean)
}


