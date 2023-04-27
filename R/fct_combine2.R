#' @title Combine different ROIs subset
#' @description Get and combine the ROIs subset, see Cardinal::combine.
#' @param msiData msi data
#' @param roiList a list of user defined ROIs. For statistics, the names of ROIs should contain two parts:
#' before underscore is ROI identifier and after underscore is the condition for statistics.
#' such as ROI1_WT, ROI2_WT, ROI3_Mu, ROI4_Mu, 4 ROIs and 2 conditions, WT and Mu. If the name is not valid, statistic will not be performed.
#' @return combined MSI Data.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(1)
#' x <- simulateImage(preset=1, nruns=2, npeaks=10, dim=c(2,2))
#' a <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
#' b <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE, TRUE)
#' c <- c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
#' l <- list(a, b, c)
#' names(l) <- c("a_X", "b_Y", "c_Y")
#' Result1 <- combine2(msiData = x, roiList = l)
#' names(l) <- c("a_X", "b_Y", "c_Y_g") # not valid name for statistics
#' Result2 <- combine2(msiData = x, roiList = l)


combine2 <- function(msiData, roiList){
  #(1) subset msiData based on a list of selected ROIs -------------------------
  m <- lapply(roiList, function(i) msiData[i])
  #(2) rename runs using ROI names ---------------------------------------------
  lapply(seq_along(m), function(i) {
    Cardinal::run(m[[i]]) <<- names(m)[i]
  })
  #(3) check if the roiList names are valid for statistics----------------------
  ## if Valid, a condition will be added in pData for statistics
  pattern <- "^[^_]+_[^_]+$"
  namesChek <- all(grepl(pattern, names(roiList)))
  if(isTRUE(namesChek)){
    lapply(seq_along(m), function(i) {
      m[[i]]$condition <<- as.factor(strsplit(names(m)[i], "_")[[1]][2])
    })
  }

  Cardinal::combine(m)
}
