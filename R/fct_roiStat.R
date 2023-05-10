#' @title Perform statistics on different ROIs.
#' @description Perform statistics on different ROIs.
#' @param roiMSIData Combined ROI MSI data. The output from combine2 function.
#' @return A data frame, including mean ion intensities of the ROIs and/or
#' @noRd
#' @examples
#' library(Cardinal)
#' #(1) from roiList
#' set.seed(1)
#' x <- simulateImage(preset=1, nruns=2, npeaks=10, dim=c(2,2))
#' a <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
#' b <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE, TRUE)
#' c <- c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
#' l <- list(a, b, c)
#' names(l) <- c("a_X", "b_Y", "c_Y")
#' Result1 <- combine2(msiData = x, roiList = l)
#' roiStat(roiMSIData = Result1)
#' #(2) with statistics
#' set.seed(2)
#' x <- simulateImage(preset=4, nruns=3, npeaks=10, dim=c(10,10))
#' roiStat(roiMSIData = x)

roiStat <- function(roiMSIData){
  #(1) Get mean intensity for each ROI MSI data -------------------------------
  roiMean <- Cardinal::aggregate(x = roiMSIData, FUN= c('mean'), groups = Cardinal::run(roiMSIData), as = 'DataFrame') |>
    as.data.frame(x = _) |>
    transform(mz = round(mz, 4))
  #(2) Means test --------------------------------------------------------------
  numPerLevel <- sapply(levels(roiMSIData$condition), function(sLevel) sum(roiMSIData$condition == sLevel))
  if(!is.null(roiMSIData$condition) & length(levels(roiMSIData$condition)) >= 2 & all(numPerLevel >= 2)){
    fit <- Cardinal::meansTest(x = roiMSIData, ~ condition, groups = Cardinal::run(roiMSIData))
    roiStat <- Cardinal::summary(fit) |>
      as.data.frame(x = _) |>
      within(data = _, rm("Feature"))
  } else{
    roiStat <- NULL
  }
  #(3) Show result -------------------------------------------------------------
  if(!is.null(roiStat)){
    cbind(roiMean, roiStat) |>
      (\(x) x[rowSums(x[, -which(names(x) == "mz")], na.rm = TRUE) > 0, ])() ## rm rows with all means == 0
  } else{
    roiMean |>
      (\(x) x[rowSums(x[, -which(names(x) == "mz")], na.rm = TRUE) > 0, ])() ## rm rows with all means == 0
  }
}

