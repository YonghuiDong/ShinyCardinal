#' @title Plot ion intensity of selected m/z values along the ROI pixels.
#' @description Plot ion intensity of selected m/z values along the ROI pixels.
#' @param msiData description
#' @param roiMSIData a list of user defined ROIs. The length should be 1. There are no requirements for the name of ROI.
#' @param mz user defined m/z values.
#' @return plotly plot
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(1)
#' x <- simulateImage(preset=1, nruns=2, npeaks=10, dim=c(2,2))
#' a <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
#' l <- list(a)
#' names(l) <- "a"
#' plotROIProfile(msiData = x, roiMSIData = l, mz = c(1143.6165, 798.8322))


plotROIProfile <- function(msiData, roiMSIData, mz){
  #(1) Check input
  if(length(roiMSIData) != 1){stop("Only one ROI is allowed.")}
  mz <- mz[order(mz)] # 'mz' must be in increasing order here
  #(2) Prepare data ------------------------------------------------------------
  roi <- combine2(msiData = msiData, roiList = roiMSIData)
  fid <- Cardinal::features(roi, mz = mz)
  subROIMSIData <- roi[fid, ] # here needs 'mz' must be in increasing order
  roiPxiel <- as.data.frame(Cardinal::coord(subROIMSIData))
  df <- as.data.frame(t(Cardinal::spectra(subROIMSIData)))
  colnames(df) <- paste0("mz", mz)
  df$position1 <- 1:nrow(df)
  df$position2 <- paste0("X", roiPxiel$x, "Y", roiPxiel$y)
  #(3) Plot --------------------------------------------------------------------
  featureNames <- names(df)[!names(df) %in% c("position1", "position2")]
  p <- plotly::plot_ly(data = df)
  for(k in 1:length(featureNames)) {
    dfk <- data.frame(Intensity = df[[featureNames[k]]], position = df$position1, position2 = df$position2)
    p <- plotly::add_trace(p,
                           data = dfk,
                           x = ~ position,
                           y = ~ Intensity,
                           text = ~ position2,
                           type = "scatter",
                           mode = 'lines+markers',
                           name = featureNames[k]
                          ) %>%
      plotly::config(
        toImageButtonOptions = list(format = "svg", filename = "roiProfile")
      )
  }
  return(p)
}
