#' @title Plot MSI images.
#' @description Plot MSI images. See image() in Cardinal package.
#' @param msiData MSI dataset, an object of class 'MSContinuousImagingExperiment'.
#' @param mz m/z value, can be a vector.
#' @param smooth.image smoothing method.
#' @param plusminus mass window. If is not NA, a window of m/z values will be included in the plot.
#' @param colorscale colorscale.
#' @param normalize.image Normalization function to be applied to each image.
#' @param contrast.enhance Contrast enhancement function to be applied to each image.
#' @param superpose Should different feature images superposed on the same plot?
#' @param msiRun Which MSI run to display?
#' @return MSI image
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 2, baseline = 1)
#' plotImage(msiData = mse, mz = 426.529, msiRun = "run0")
#' plotImage(msiData = mse, mz = 426.529, plusminus = 0.001, msiRun = "run0")


plotImage <- function(msiData, mz, smooth.image = "none", plusminus = NA,
                      colorscale = "cividis", normalize.image = "linear", zlim = c(0, 1),
                      contrast.enhance = "suppression", superpose = FALSE, msiRun = "All"){

  if(is.na(plusminus)){plusminus = NULL}

  if(msiRun == "All"){
    Cardinal::image(msiData,
                    mz = mz,
                    smooth.image = smooth.image,
                    plusminus = plusminus,
                    colorscale = Cardinal::col.map(colorscale),
                    contrast.enhance = contrast.enhance,
                    normalize.image = normalize.image,
                    zlim = zlim,
                    superpose = superpose
                    )
  } else{
    msiData <- msiData[Cardinal::run(msiData) == msiRun]
    Cardinal::image(msiData,
                    mz = mz,
                    smooth.image = smooth.image,
                    plusminus = plusminus,
                    colorscale = Cardinal::col.map(colorscale),
                    contrast.enhance = contrast.enhance,
                    normalize.image = normalize.image,
                    zlim = zlim,
                    superpose = superpose
                    )

  }
}
