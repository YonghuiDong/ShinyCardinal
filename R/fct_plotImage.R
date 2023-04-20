#' @title Plot MSI images.
#' @description Plot MSI images. See image() in Cardinal package.
#' @param msiData MSI dataset, an object of class 'MSContinuousImagingExperiment'.
#' @param mz m/z value, can be a vector.
#' @param smooth.image smoothing method.
#' @param plusminus mass window.
#' @param colorscale colorscale.
#' @param normalize.image Normalization function to be applied to each image.
#' @param contrast.enhance Contrast enhancement function to be applied to each image.
#' @param superpose Should different feature images superposed on the same plot?
#' @param run Which MSI run to display?
#' @return MSI image
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 1, baseline = 1)
#' plotImage(msiData = mse, mz = 426.529)


plotImage <- function(msiData, mz, smooth.image = "none", plusminus = 0.003,
                      colorscale = "cividis", normalize.image = "linear",
                      contrast.enhance = "suppression", superpose = FALSE, run = "All"){

  if(run == "All"){
    Cardinal::image(msiData,
                    mz = mz,
                    smooth.image = smooth.image,
                    plusminus = plusminus,
                    colorscale = Cardinal::col.map(colorscale),
                    contrast.enhance = contrast.enhance,
                    normalize.image = normalize.image,
                    superpose = superpose
                    )
  } else {
    Cardinal::image(msiData,
                    mz = mz,
                    smooth.image = smooth.image,
                    plusminus = plusminus,
                    colorscale = Cardinal::col.map(colorscale),
                    contrast.enhance = contrast.enhance,
                    normalize.image = normalize.image,
                    superpose = superpose,
                    subset = Cardinal::run(msiData) == run
                    )

    }
}
