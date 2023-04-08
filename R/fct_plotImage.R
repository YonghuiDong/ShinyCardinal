#' plotImage
#'
#' @description A fct function
#' @return The return value, if any, from executing the function.
#'
#' @noRd


plotImage <- function(msiData, mz, smooth.image = "none", plusminus = 0.03,
                      colorscale = "cividis", normalize.image = "linear",
                      contrast.enhance = "suppression", superpose = FALSE){

  Cardinal::image(msiData,
                  mz = mz,
                  smooth.image = smooth.image,
                  plusminus = plusminus,
                  colorscale = Cardinal::col.map(colorscale),
                  contrast.enhance = contrast.enhance,
                  normalize.image = normalize.image,
                  superpose = superpose
                  )
}
