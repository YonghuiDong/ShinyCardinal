#' plotImage
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


plotImage <- function(msiData, mz, smooth.image = "none", plusminus = 0.03,
                      colorscale = "cividis", normalize.image = "linear",
                      contrast.enhance = "suppression"){

  ## the input mz is text, so I need to convert into numeric values
  mz <- gsub(" ", "", mz)
  split <- strsplit(mz, ",", fixed = FALSE)
  mz <- as.numeric(split)

  image <- Cardinal::image(msiData,
                           mz = mz,
                           smooth.image = smooth.image,
                           plusminus = plusminus,
                           colorscale = Cardinal::col.map(colorscale),
                           contrast.enhance = contrast.enhance,
                           normalize.image = normalize.image
                           )
  return(image)
}
