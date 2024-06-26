#' plotSSCImage
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' x <- simulateImage(preset = 1, nruns = 2, npeaks = 10)
#' sscResult <- getSSC(x, r = 1, k = 2, s = 0, msiRun = "run0")
#' plotSSCImage(sscResult, r = 1, k = 2, s = 0, cluster = c(1, 2), superpose = T)

plotSSCImage <- function(sscResult, r, k, s, clusters, superpose = TRUE){
  N <- length(clusters)
  cols <- Cardinal::discrete.colors(N)
  if(isTRUE(superpose)){
    p <- Cardinal::image(sscResult,
                         model = list(r = r, k = k, s = s),
                         column = clusters,
                         col = cols,
                         strip = TRUE,
                         key = TRUE
                         )
  } else{
    p <- vector(mode = "list", length = length(clusters))
    for (i in seq_along(clusters)){
      p[[i]] <- Cardinal::image(sscResult,
                                model = list(r = r, k = k, s = s),
                                column = clusters[i],
                                col = cols[i],
                                strip = TRUE,
                                key = TRUE,
                                layout = FALSE
                                )
    }
  }
  p

}
