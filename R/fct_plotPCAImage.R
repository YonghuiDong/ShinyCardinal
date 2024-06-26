#' @title Plot PCA images
#' @description Plot PCA images.
#' @param pcaResult PCA result from getPCA function.
#' @param clusters Which clusers to plot, default All.
#' @param superpose Should images be superposed?
#' @return PCA images
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 2, baseline = 1)
#' pcaResult <- getPCA(msiData = mse)
#' plotPCAImage(pcaResult, clusters = c(1, 3), superpose = TRUE)

plotPCAImage <- function(pcaResult, clusters, superpose = TRUE){
  cols <- Cardinal::discrete.colors(length(clusters))
  if(isTRUE(superpose)){
    p <- Cardinal::image(pcaResult,
                         column = clusters,
                         col = cols,
                         strip = FALSE,
                         key = TRUE
                         )
  } else{
    p <- vector(mode = "list", length = length(clusters))
    for (i in seq_along(clusters)){
      p[[i]] <- Cardinal::image(pcaResult,
                                column = clusters[i],
                                col = cols[i],
                                strip = FALSE,
                                key = TRUE,
                                layout = FALSE
                                )
    }
  }
  p
}
