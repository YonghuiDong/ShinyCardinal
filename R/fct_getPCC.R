#' @title Calculate Pearson correlation coefficient.
#' @description Calculate Pearson correlation coefficient either for all mass features or a single mass feature.
#' @param msiData MSI data set.
#' @param nth subset MSI data by selecting every nth pixel.
#' @param msiRun MSI runs.
#' @return an igraph object ready for network visualization
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 1, baseline = 1)
#' ## all mass features
#' getPCC(mse)
#' ## single mass feature
#' getPCC(mse, mz = 493.3798)

getPCC <- function(msiData, mz = NULL, nth = 1, msiRun = "All"){
  #(1) subset by run -----------------------------------------------------------
  if(msiRun != "All"){
    msiData <- msiData[Cardinal::run(msiData) == msiRun]
  }
  #(2) subset by pixel ---------------------------------------------------------
  nth <- ifelse(nth > max(Cardinal::pixels(msiData)), 1, nth)
  if(nth > 1){
    msiData <- msiData[, seq(1, max(Cardinal::pixels(msiData)), by = nth)]
  }
  #(3) calculate PCC -----------------------------------------------------------
  if(is.null(mz)){
    specData <- Cardinal::spectra(msiData)
    rownames(specData) <- round(Cardinal::mz(msiData), 4)
    stats::cor(t(specData))
  } else {
    Cardinal::colocalized(object = msiData, mz = mz, n = Inf) |>
      subset(x = _, select = c("mz", "correlation")) |>
      transform(mz = round(mz, 4)) |>
      as.data.frame(x = _)
  }
}
