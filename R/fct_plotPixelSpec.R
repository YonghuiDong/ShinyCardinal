#' @title Plot spectra of selected pixels
#' @description Plot spectra of selected pixels
#' @param msiData MSI data set.
#' @param pixelDF Pixel dataframe describing x-y position of each pixel.
#' @return The return value, if any, from executing the function.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 1, baseline = 1, representation = "centroid")
#' pixelDF <- data.frame(x = c(1, 2, 3), y = c(1, 100, 10))
#' plotPixelSpec(msiData = mse, pixelDF = pixelDF)

plotPixelSpec <- function(msiData, pixelDF){
  #(1) Prepare df --------------------------------------------------------------
  df <- data.frame(matrix("", nrow = length(Cardinal::mz(msiData)), ncol = nrow(pixelDF)+1))
  for(i in 1:nrow(pixelDF)){
    pid <- Cardinal::pixels(msiData, coord = list(x = pixelDF[i, 1], y = pixelDF[i, 2]))
    if(identical(pid, integer(0))){
      df[, (i+1)] <- NA
    } else{
      selected <- msiData[, pid]
      df[, (i+1)] <- Cardinal::iData(selected)[, 1]
      colnames(df)[(i+1)] <- paste0("X", pixelDF[i, 1], "Y", pixelDF[i, 2])
    }
  }
  df[, 1] <- Cardinal::mz(msiData)
  colnames(df)[1] <- "mz"
  df <- df[, colMeans(is.na(df)) != 1, drop = FALSE]

  if(ncol(df) > 1){
    #(2) Set the last pixel negative when there are over 1 pixels --------------
    if(ncol(df) > 2){
      df[, ncol(df)] <- -df[, ncol(df)]
    }

    #(3) Plot spectrum ---------------------------------------------------------
    pixelNames <- names(df)[-1]
    p <- plotly::plot_ly(data = df)
    for(k in 1:length(pixelNames)) {
      dfk <- data.frame(mz = df$mz, Intensity = df[[pixelNames[k]]])
      p <- plotly::add_segments(p,
                                data = dfk,
                                x = ~ mz,
                                xend = ~ mz,
                                y = 0,
                                yend = ~ Intensity,
                                name = pixelNames[k]
                                )
    }
    p <- p %>%
      plotly::layout(xaxis = list(title = 'm/z', range = c(min(df$mz) - 5, max(df$mz) + 5))) %>%
      plotly::config(toImageButtonOptions = list(format = "svg", filename = "pxielSpec"))
    return(p)
  }
}

