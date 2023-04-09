#' @title Plot spectra of selected pixels
#' @description A fct function
#' @return The return value, if any, from executing the function.
#' @noRd

plotPixelSpec <- function(msiData, pixelDF){
  #(1) Prepare df --------------------------------------------------------------
  df <- data.frame(matrix("", nrow = length(msiData@featureData@mz), ncol = nrow(pixelDF)))
  for(i in 1:nrow(pixelDF)){
    pid <- Cardinal::pixels(msiData, coord = list(x = pixelDF[i, 1], y = pixelDF[i, 2]))
    selected <- msiData[, pid]
    df[, (i+1)] <- Cardinal::iData(selected)[,1]
    colnames(df)[(i+1)] <- paste0("X", pixelDF[i, 1], "Y", pixelDF[i, 2])
  }
  df[, 1] <- msiData@featureData@mz
  colnames(df)[1] <- "mz"

  #(2) Plot spectrum -----------------------------------------------------------
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
  return(p)
  }

