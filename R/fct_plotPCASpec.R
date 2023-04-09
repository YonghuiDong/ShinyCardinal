#' plotPCASpec
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

plotPCASpec <- function(msiData, pcaResult){

  #(1) Prepare the data --------------------------------------------------------
  pcaloadings <- data.frame(Cardinal::mz(msiData),
                            as.data.frame(Cardinal::resultData(pcaResult, 1, "loadings"))
                            )
  colnames(pcaloadings)[1] <- "mz"

  #(2) Plot --------------------------------------------------------------------
  PCs <- colnames(pcaloadings)[-1]
  N <- length(PCs)
  plot_list <- vector("list", length = N)

  for(i in 1:N){
    df <- data.frame(mz = pcaloadings$mz, Loadings = pcaloadings[[PCs[(i)]]])
    p <- plotly::plot_ly(data = df) %>%
      plotly::add_segments(x = ~ mz,
                           xend = ~ mz,
                           y = 0,
                           yend = ~ Loadings,
                           name = paste0("PC", i)
                           ) %>%
      plotly::layout(xaxis = list(title = 'm/z'),
                     yaxis = list(title = 'Loadings')
                     )
    plot_list[[i]] = p
  }

  plotly::subplot(plot_list,
                  nrows = ceiling(N/2),
                  shareX = TRUE,
                  shareY = TRUE,
                  titleX = TRUE,
                  titleY = TRUE) %>%
    plotly::config(
      toImageButtonOptions = list(format = "svg", filename = "pcaLoadingSpec")
    )
}




