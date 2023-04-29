#' @title Plot PCA loading.
#' @description Plot PCA loading.
#' @param pcaResult PCA result obtained from getPCA()
#' @param msiRun Which MSI run to display?
#' @return plotly plot.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 2, baseline = 1)
#' pcaResult <- getPCA(msiData = mse, msiRun = "run0")
#' plotPCASpec(pcaResult = pcaResult, msiRun = "run0")

plotPCASpec <- function(pcaResult, msiRun){
  #(1) Prepare the data --------------------------------------------------------
  pcaLoadingList <- vector(mode = "list", length = 2)
  pcaLoadings <- data.frame(mz = round(as.data.frame(pcaResult@featureData), 4),
                            as.data.frame(Cardinal::resultData(pcaResult, 1, "loadings"))
                            )

  #(2) Plot --------------------------------------------------------------------
  PCs <- colnames(pcaLoadings)[-1]
  N <- length(PCs)
  plot_list <- vector("list", length = N)

  for(i in 1:N){
    df <- data.frame(mz = pcaLoadings$mz, Loadings = pcaLoadings[[PCs[(i)]]])
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

  pacLoadingPlot <- plotly::subplot(plot_list,
                                    nrows = ceiling(N/2),
                                    shareX = TRUE,
                                    shareY = TRUE,
                                    titleX = TRUE,
                                    titleY = TRUE) %>%
    plotly::layout(title = paste("PCA loading plot (pseudospectrum) for", msiRun, sep = " ")) %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "pcaLoadingSpec"))
  pcaLoadingList$df <- pcaLoadings
  pcaLoadingList$plot <- pacLoadingPlot
  return(pcaLoadingList)
}




