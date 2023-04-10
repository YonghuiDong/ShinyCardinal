#' plotSSCCSpec
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

plotSSCCSpec <- function(getSSCC, r, s, k){
  #(1) Format input ------------------------------------------------------------
  DF <- as.data.frame(Cardinal::subset(Cardinal::topFeatures(
    object = ssc1,
    model = list(r = r, s = s, k = k),
    n = Inf))
    )

  #(2) Plot --------------------------------------------------------------------
  classFactor <- levels(as.factor(DF$class))
  N <- length(classFactor)
  plot_list <- vector("list", length = N)
  for(i in 1:N){
    df <- DF[DF$class == classFactor[i], ]
    p <- plotly::plot_ly(data = df) %>%
      plotly::add_segments(x = ~ mz,
                           xend = ~ mz,
                           y = 0,
                           yend = ~ statistic,
                           name = paste0("Segment", i)
                           ) %>%
      plotly::layout(xaxis = list(title = 'm/z'),
                     yaxis = list(title = 'Statistic')
                     )
    plot_list[[i]] = p
  }
  plotly::subplot(plot_list,
                  nrows = ceiling(N/2),
                  shareX = TRUE,
                  shareY = TRUE,
                  titleX = TRUE,
                  titleY = TRUE) %>%
    plotly::layout(title = paste("r = ", r, ", s = ", s, ", k = ", k, sep = " ")) %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "ssccStatisticSpec"))
}


