#' @title Plot network for a single feature.
#' @description Plot network for a single feature.
#' @param PCC pearson correlation coefficient obtained from getPCC function.
#' @param threshold PCC threshold.
#' @param labelSize label size.
#' @return network Plot.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 1, baseline = 1)
#' PCC <- getPCC(mse, mz = 493.3798)
#' plotSingleNetwork(PCC, mz = 493.3797)


plotSingleNetwork <- function(PCC, mz, threshold = 0.9, labelSize = 40){
  #(1) Prepare igraph network object -------------------------------------------
  PCC <- subset(PCC, correlation >= threshold)

  ## In many times, the user input mz is not identical to the real mz in the data.
  ## I need to use the one from the real data, otherwise, the edges will disappear.
  PCC <- PCC[!duplicated(PCC$mz), ] # sometimes there are duplicated rows.
  mzFrom <- PCC[round(PCC$correlation, 7) == 1, ]$mz ##see #18
  if(length(mzFrom) > 1){
    mzFrom <- mzFrom[which.min(abs(mzFrom - mz))]
  }
  edges <- cbind.data.frame(from = rep(mzFrom, dim(PCC)[1]),
                            to = PCC$mz,
                            weight = PCC$correlation
                            )
  nodes <- data.frame(id = PCC$mz)
  nodes$label <- as.character(PCC$mz)
  edges$value <- PCC$correlation

  #(2) Plot network ------------------------------------------------------------
  visNetwork::visNetwork(nodes = nodes, edges[edges$from != edges$to, ], width = "100%") %>%
    visNetwork::visNodes(font = list(size = labelSize)) %>%
    visNetwork::visOptions(highlightNearest = TRUE) %>%
    visNetwork::visInteraction(navigationButtons = TRUE) %>%
    visNetwork::visIgraphLayout()
}
