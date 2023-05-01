#' @title Plot network for all features.
#' @description lot network for all features.
#' @param PCC pearson correlation coefficient obtained from getPCC function.
#' @param threshold PCC threshold.
#' @param labelSize label size.
#' @param interactive generate interactive or static plot.
#' @return network Plot.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 1, baseline = 1)
#' PCC <- getPCC(mse)
#' showAllNetwork(PCC, labelSize = 60)


showAllNetwork <- function(PCC, threshold = 0.9, labelSize = 40, interactive = TRUE){
  #(1) Prepare igraph network object -------------------------------------------
  PCC[PCC < threshold] = 0
  network <- igraph::graph_from_adjacency_matrix(PCC, weighted = TRUE, mode = "lower", diag = FALSE)
  isolated <- which(igraph::degree(network) == 0)
  networkObject <- igraph::delete.vertices(network, isolated)
  #(2) Plot network ------------------------------------------------------------
  nodes <- igraph::as_data_frame(networkObject, what = "vertices")
  colnames(nodes)[1] <- "id"
  edges <- igraph::as_data_frame(networkObject, what = "edges")
  nodes$label <- igraph::V(networkObject)$name
  edges$value <- igraph::E(networkObject)$weight
  visNetwork::visNetwork(nodes = nodes, edges = edges, width = "100%") %>%
    visNetwork::visNodes(font = list(size = labelSize)) %>%
    visNetwork::visOptions(highlightNearest = TRUE) %>%
    visNetwork::visIgraphLayout()
}
