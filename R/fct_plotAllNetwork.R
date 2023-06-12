#' @title Plot network for all features.
#' @description lot network for all features.
#' @param PCC pearson correlation coefficient obtained from getPCC function.
#' @param threshold PCC threshold.
#' @param labelSize label size.
#' @return network Plot.
#' @noRd
#' @examples
#' library(Cardinal)
#' set.seed(2020)
#' mse <- simulateImage(preset = 1, npeaks = 10, nruns = 1, baseline = 1)
#' PCC <- getPCC(mse)
#' plotAllNetwork(PCC, labelSize = 60)


#(1) Method 1: faster, but introduce igrph package =============================

# plotAllNetwork <- function(PCC, threshold = 0.9, labelSize = 40){
#   #(1) Prepare igraph network object -----------------------------------------
#   PCC[PCC < threshold] = 0
#   network <- igraph::graph_from_adjacency_matrix(PCC, weighted = TRUE, mode = "lower", diag = FALSE)
#   isolated <- which(igraph::degree(network) == 0)
#   networkObject <- igraph::delete.vertices(network, isolated)
#   #(2) Plot network ----------------------------------------------------------
#   nodes <- igraph::as_data_frame(networkObject, what = "vertices")
#   colnames(nodes)[1] <- "id"
#   edges <- igraph::as_data_frame(networkObject, what = "edges")
#   nodes$label <- igraph::V(networkObject)$name
#   edges$value <- igraph::E(networkObject)$weight
#   visNetwork::visNetwork(nodes = nodes, edges = edges, width = "100%") %>%
#     visNetwork::visNodes(font = list(size = labelSize)) %>%
#     visNetwork::visOptions(highlightNearest = TRUE) %>%
#     visNetwork::visInteraction(navigationButtons = TRUE) %>%
#     visNetwork::visIgraphLayout(randomSeed = 123)
# }

#(2) Method 2: 4 times slower, but one package dependency less =================
plotAllNetwork <- function(PCC, threshold = 0.9, labelSize = 40){
  #(1) Prepare network object --------------------------------------------------
  PCC[upper.tri(PCC, diag = TRUE)] <- 0
  cor_df <- as.data.frame(as.table(PCC))
  cor_df <- subset(cor_df, Freq >= threshold)
  id <- unique(c(cor_df$Var1, cor_df$Var2))
  nodes <- data.frame(id = id, label = id)
  edges <- data.frame(to = cor_df$Var1, from = cor_df$Var2, value = cor_df$Freq)

  #(2) Plot network ------------------------------------------------------------
  visNetwork::visNetwork(nodes = nodes, edges = edges, width = "100%") %>%
    visNetwork::visNodes(font = list(size = labelSize)) %>%
    visNetwork::visOptions(highlightNearest = TRUE) %>%
    visNetwork::visInteraction(navigationButtons = TRUE) %>%
    visNetwork::visIgraphLayout(randomSeed = 123)
}


