# function to calculate analytical metrics of hierarchy on a sociomatrix

calculate_analytical_hierarhy_measures <- function(sociomatrix,
                                                   mode = "directed"){
    #statistics using igraph
    require(igraph)
    sociomatrix <- graph.adjacency(sociomatrix, mode = mode)

    #calculate global scores
    global <- list()
    global$degree_centralization <- centralization.degree (sociomatrix, mode = "all")$centralization
    global$closeness_centralization <- centralization.closeness (sociomatrix, mode = "all")$centralization
    global$betweenness_centralization <-  centralization.betweenness (sociomatrix, directed = TRUE)$centralization
    global$eigenvector_centralization <- centralization.evcent (sociomatrix, directed = TRUE)$centralization

    #calculate local scores
    local <- list()
    local$degree_centrality$score <- centralization.degree (sociomatrix, mode = "all")$res
    local$degree_centrality$rank <- order(local$degree_centrality$score, decreasing = T)
    local$closeness_centrality$score <- centralization.closeness (sociomatrix, mode = "all")$res
    local$closeness_centrality$rank <- order(local$closeness_centrality$score, decreasing = T)
    local$betweenness_centrality$score <-  centralization.betweenness (sociomatrix, directed = TRUE)$res
    local$betweenness_centrality$rank <- order(local$betweenness_centrality$score, decreasing = T)
    local$eigenvector_centrality$score <- centralization.evcent (sociomatrix, directed = TRUE)$vector
    local$eigenvector_centrality$rank <- order(local$eigenvector_centrality$score, decreasing = T)

    # return a list object with a $global and $local sublist, each of which contains the output from all of the different measures which are appropriately named. In the $local sublist, we provide the $rank and $score for each node in the network.
    return_list <- list(global = global, local = local)
    return(return_list)
}