calculate_descriptive_statistics <- function(Network_Data){

    local_statistics <- matrix(0,nrow = length(Network_Data), ncol = 4)
    network_types <- rep("",length(Network_Data))
    for(i in 1:length(Network_Data)){
        sociomatrix <- graph.adjacency(Network_Data[[i]]$sociomatrix,
                                       mode = Network_Data[[i]]$mode)
        local_statistics[i,1] <- nrow(Network_Data[[i]]$sociomatrix)
        local_statistics[i,2] <- sum(Network_Data[[i]]$sociomatrix)
        local_statistics[i,3] <- igraph::graph.density(sociomatrix)
        local_statistics[i,4] <- igraph::transitivity(sociomatrix, type="global")
        network_types[i] <- Network_Data[[i]]$type
    }
    rownames(local_statistics) <- names(Network_Data)
    colnames(local_statistics) <- c("nodes","edges","density","clustering_coefficient")
    local_statistics <- data.frame(local_statistics)

    #calculate type averages
    unique_types <- unique(network_types)
    type_statistics <- matrix(0,nrow = length(unique_types), ncol = 5)
    for(i in 1:length(unique_types)){
        cur <- local_statistics[which(network_types == unique_types[i]),]
        type_statistics[i,] <- c(length(which(network_types == unique_types[i])),colMeans(cur, na.rm = T))
    }
    rownames(type_statistics) <- unique_types
    colnames(type_statistics) <- c("number","nodes","edges","density","clustering_coefficient")
    type_statistics <- data.frame(type_statistics)

    return(list(network_statistics = local_statistics,
                type_statistics = type_statistics))
}