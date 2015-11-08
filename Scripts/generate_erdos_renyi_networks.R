
generate_erdos_renyi_networks <- function(nodes = c(50,200,500),
                                   samples = 500,
                                   seed = 12345,
                                   p = c(0.05,0.1,0.25,0.5)){

    set.seed(seed)
    num_network_sizes <- length(nodes)

    num_params <- length(p)

    #pre-allocate list
    er_networks <- vector(
        mode = "list",
        length = num_network_sizes*num_params*samples)

    # loop over everything to generate networks
    counter <- 1
    for(i in 1:num_network_sizes){
        cat("Erdos-Renyi network with:",nodes[i],"nodes...\n")
        for(j in 1:num_params){
            cat("Current value for p:", p[j],"\n")
            for(k in 1:samples){
                net <- get.adjacency(igraph::erdos.renyi.game(n = nodes[i],
                                                              p = p[j],
                                                              type="gnp",
                                                              directed = TRUE))
                ins <- list(sociomatrix = net,
                            mode = "directed",
                            weighted = TRUE,
                            type = paste("ER_n-",nodes[i],
                                         "_p-",p[j],sep = ""))
                er_networks[[counter]] <- ins
                names(er_networks)[counter] <- paste("n-",nodes[i],
                                                       "_p-",p[j],"_s-",
                                                       k,sep = "")
                counter <- counter + 1
            }
        }
    }
    return(er_networks)
}


