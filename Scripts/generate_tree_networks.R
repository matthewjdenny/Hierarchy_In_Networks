
generate_tree_networks <- function(nodes = c(50,200,500),
                                   samples = 500,
                                   seed = 12345,
                                   children = c(2,5,10,50)){

    set.seed(seed)
    num_network_sizes <- length(nodes)

    num_params <- length(children)

    #pre-allocate list
    tree_networks <- vector(
        mode = "list",
        length = num_network_sizes*num_params*samples)

    # loop over everything to generate networks
    counter <- 1
    for(i in 1:num_network_sizes){
        cat("Tree network with:",nodes[i],"nodes...\n")
        for(j in 1:num_params){
            cat("Current numer of children:", children[j],"\n")
            for(k in 1:samples){
                net <- get.adjacency(igraph::make_tree(n = nodes[i],
                                                       children = children[j],
                                                       mode = "out"))
                ins <- list(sociomatrix = net,
                            mode = "directed",
                            weighted = TRUE,
                            type = paste("TR_n-",nodes[i],
                                         "_p-",children[j],sep = ""))
                tree_networks[[counter]] <- ins
                names(tree_networks)[counter] <- paste("n-",nodes[i],
                                                       "_p-",children[j],"_s-",
                                                       k,sep = "")
                counter <- counter + 1
            }
        }
    }
    return(tree_networks)
}


