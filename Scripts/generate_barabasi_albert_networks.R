
generate_barabasi_albert_networks <- function(nodes = c(50,200,500),
                              samples = 500,
                              seed = 12345,
                              pref_attachment_params = c(0.5,1,2,5,10)){

    set.seed(seed)
    num_network_sizes <- length(nodes)

    #generate networks from barabasi-albert model
    num_params <- length(pref_attachment_params)

    #pre-allocate list
    barabasi_albert_networks <- vector(
        mode = "list",
        length = num_network_sizes*num_params*samples)

    # loop over everything to generate networks
    counter <- 1
    for(i in 1:num_network_sizes){
        cat("Barabasi-Albert game with:",nodes[i],"nodes...\n")
        for(j in 1:num_params ){
            cat("Current prefferential attachment parameter:",
                pref_attachment_params[j],"\n")
            for(k in 1:samples){
                net <- get.adjacency(igraph::sample_pa(n = nodes[i],power = pref_attachment_params[j]))
                ins <- list(sociomatrix = net,
                            mode = "directed",
                            weighted = TRUE,
                            type = paste("BA_n-",nodes[i],
                                         "_p-",pref_attachment_params[j],sep = ""))
                barabasi_albert_networks[[counter]] <- ins
                names(barabasi_albert_networks)[counter] <- paste("n-",nodes[i],
                    "_p-",pref_attachment_params[j],"_s-",k,sep = "")
                counter <- counter + 1
            }
        }
    }
    return(barabasi_albert_networks)
}


