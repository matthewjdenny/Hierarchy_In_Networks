
generate_barabasi_albert_networks <- function(nodes = seq(25,100,by = 25),
                              samples = 500,
                              seed = 12345){

    set.seed(seed)
    num_network_sizes <- length(nodes)

    #generate networks from barabasi-albert model
    pref_attachment_params <- c(0.5,1,1.5,2,2.5)
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

# string <- "BA_n-100_p-0.5_s-123"
collapse_over_size <- function(dataframe){
    remove_n <- function(string){
        paste0(stringr::str_split(string,"_")[[1]][c(1,3)],collapse = " ")
    }
    dataframe$network_type <- sapply(dataframe$network_type, remove_n)
    return(dataframe)
}

collapse_over_parameter <- function(dataframe){
    remove_n <- function(string){
        paste0(stringr::str_split(string,"_")[[1]][c(1,2)],collapse = " ")
    }
    dataframe$network_type <- sapply(dataframe$network_type, remove_n)
    return(dataframe)
}
