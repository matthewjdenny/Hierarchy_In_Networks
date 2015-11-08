generate_hierarchy_dataset <- function(Network_Data){
    # calculate the measures
    Measures <- vector(length = length(Network_Data), mode = "list")
    for(i in 1:length(Network_Data)){
        cat("Calculating hierarchy measures for",names(Network_Data)[i],"number",i,"of",length(Network_Data),"\n")
        Measures[[i]] <- calculate_analytical_hierarhy_measures(
            sociomatrix = Network_Data[[i]]$sociomatrix,
            mode = Network_Data[[i]]$mode
        )
    }
    names(Measures) <- names(Network_Data)
    #populate dataframe with global measures
    global_measures <- data.frame(matrix(0,
                                         nrow = length(Measures),
                                         ncol = length(Measures[[1]]$global)))
    for(i in 1:length(Measures)){
        global_measures[i,] <- unlist(Measures[[i]]$global)
    }
    colnames(global_measures) <- names((Measures[[1]]$global))
    rownames(global_measures) <- names(Network_Data)

    #add on columns with type and name.
    network_type <- rep("",nrow(global_measures))
    network_name <- names(Network_Data)
    for(i in 1:length(network_type)){
        network_type[i] <- Network_Data[[i]]$type
    }
    global_measures <- data.frame(cbind(global_measures,
                                        network_type,
                                        network_name),
                                  stringsAsFactors = F)
    global_measures$network_type <- as.character(global_measures$network_type)
    global_measures$network_name <- as.character(global_measures$network_name)

    #now score against their leadership rank
    measure_scores <- score_leadership_rank(
        Network_Data = Network_Data,
        Measures = Measures)

    return(list(network_measure_list = Measures,
                global_measure_dataframe =  global_measures,
                leadership_ranking_scores = measure_scores,
                network_list = Network_Data))
}