#calculate measures for all networks
#works on Matt's Computer

#preliminaries
rm(list = ls())

#load in functions
source("./Scripts/calculate_analytical_hierarchy_measures.R")

#load data
load("./Data/Network_Data.Rdata")

# calculate measures for all networks
Measures <- vector(length = length(Network_Data), mode = "list")
for(i in 1:length(Network_Data)){
    print(i)
    Measures[[i]] <- calculate_analytical_hierarhy_measures(
        sociomatrix = Network_Data[[i]]$sociomatrix,
        mode = Network_Data[[i]]$mode
    )
}

#populate dataframe with global measures
global_measures <- data.frame(matrix(0,
                                     nrow = length(Measures),
                                     ncol = length(Measures[[1]]$global)))
for(i in 1:length(Measures)){
    global_measures[i,] <- unlist(Measures[[i]]$global)
}
colnames(global_measures) <- names((Measures[[1]]$global))
rownames(global_measures) <- names(Network_Data)

save(global_measures, file = "./Data/global_hierarchy_measures.Rdata")

#now score against
