#calculate measures for all networks
#works on Matt's Computer

#preliminaries
rm(list = ls())

#load in functions
source("./Scripts/calculate_analytical_hierarchy_measures.R")
source("./Scripts/score_leadership_rank.R")
source("./Scripts/multi_plot.R")

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


multi_plot(data = global_measures,
           pdf_name = "Global_Measures",
           output_pdf = T,
           2:4)


#now score against
measure_scores <- score_leadership_rank(Network_Data = Network_Data,
                                        Measures = Measures)

multi_plot(data = measure_scores,
           pdf_name = "Measure_Scores",
           output_pdf = T)

colMeans(measure_scores[1:17,])
colMeans(measure_scores[1:29,])

