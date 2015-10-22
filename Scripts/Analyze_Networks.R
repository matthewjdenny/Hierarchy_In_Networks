#calculate measures for all networks
#works on Matt's Computer

#preliminaries
rm(list = ls())

#load in functions
source("./Scripts/calculate_analytical_hierarchy_measures.R")
source("./Scripts/score_leadership_rank.R")
source("./Scripts/multi_plot.R")
source('./Scripts/generate_hierarchy_dataset.R')

#for better plotting
#devtools::install_github("matthewjdenny/SpeedReader")
library(SpeedReader)


#load data
load("./Data/Network_Data.Rdata")

data_list <- generate_hierarchy_dataset(Network_Data)

multi_plot(data = data_list$global_measure_dataframe,
           pdf_name = "Global_Measures",
           output_pdf = F,
           c(2:5,7))

multi_plot(data = data_list$leadership_ranking_scores,
           pdf_name = "Measure_Scores",
           output_pdf = F)

# calculate some descriptive statistics
desc_stats <- matrix(0,nrow = length(Network_Data), ncol = 2)
for(i in 1:length(Network_Data)){
    desc_stats[i,1] <- sum(Network_Data[[i]]$sociomatrix)
    desc_stats[i,2] <- nrow(Network_Data[[i]]$sociomatrix)
}
mean(desc_stats[1:17,1])
mean(desc_stats[1:17,2])
mean(desc_stats[18:35,1])
mean(desc_stats[18:35,2])




