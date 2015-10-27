#calculate measures for all networks
#works on Matt's Computer

#preliminaries
rm(list = ls())

# change your working directory to the "Hierarchy_In_Networks" folder location.
# for me, this is:
setwd("~/Dropbox/SoDA_502/Hierarchy_In_Networks")

#load in functions
source("./Scripts/calculate_analytical_hierarchy_measures.R")
source("./Scripts/score_leadership_rank.R")
source("./Scripts/multi_plot.R")
source('./Scripts/generate_hierarchy_dataset.R')
source('./Scripts/calculate_descriptive_statistics.R')

# for better plotting you can try this
# devtools::install_github("matthewjdenny/SpeedReader")
# library(SpeedReader)

#load data
load("./Data/Network_Data.Rdata")

data_list <- generate_hierarchy_dataset(Network_Data)

global_measures <- data_list$global_measure_dataframe
#deal with weird networks
global_measures <- global_measures[-100,]
global_measures <- global_measures[-which(row.names(global_measures) == "drugnet"),]
global_measures <- global_measures[-which(row.names(global_measures) == "Terro_4275"),]
global_measures <- global_measures[-which(row.names(global_measures) == "drug_net_61"),]
global_measures <- global_measures[-which(row.names(global_measures) == "Koster_data8"),]
global_measures <- global_measures[-which(row.names(global_measures) == "CITIES"),]

save(global_measures, file = "./Data/global_hierarchy_measures.Rdata")

multi_plot(data = global_measures,
           pdf_name = "Global_Measures",
           output_pdf = F,
           c(2:5,7))

multi_plot(data = data_list$leadership_ranking_scores,
           pdf_name = "Measure_Scores",
           output_pdf = F)

# calculate some descriptive statistics
descriptive_stats <- calculate_descriptive_statistics(Network_Data)
network_descriptive_statistics <- descriptive_stats[[1]]
type_descriptive_statistics <- descriptive_stats[[2]]



