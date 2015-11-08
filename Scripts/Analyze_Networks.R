# Replication script

#preliminaries
rm(list = ls())

# change your working directory to the "Hierarchy_In_Networks" folder location.
# for me, this is:
setwd("~/Dropbox/SoDA_502/Hierarchy_In_Networks")

# install and load functions
# devtools::install_github("matthewjdenny/SpeedReader")
# install_github("vqv/ggbiplot")
require(SpeedReader)
require(ggbiplot)
require(igraph)
require(stringr)

#load in functions
source("./Scripts/calculate_analytical_hierarchy_measures.R")
source("./Scripts/score_leadership_rank.R")
source("./Scripts/multi_plot.R")
source('./Scripts/generate_hierarchy_dataset.R')
source('./Scripts/calculate_descriptive_statistics.R')
source('./Scripts/generate_pca_plots.R')
source('./Scripts/generate_barabasi_albert_networks.R')



#load data
load("./Data/Network_Data.Rdata")

data_list <- generate_hierarchy_dataset(Network_Data)

global_measures <- data_list$global_measure_dataframe


#deal with weird networks
global_measures <- global_measures[-which(row.names(global_measures) == "mb031s01nets2"),]
global_measures <- global_measures[-which(row.names(global_measures) == "drugnet"),]
global_measures <- global_measures[-which(row.names(global_measures) == "Terro_4275"),]
global_measures <- global_measures[-which(row.names(global_measures) == "drug_net_61"),]
global_measures <- global_measures[-which(row.names(global_measures) == "Koster_data8"),]
global_measures <- global_measures[-which(row.names(global_measures) == "CITIES"),]
global_measures <- global_measures[-which(row.names(global_measures) == "AOM_division_comembership"),]
global_measures <- global_measures[-which(row.names(global_measures) == "Freeman's_EIES3"),]


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

# run pca analysis
generate_pca_plots(global_measures,
                   save_to_file = TRUE,
                   pca_choice1 = 1,
                   pca_choice2 = 2)
generate_pca_plots(global_measures,
                   save_to_file = TRUE,
                   pca_choice1 = 2,
                   pca_choice2 = 3)
generate_pca_plots(global_measures,
                   save_to_file = TRUE,
                   pca_choice1 = 1,
                   pca_choice2 = 3)


# Time to simulate some networks

################################
###   Barabasi-Albert Model  ###
################################
ba_networks <- generate_barabasi_albert_networks(nodes = seq(25,100,by = 25),
                    samples = 500,
                    seed = 12345)

ba_measures <- generate_hierarchy_dataset(ba_networks)
ba_global_measures <- ba_measures$global_measure_dataframe

# collapse over pref attachment parameters
generate_pca_plots(collapse_over_parameter(ba_global_measures),
                   name_stem = "BA_Size",
                   save_to_file = TRUE,
                   pca_choice1 = 1,
                   pca_choice2 = 2)

# collapse over network size
generate_pca_plots(collapse_over_size(ba_global_measures),
                   name_stem = "BA_Param",
                   save_to_file = TRUE,
                   pca_choice1 = 1,
                   pca_choice2 = 2)

ba_param_averages <- average_over_type(collapse_over_size(ba_global_measures))
ba_size_averages <- average_over_type(collapse_over_parameter(ba_global_measures))


multi_plot(data = ba_param_averages,
           pdf_name = "Global_Measures",
           output_pdf = F,
           c(1:8))
