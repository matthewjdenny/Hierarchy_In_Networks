# Replication script

# 0. Preliminaries

rm(list = ls())

# change your working directory to the "Hierarchy_In_Networks" folder location.
# for me, this is:
setwd("~/Dropbox/SoDA_502/Hierarchy_In_Networks")

# install and load functions (you will need the devtools pacakge to install two
# of them, which are necessary for making the pca plots, for example).
# devtools::install_github("matthewjdenny/SpeedReader",.opts = list(ssl.verifypeer = FALSE))
# devtools::install_github("vqv/ggbiplot")
require(SpeedReader)
require(ggbiplot)
require(igraph)
require(stringr)
require(GGally)

#load in functions
source("./Scripts/calculate_analytical_hierarchy_measures.R")
source("./Scripts/score_leadership_rank.R")
source("./Scripts/multi_plot.R")
source('./Scripts/generate_hierarchy_dataset.R')
source('./Scripts/calculate_descriptive_statistics.R')
source('./Scripts/plotting_utility_functions.R')
source('./Scripts/make_pairs_plots.R')
source('./Scripts/generate_pca_plots.R')
source('./Scripts/generate_barabasi_albert_networks.R')
source('./Scripts/generate_erdos_renyi_networks.R')
source('./Scripts/generate_tree_networks.R')

# 1. Observed Networks

#load data
load("./Data/Network_Data.Rdata")

data_list <- generate_hierarchy_dataset(Network_Data)

global_measures <- data_list$global_measure_dataframe

# remove networks the appear to be outliers so that they do not drive analysis.
remove <- c("mb031s01nets2","drugnet","Terro_4275","drug_net_61","Koster_data8",
            "CITIES","AOM_division_comembership","Freeman's_EIES3")
global_measures <- remove_rows(global_measures,remove)

# save(global_measures, file = "./Data/global_hierarchy_measures.Rdata")

multi_plot(data = global_measures,
           pdf_name = "Global_Measures",
           output_pdf = F,
           c(2:5,7))

multi_plot(data = data_list$leadership_ranking_scores,
           pdf_name = "Measure_Scores",
           output_pdf = F)

# two different alternatives for making pairs plots of all variables
# The super fancy way
pdf(file = "./Output/Fancy_Pairs_Plot.pdf", width = 40, height= 40)
ggpairs(global_measures[,c(1:7,9)], colour='network_type', alpha=0.4)
dev.off()
# the home-grown way
make_pairs_plots(global_measures,
                 save_pdf = TRUE)

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

# 2. Simulated Networks

################################
###   Barabasi-Albert Model  ###
################################

# In general we out to expect that with more pref attachement comes more hierarchy
ba_networks <- generate_barabasi_albert_networks(
    nodes = c(50,200,500),
    samples = 500,
    seed = 12345,
    pref_attachment_params = c(0.5,1,2,5,10))

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

# now plot measure averages
ba_param_averages <- average_over_type(collapse_over_size(ba_global_measures))
ba_size_averages <- average_over_type(collapse_over_parameter(ba_global_measures))
multi_plot(data = ba_param_averages,
           pdf_name = "./Output/BA_Param_Averages",
           output_pdf = T,
           c(1:8),
           connect_with_lines = T)

multi_plot(data = ba_size_averages,
           pdf_name = "./Output/BA_Size_Averages",
           output_pdf = T,
           c(1:8),
           connect_with_lines = T)


###################################
###   Tree-Structured Networks  ###
###################################

tr_networks <- generate_tree_networks(
    nodes = c(50,200,500),
    samples = 500,
    seed = 12345,
    children = c(2,5,10,50))

tr_measures <- generate_hierarchy_dataset(tr_networks)
tr_global_measures <- tr_measures$global_measure_dataframe

# collapse over pref attachment parameters
generate_pca_plots(collapse_over_parameter(tr_global_measures),
                   name_stem = "TR_Size",
                   save_to_file = TRUE,
                   pca_choice1 = 1,
                   pca_choice2 = 2)

# collapse over network size
generate_pca_plots(collapse_over_size(tr_global_measures),
                   name_stem = "TR_Param",
                   save_to_file = TRUE,
                   pca_choice1 = 1,
                   pca_choice2 = 2)

# now plot measure averages
tr_param_averages <- average_over_type(collapse_over_size(tr_global_measures))
tr_size_averages <- average_over_type(collapse_over_parameter(tr_global_measures))
multi_plot(data = tr_param_averages,
           pdf_name = "./Output/TR_Param_Averages",
           output_pdf = T,
           c(1:8),
           connect_with_lines = T)

multi_plot(data = tr_size_averages,
           pdf_name = "./Output/TR_Size_Averages",
           output_pdf = T,
           c(1:8),
           connect_with_lines = T)



###############################
###   Erdos-Renyi Networks  ###
###############################

er_networks <- generate_erdos_renyi_networks(
    nodes = c(50,200,500),
    samples = 500,
    seed = 12345,
    p = c(0.05,0.1,0.25,0.5))

er_measures <- generate_hierarchy_dataset(er_networks)
er_global_measures <- er_measures$global_measure_dataframe

# collapse over pref attachment parameters
generate_pca_plots(collapse_over_parameter(er_global_measures),
                   name_stem = "ER_Size",
                   save_to_file = TRUE,
                   pca_choice1 = 1,
                   pca_choice2 = 2)

# collapse over network size
generate_pca_plots(collapse_over_size(er_global_measures),
                   name_stem = "ER_Param",
                   save_to_file = TRUE,
                   pca_choice1 = 1,
                   pca_choice2 = 2)

# now plot measure averages
er_param_averages <- average_over_type(collapse_over_size(er_global_measures))
er_size_averages <- average_over_type(collapse_over_parameter(er_global_measures))
multi_plot(data = er_param_averages,
           pdf_name = "./Output/ER_Param_Averages",
           output_pdf = T,
           c(1:7),
           connect_with_lines = T)

multi_plot(data = er_size_averages,
           pdf_name = "./Output/ER_Size_Averages",
           output_pdf = T,
           c(1:7),
           connect_with_lines = T)


save(list = c("er_global_measures",
              "tr_global_measures",
              "ba_global_measures"),
     file = "./Data/Simulated_Network_Global_Measures.Rdata")




