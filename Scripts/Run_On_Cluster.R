# Run on Cluster
# R CMD BATCH /home/mdenny/Dropbox/SoDA_502/Hierarchy_In_Networks/Scripts/Run_On_Cluster.R > Simulate_Networks_12-11-15.txt &

################################################################################
# 0. Preliminaries -- set working directory, load packages, functions and data #
################################################################################


rm(list = ls())

# change your working directory to the "Hierarchy_In_Networks" folder location.
# for me, this is:
setwd("~/Dropbox/SoDA_502/Hierarchy_In_Networks")

# install and load functions (you will need the devtools pacakge to install two
# of them, which are necessary for making the pca plots, for example).
# devtools::install_github("vqv/ggbiplot")
# install.packages(c("GGally","ineq","corrplot","keyplayer","xtable","igraph","statnet"), dependencies = T)
require(ggbiplot)
require(igraph)
require(stringr)
require(GGally)
require(ineq)
require(corrplot)
require(xtable)
require(keyplayer)

# 0.1 -- load in functions which perform the analysis

# actually calculates hierarchy measures
source("./Scripts/calculate_analytical_hierarchy_measures.R")
# scores each measure based on where it places the manager of
# each of 17 counties in its local hierarchy rankings.
source("./Scripts/score_leadership_rank.R")
# general purpose extension to the matplot function
source("./Scripts/multi_plot.R")
# wrapper for calculate_analytical_hierarchy_measures(), does so for all networks
# and organizes data in a format that is nice for further analysis.
source('./Scripts/generate_hierarchy_dataset.R')
# calculates descriptive statistics (nodes, density, etc.) for a list of networks
source('./Scripts/calculate_descriptive_statistics.R')
# some extra utility function that make plotting easier and collapse datasets
# over the network_type field.
source('./Scripts/plotting_utility_functions.R')
# a wrapper for the pairs() function that makes things look good.
source('./Scripts/make_pairs_plots.R')
# a function that runs principle components analysis and generates plots with
# nice formatting. Optionally returns PCA object for further analysis.
source('./Scripts/generate_pca_plots.R')
# generates a sample of networks from the Barabasi-Albert model (prefferential
# attachment).
source('./Scripts/generate_barabasi_albert_networks.R')
# generates a sample of networks from the Erdos-Renyi model (random networks).
source('./Scripts/generate_erdos_renyi_networks.R')
# generates a sample of networks from from a deterministic tree model.
source('./Scripts/generate_tree_networks.R')


################################
###   Barabasi-Albert Model  ###
################################

# In general we out to expect that with more pref attachement comes more hierarchy
ba_networks <- generate_barabasi_albert_networks(
    nodes = c(50,100,150,200,250,300,400,500),
    samples = 250,
    seed = 12345,
    pref_attachment_params = c(0.5,1,2,5,10))

# generate local and global hierarchy measures for all networks and output them
# into a list object
ba_measures <- generate_hierarchy_dataset(ba_networks)
# extract a data.frame of global measures
ba_global_measures <- ba_measures$global_measure_dataframe

# collapse over parameter values and run principle components analysis, saving plots
# to the output_directory by default
generate_pca_plots(collapse_over_parameter(ba_global_measures),
                   name_stem = "BA_Size",
                   save_to_file = TRUE,
                   pca_choice1 = 1,
                   pca_choice2 = 2)

# collapse over network size and run principle components analysis, saving plots
# to the output_directory by default
generate_pca_plots(collapse_over_size(ba_global_measures),
                   name_stem = "BA_Param",
                   save_to_file = TRUE,
                   pca_choice1 = 1,
                   pca_choice2 = 2)

# now plot averages of each measure when collapsed over network size and parameter
# values
ba_param_averages <- average_over_type(collapse_over_size(ba_global_measures))
ba_size_averages <- average_over_type(collapse_over_parameter(ba_global_measures))

multi_plot(data = ba_param_averages,
           pdf_name = "./Output/BA_Param_Avg_Norm",
           output_pdf = T,
           c(1:12),
           connect_with_lines = T,
           normalize = T)

multi_plot(data = ba_size_averages,
           pdf_name = "./Output/BA_Size_Avg_Norm",
           output_pdf = T,
           c(1:12),
           connect_with_lines = T,
           normalize = T)

multi_plot(data = ba_param_averages,
           pdf_name = "./Output/BA_Param_Averages",
           output_pdf = T,
           c(1:12),
           connect_with_lines = T)

multi_plot(data = ba_size_averages,
           pdf_name = "./Output/BA_Size_Averages",
           output_pdf = T,
           c(1:12),
           connect_with_lines = T)


###################################
###   Tree-Structured Networks  ###
###################################

tr_networks <- generate_tree_networks(
    nodes = c(50,100,150,200,250,300,400,500),
    samples = 250,
    seed = 12345,
    children = c(2,5,10,25,50))

# generate local and global hierarchy measures for all networks and output them
# into a list object
tr_measures <- generate_hierarchy_dataset(tr_networks)
# extract a data.frame of global measures
tr_global_measures <- tr_measures$global_measure_dataframe

# collapse over parameter values and run principle components analysis, saving plots
# to the output_directory by default
generate_pca_plots(collapse_over_parameter(tr_global_measures),
                   name_stem = "TR_Size",
                   save_to_file = TRUE,
                   pca_choice1 = 1,
                   pca_choice2 = 2)

# collapse over network size and run principle components analysis, saving plots
# to the output_directory by default
generate_pca_plots(collapse_over_size(tr_global_measures),
                   name_stem = "TR_Param",
                   save_to_file = TRUE,
                   pca_choice1 = 1,
                   pca_choice2 = 2)

# now plot averages of each measure when collapsed over network size and parameter
# values
tr_param_averages <- average_over_type(collapse_over_size(tr_global_measures))
tr_size_averages <- average_over_type(collapse_over_parameter(tr_global_measures))

multi_plot(data = tr_param_averages,
           pdf_name = "./Output/TR_Param_Avg_Norm",
           output_pdf = T,
           c(1:12),
           connect_with_lines = T,
           normalize = T)

multi_plot(data = tr_size_averages,
           pdf_name = "./Output/TR_Size_Avg_Norm",
           output_pdf = T,
           c(1:12),
           connect_with_lines = T,
           normalize = T)

multi_plot(data = tr_param_averages,
           pdf_name = "./Output/TR_Param_Averages",
           output_pdf = T,
           c(1:12),
           connect_with_lines = T)

multi_plot(data = tr_size_averages,
           pdf_name = "./Output/TR_Size_Averages",
           output_pdf = T,
           c(1:12),
           connect_with_lines = T)



###############################
###   Erdos-Renyi Networks  ###
###############################

er_networks <- generate_erdos_renyi_networks(
    nodes = c(50,100,150,200,250,300,400,500),
    samples = 250,
    seed = 12345,
    p = c(0.05,0.075,0.1,0.125,,0.15))

# generate local and global hierarchy measures for all networks and output them
# into a list object
er_measures <- generate_hierarchy_dataset(er_networks)
# extract a data.frame of global measures
er_global_measures <- er_measures$global_measure_dataframe

# collapse over parameter values and run principle components analysis, saving plots
# to the output_directory by default
generate_pca_plots(collapse_over_parameter(er_global_measures),
                   name_stem = "ER_Size",
                   save_to_file = TRUE,
                   pca_choice1 = 1,
                   pca_choice2 = 2)

# collapse over network size and run principle components analysis, saving plots
# to the output_directory by default
generate_pca_plots(collapse_over_size(er_global_measures),
                   name_stem = "ER_Param",
                   save_to_file = TRUE,
                   pca_choice1 = 1,
                   pca_choice2 = 2)

# now plot averages of each measure when collapsed over network size and parameter
# values
er_param_averages <- average_over_type(collapse_over_size(er_global_measures))
er_size_averages <- average_over_type(collapse_over_parameter(er_global_measures))

multi_plot(data = er_param_averages,
           pdf_name = "./Output/ER_Param_Avg_Norm",
           output_pdf = T,
           c(1:12),
           connect_with_lines = T,
           normalize = T)

multi_plot(data = er_size_averages,
           pdf_name = "./Output/ER_Size_Avg_Norm",
           output_pdf = T,
           c(1:12),
           connect_with_lines = T,
           normalize = T)

multi_plot(data = er_param_averages,
           pdf_name = "./Output/ER_Param_Averages",
           output_pdf = T,
           c(1:9,11,12),
           connect_with_lines = T)

multi_plot(data = er_size_averages,
           pdf_name = "./Output/ER_Size_Averages",
           output_pdf = T,
           c(1:9,11,12),
           connect_with_lines = T)

# save all of the global measures
save(list = c("er_global_measures",
              "tr_global_measures",
              "ba_global_measures"),
     file = "./Data/Simulated_Network_Global_Measures.Rdata")


calculate_descriptive_statistics(ba_networks)[[2]]
calculate_descriptive_statistics(tr_networks)[[2]]
calculate_descriptive_statistics(er_networks)[[2]]

