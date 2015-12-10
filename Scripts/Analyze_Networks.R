# Replication script for "Hierarchy in Networks"

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
require(ggbiplot)
require(igraph)
require(stringr)
require(GGally)
require(ineq)
require(corrplot)
require(xtable)

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

################################################################################
#                         1. Analyze Observed Networks                         #
################################################################################

# load data the observed network data
load("./Data/Network_Data.Rdata")

# generate local and global hierarchy measures for all networks and output them
# into a list object
data_list <- generate_hierarchy_dataset(Network_Data)
# extract a data.frame of global measures
global_measures <- data_list$global_measure_dataframe

# remove networks the appear to be outliers so that they do not drive analysis.
remove <- c("mb031s01nets2","drugnet","Terro_4275","drug_net_61","Koster_data8",
            "CITIES","AOM_division_comembership","Freeman's_EIES3", "pv504",
            "pv960", "Koster_data7")
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
# First: the super fancy way
pdf(file = "./Output/Fancy_Pairs_Plot.pdf", width = 40, height= 40)
ggpairs(global_measures[,c(1:7,9)],
        colour=as.factor(global_measures$network_type), alpha=0.4)
dev.off()
# Second: the home-grown way
make_pairs_plots(global_measures,
                 save_pdf = TRUE)

# calculate some descriptive statistics
Reduced_Data  <- remove_rows(Network_Data, remove, TRUE)
descriptive_stats <- calculate_descriptive_statistics(Reduced_Data)
network_descriptive_statistics <- descriptive_stats[[1]]
type_descriptive_statistics <- descriptive_stats[[2]]
xtable(type_descriptive_statistics)


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

# generate correlation plots of global measures against eachother
M <- global_measures[,c(1:7,9,10)]
cn <- as.character(sapply(colnames(M),replac))
colnames(M) <- cn
M <- cor(M)

# first just plain correlation plot
pdf(file = "./Output/Global_Measure_Correlations.pdf", height = 11, width = 11)
corrplot(M, method = "pie",diag = F,tl.col = "black",tl.pos = "d")
dev.off()

# now with significance tests
res1 <- cor.mtest(M, 0.95)
pdf(file = "./Output/Global_Measure_Correlations_with_Tests.pdf", height =11, width = 11)
corrplot(M, p.mat = res1[[1]], sig.level = 0.05, method = "pie",diag = F,tl.col = "black",tl.pos = "d")
dev.off()

################################################################################
#                         1. Analyze Simulated Networks                        #
################################################################################

# instead of having to rerun the network generation functions, you can simply load
# in the aggregate measures from the following file.
load("./Data/Simulated_Network_Global_Measures.Rdata")
# If you choose to do this, you will not need to run any of the
# generate_xxx_networks() functions or the generate_hierarchy_dataset() function,
# which has the longest runtime. Total runtime if replicating from scratch
# should be 2-3 hours on a laptop.

################################
###   Barabasi-Albert Model  ###
################################

# In general we out to expect that with more pref attachement comes more hierarchy
ba_networks <- generate_barabasi_albert_networks(
    nodes = c(50,200,500),
    samples = 500,
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
           c(1:10),
           connect_with_lines = T,
           normalize = T)

multi_plot(data = ba_size_averages,
           pdf_name = "./Output/BA_Size_Avg_Norm",
           output_pdf = T,
           c(1:10),
           connect_with_lines = T,
           normalize = T)

multi_plot(data = ba_param_averages,
           pdf_name = "./Output/BA_Param_Averages",
           output_pdf = T,
           c(1:10),
           connect_with_lines = T)

multi_plot(data = ba_size_averages,
           pdf_name = "./Output/BA_Size_Averages",
           output_pdf = T,
           c(1:10),
           connect_with_lines = T)


###################################
###   Tree-Structured Networks  ###
###################################

tr_networks <- generate_tree_networks(
    nodes = c(50,200,500),
    samples = 500,
    seed = 12345,
    children = c(2,5,10,50))

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
           c(1:10),
           connect_with_lines = T,
           normalize = T)

multi_plot(data = tr_size_averages,
           pdf_name = "./Output/TR_Size_Avg_Norm",
           output_pdf = T,
           c(1:10),
           connect_with_lines = T,
           normalize = T)

multi_plot(data = tr_param_averages,
           pdf_name = "./Output/TR_Param_Averages",
           output_pdf = T,
           c(1:10),
           connect_with_lines = T)

multi_plot(data = tr_size_averages,
           pdf_name = "./Output/TR_Size_Averages",
           output_pdf = T,
           c(1:10),
           connect_with_lines = T)



###############################
###   Erdos-Renyi Networks  ###
###############################

er_networks <- generate_erdos_renyi_networks(
    nodes = c(50,200,500),
    samples = 500,
    seed = 12345,
    p = c(0.05,0.075,0.1))

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
           c(1:10),
           connect_with_lines = T,
           normalize = T)

multi_plot(data = er_size_averages,
           pdf_name = "./Output/ER_Size_Avg_Norm",
           output_pdf = T,
           c(1:10),
           connect_with_lines = T,
           normalize = T)

multi_plot(data = er_param_averages,
           pdf_name = "./Output/ER_Param_Averages",
           output_pdf = T,
           c(1:7,9,10),
           connect_with_lines = T)

multi_plot(data = er_size_averages,
           pdf_name = "./Output/ER_Size_Averages",
           output_pdf = T,
           c(1:7,9,10),
           connect_with_lines = T)

# save all of the global measures
save(list = c("er_global_measures",
              "tr_global_measures",
              "ba_global_measures"),
     file = "./Data/Simulated_Network_Global_Measures.Rdata")


calculate_descriptive_statistics(ba_networks)[[2]]
calculate_descriptive_statistics(tr_networks)[[2]]
calculate_descriptive_statistics(er_networks)[[2]]


# not sure what this does-- why is this in here?
par(mfrow=c(2,2))
multi_plot(data = ba_size_avg_norm,
           output_pdf = F,
           c(1:10),
           connect_with_lines = T)
multi_plot(data = tr_size_avg_norm,
           output_pdf = F,
           c(1:10),
           connect_with_lines = T,legend_location="")
multi_plot(data = er_size_avg_norm,
           output_pdf = F,
           c(1:10),
           connect_with_lines = T,legend_location="")