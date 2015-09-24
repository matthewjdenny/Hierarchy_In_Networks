# analyze manager-to-manager email networks in 17 county governments
#works on Matt's Computer

#preliminaries
rm(list = ls())

#load in functions
source("./Scripts/calculate_analytical_hierarchy_measures.R")

#load data
load("./Data/Manager_Email_Networks.Rdata")

# calculate measures for all networks
Measures <- vector(length = length(Manager_Email_Networks), mode = "list")
for(i in 1:length(Manager_Email_Networks)){
    print(i)
    Measures[[i]] <- calculate_analytical_hierarhy_measures(
        sociomatrix = Manager_Email_Networks[[i]]$sociomatrix,
        mode = Manager_Email_Networks[[i]]$mode
                                                            )
}


#now score against





