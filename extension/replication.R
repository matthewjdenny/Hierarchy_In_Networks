
## Do not need to rerun this
# read in the raw data
# data <- readr::read_delim(file = "/Users/matthewjdenny/Dropbox/replication_data/analysis/baseline/baseline_release_metadata.csv",
#                  delim = ",",
#                  col_names = TRUE)
# read in the raw baseline risk indicator
# risk <- readr::read_delim(file = "/Users/matthewjdenny/Dropbox/SoDA_502/synthetic_text/data/results/risk_results_baseline.csv",
# 				          delim = ",",
# 				          col_names = TRUE)

# set your working directory
setwd("~/Dropbox/SoDA_502/synthetic_text/extension/")
# load in the data
load("Replication_Data.Rdata")
# generate colors for map, red means the person was identified, blue means not.
baseline_color <- rep("black",nrow(data))
for(i in 1:nrow(risk)){
    print(i)
    inds <- which(data$user_id == risk$user_id[i])
    if(risk$identified[i]){
        baseline_color[inds] <- "red"
    }else{
        baseline_color[inds] <- "blue"
    }
}

#already added this to the dataset
# data <- data.frame(cbind(data,baseline_color),stringsAsFactors = F)

# example plot
pdf(file = "Baseline_Risk_Map.pdf", height = 25, width = 25)
plot(x = data$longitude,
     y = data$latitude,
     pch = ".",
     col = baseline_color)
dev.off()

#save(list = ls(), file = "~/Desktop/Replication_Data.Rdata")
