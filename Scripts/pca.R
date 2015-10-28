library(ggbiplot)

library(devtools)
install_github("vqv/ggbiplot")

# global_measures <- load("~/Dropbox/projects/work/soda502_hierarchy/Hierarchy_In_Networks/Data/global_hierarchy_measures.Rdata")

# on Matt's computer or any computer where you have opened the RStudio project
load("./Data/global_hierarchy_measures.Rdata")

# updated to new
# pca <- with(global_measures, prcomp(~degree_centralization + closeness_centralization +
#                 betweenness_centralization + eigenvector_centralization,
#               scale = TRUE,
#               center = TRUE))

# pca with all measures except D_root which was mostly NA.
pca <- with(global_measures, prcomp(~degree_centralization +
                                    closeness_centralization +
                                    betweenness_centralization +
                                    eigenvector_centralization +
                                    landau +
                                    kendall +
                                    GRC,
                                    scale = TRUE,
                                    center = TRUE))

plot(pca, type = "l")
hold=matrix(nrow=length(Network_Data),ncol=2)
for(i in 1:length(Network_Data)){
  hold[i,]=c(names(Network_Data)[i],Network_Data[[i]]$type)
}
facts=as.factor(hold[-c(100,52,54,128,53,78),2])

pca.g <- ggbiplot(pca, choices = c(1,2),
                  obs.scale = 1,
                  var.scale = 1,
                  ellipse = TRUE,
                  circle = TRUE,
                  groups = c("black","yellow","red","orange","green","blue",
                             "violetred4","maroon1","tan2")[unclass(facts)])

pca.g
