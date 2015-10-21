library(ggbiplot)

global_measures <- load("~/Dropbox/projects/work/soda502_hierarchy/Hierarchy_In_Networks/Data/global_hierarchy_measures.Rdata")

# on Matt's computer
load("./Data/global_hierarchy_measures.Rdata")

pca <- with(global_measures, prcomp(~degree_centralization + closeness_centralization +
                betweenness_centralization + eigenvector_centralization,
              scale = TRUE,
              center = TRUE))

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

pca.g <- ggbiplot(pca, choices = c(1,2),
                  obs.scale = 1,
                  var.scale = 1,
                  ellipse = TRUE,
                  circle = TRUE)