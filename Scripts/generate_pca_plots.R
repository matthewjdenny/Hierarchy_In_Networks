generate_pca_plots <- function(global_measures,
                               save_to_file = FALSE,
                               output_directory = "./Output",
                               return_pca_object = FALSE){
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

    components <- data.frame(x = 1:length(pca$sdev),
                        y = (pca$sdev)^2)
    facts=as.factor(global_measures$network_type)
    pca.g <- ggbiplot(pca, choices = c(1,2),
                      obs.scale = 1,
                      var.scale = 1,
                      ellipse = TRUE,
                      circle = TRUE,
                      groups = levels(facts)[unclass(facts)]) +
        scale_color_discrete(name = '') +
        theme(legend.direction = 'horizontal', legend.position = 'top')

    if(save_to_file){
        currentwd <- getwd()
        setwd(output_directory)

        pdf(file = "PCA_Component_Varinces.pdf", width = 6, height = 5)
        plot(components,
             pch = 19,
             ylab = "Variances",
             xlab = "Component",
             main = "Principle Component Variances")
        lines(y = c(1,1),
              x= c(min(components$x),max(components$x)),
              lwd = 2,
              col = "red")
        dev.off()

        pdf(file = "PCA_Plot.pdf", width = 12, height = 8)
        pca.g
        dev.off()

        setwd(currentwd)
    }else{
        plot(components,
             pch = 19,
             ylab = "Variances",
             xlab = "Component",
             main = "Principle Component Variances")
        lines(y = c(1,1),
              x= c(min(components$x),max(components$x)),
              lwd = 2,
              col = "red")
        Sys.sleep(2)
        print(pca.g)
    }
    if(return_pca_object){
        return(pca)
    }
}

