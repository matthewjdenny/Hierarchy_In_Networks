generate_pca_plots <- function(global_measures,
                               save_to_file = FALSE,
                               name_stem = "Observed",
                               output_directory = "./Output",
                               return_pca_object = FALSE,
                               pca_choice1 = 1,
                               pca_choice2 = 2,
                               elipse = FALSE,
                               circle = TRUE){

    #remove rows with NA for triangle transitivity
    rem <- which(is.na(global_measures$triangle_transitivity))
    global_measures <- global_measures[-rem,]


    if(length(which(is.na(global_measures$D_root))) > 0 & length(unique(global_measures$eigenvector_centralization)) > 1){
           # pca with all measures except D_root which was mostly NA.
           pca <- with(global_measures, prcomp(~degree_centralization +
                                                   closeness_centralization +
                                                   betweenness_centralization +
                                                   eigenvector_centralization +
                                                   landau +
                                                   kendall +
                                                   GRC +
                                                   m_close +
                                                   m_degree +
                                                   krackhardt +
                                                   triangle_transitivity,
                                               scale = TRUE,
                                               center = TRUE))
       }

    if(length(which(is.na(global_measures$D_root))) == 0 & length(unique(global_measures$eigenvector_centralization)) == 1){
           # pca with all measures except D_root which was mostly NA.
           pca <- with(global_measures, prcomp(~degree_centralization +
                                                   closeness_centralization +
                                                   betweenness_centralization +
                                                   landau +
                                                   kendall +
                                                   GRC +
                                                   D_root +
                                                   m_close +
                                                   m_degree +
                                                   krackhardt +
                                                   triangle_transitivity,
                                               scale = TRUE,
                                               center = TRUE))
    }


    components <- data.frame(x = 1:length(pca$sdev),
                        y = (pca$sdev)^2)
    facts=as.factor(global_measures$network_type)


    if(save_to_file){
        currentwd <- getwd()
        setwd(output_directory)

        pdf(file = paste(name_stem,"_PCA_Component_Varinces.pdf",sep = ""),
            width = 6, height = 5)
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

        pdf(file = paste(name_stem,"_PCA_Components",pca_choice1,"_",
                         pca_choice2,".pdf",sep = ""), width = 12, height = 8)
        pca.g <- ggbiplot(pca, choices = c(pca_choice1, pca_choice2),
                          obs.scale = 1,
                          var.scale = 1,
                          ellipse = elipse,
                          circle = circle,
                          groups = levels(facts)[unclass(facts)]) +
            scale_color_discrete(name = '') +
            theme(legend.direction = 'horizontal', legend.position = 'top')+
            ggtitle(paste("Principal Components:",pca_choice1,"and",pca_choice2))
        print(pca.g)
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
        pca.g <- ggbiplot(pca, choices = c(pca_choice1, pca_choice2),
                          obs.scale = 1,
                          var.scale = 1,
                          ellipse = elipse,
                          circle = circle,
                          groups = levels(facts)[unclass(facts)]) +
            scale_color_discrete(name = '') +
            theme(legend.direction = 'horizontal', legend.position = 'top')+
            ggtitle(paste("Principal Components:",pca_choice1,"and",pca_choice2))
        print(pca.g)
    }
    if(return_pca_object){
        return(pca)
    }
}

