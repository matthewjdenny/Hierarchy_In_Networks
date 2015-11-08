
make_pairs_plots <- function(
    global_measures,
    plot_columns = c(1:7),
    measure_labels = c("degree","closeness","betweenness","eigenvector", "landau","kendall","GRC"),
    output_directory = "./Output",
    save_pdf = FALSE){

    facts=as.factor(global_measures$network_type)

    colors <- c("black","yellow",
                "red","orange",
                "green","blue",
                "violetred4","maroon1",
                "tan2")
    if(save_pdf){
        cur_dir <- getwd()
        setwd(output_directory)
        pdf(file = "Global_Measure_Pairs_Plots.pdf", width = 12, height = 10)
        pairs(global_measures[,plot_columns],
              main = "",
              pch = "*",
              col = colors[unclass(facts)],
              cex.labels=2,
              labels=measure_labels)
        dev.off()
        setwd(cur_dir)
    }else{
        pairs(global_measures[,plot_columns],
              main = "",
              pch = "*",
              col = colors[unclass(facts)],
              cex.labels=2,
              labels=measure_labels)
    }
}