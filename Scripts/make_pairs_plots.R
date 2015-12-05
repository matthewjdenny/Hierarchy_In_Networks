
make_pairs_plots <- function(
    global_measures,
    plot_columns = c(1:7,9,10),
    measure_labels = c("degree","closeness","betweenness","eigenvector", "landau","kendall","GRC","m_degree","m_close"),
    output_directory = "./Output",
    save_pdf = FALSE){

    facts=as.factor(global_measures$network_type)

    colors <- c("black","yellow",
                "red","orange",
                "green","blue",
                "violetred4","maroon1",
                "tan2","brown","darkgoldenrod")
    if(save_pdf){
        cur_dir <- getwd()
        setwd(output_directory)
        pdf(file = "Global_Measure_Pairs_Plots.pdf", width = 15, height = 12)
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