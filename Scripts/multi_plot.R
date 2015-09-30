
multi_plot <- function(data,
                       pdf_name = NULL,
                       output_pdf = F,
                       plot_columns = 1:ncol(data),
                       legend_location = "bottomleft"){

    UMASS_BLUE <- rgb(51,51,153,195,maxColorValue = 255)
    UMASS_RED <- rgb(153,0,51,195,maxColorValue = 255)
    UMASS_GREEN <- rgb(0,102,102,195,maxColorValue = 255)
    UMASS_YELLOW <- rgb(255,255,102,255,maxColorValue = 255)
    UMASS_ORANGE <- rgb(255,204,51,195,maxColorValue = 255)

    colors <- c(UMASS_BLUE,UMASS_RED, UMASS_GREEN, UMASS_ORANGE, UMASS_YELLOW)
    plot_colors <- NULL
    index <- 1
    for(i in 1:length(plot_columns)){
        plot_colors <- c(plot_colors, colors[index])
        index <- index + 1
        if(index == 6){
            index <- 1
        }
    }

    if(output_pdf){
        pdf(file = paste("./Output/",pdf_name,".pdf",sep = ""),height=5, width=8, family="Times", pointsize=13.5)
        par(las=2,mar=c(6,5,.5,.5),cex.lab=.5,xpd=TRUE)

        plot(data[,plot_columns[1]],ylim=c(min(data[,plot_columns],0),max(data[,plot_columns],1)),lwd=-1,col = "white",type="l",xaxt= "n",xlab="Network",ylab="Score", main = "",cex.axis = 0.5)
        abline(v=1:nrow(data),lwd=0.5,col="grey80",lty=3)
        col_names <- NULL
        col_colors <- NULL
        col_shapes <- NULL
        for(i in 1:length(plot_columns)){
            points(data[,plot_columns[i]] ~ jitter(1:29,0.3),
                   pch=14+i,
                   col=plot_colors[i],
                   bg=plot_colors[i])
            col_names <- c(col_names,colnames(data)[plot_columns[i]])
            col_colors <- c(col_colors,plot_colors[i])
            col_shapes <- c(col_shapes,14+i)
        }
        axis(1, col='black', at=1:nrow(data), labels=rownames(data),lwd= 1, cex.axis = 0.5)

        legend("bottomleft",
               legend=col_names,
               pch=col_shapes ,
               col = col_colors,
               title="Measure",
               cex = 0.5)
        dev.off()
    }else{
        par(las=2,mar=c(6,5,.5,.5),cex.lab=.5,xpd=TRUE)

        plot(data[,plot_columns[1]],ylim=c(min(data,0),max(data,1)),lwd=0,type="l",xaxt= "n",xlab="Network",ylab="Score", main = "",cex.axis = 0.5)
        abline(v=1:nrow(data),lwd=0.5,col="grey80",lty=3)
        col_names <- NULL
        col_colors <- NULL
        col_shapes <- NULL
        for(i in 1:length(plot_columns)){
            points(data[,plot_columns[i]],pch=14+i,col=plot_colors[i],bg=plot_colors[i])
            col_names <- c(col_names,colnames(data)[plot_columns[i]])
            col_colors <- c(col_colors,plot_colors[i])
            col_shapes <- c(col_shapes,14+i)
        }
        axis(1, col='black', at=1:nrow(data), labels=rownames(data),lwd= 1, cex.axis = 0.5)

        legend(legend_location,
               legend=col_names,
               pch=col_shapes ,
               col = col_colors,
               title="Measure",
               cex = 0.5)
        dev.off()
    }
}
