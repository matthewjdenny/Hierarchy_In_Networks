# function to score measures if $leadership is provided
score_leadership_rank <- function(Network_Data,
                                  Measures){
    leadership_ranks <- data.frame(matrix(NA,ncol = length(Measures[[1]]$local), nrow = length(Measures)))

    for(i in 1:nrow(leadership_ranks)){
        for(j in 1:ncol(leadership_ranks)){
            if(!is.null(Network_Data[[i]]$leadership)){
                indexes <- which(Network_Data[[i]]$leadership == 1)
                score <- mean(1:length(indexes)) - mean(Measures[[i]]$local[[j]]$rank[indexes])
                leadership_ranks[i,j] <- (score/length(Network_Data[[i]]$leadership)) +1
            }
        }
    }
    colnames(leadership_ranks) <- names(Measures[[1]]$local)
    rownames(leadership_ranks) <- names(Network_Data)

    #remove NA rows
    remove <- rep(0,nrow(leadership_ranks))
    for(i in 1:nrow(leadership_ranks)){
        naInRow <- which(is.na(leadership_ranks[i,]))
        if(length(naInRow) > 0){
            remove[i] <- 1
        }
    }
    leadership_ranks <- leadership_ranks[-which(remove == 1),]
    return(leadership_ranks)
}