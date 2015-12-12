#utility functions to aid with plotting and manipulating data.

remove_rows <- function(dframe,
                        remove,
                        list = FALSE){
    if(list){
        for(i in 1:length(remove)){
            dframe <- dframe[-which(names(dframe) == remove[i])]
        }
    }else{
        for(i in 1:length(remove)){
            dframe <- dframe[-which(row.names(dframe) == remove[i]),]
        }
    }
    return(dframe)
}

# string <- "BA_n-100_p-0.5_s-123"
collapse_over_size <- function(dataframe){
    remove_n <- function(string){
        paste0(stringr::str_split(string,"_")[[1]][c(1,3)],collapse = " ")
    }
    dataframe$network_type <- sapply(dataframe$network_type, remove_n)
    return(dataframe)
}

collapse_over_parameter <- function(dataframe){
    remove_n <- function(string){
        paste0(stringr::str_split(string,"_")[[1]][c(1,2)],collapse = " ")
    }
    dataframe$network_type <- sapply(dataframe$network_type, remove_n)
    return(dataframe)
}

# takes a data-frame that has already been collapsed over size, type or otherwise
average_over_type <- function(dataframe){
    ret <- NULL
    un <- unique(dataframe$network_type)

    numeric_cols <- NULL
    for(i in 1:ncol(dataframe)){
        numeric_cols <- c(numeric_cols,class(dataframe[,i]))
    }
    character_cols <- which(numeric_cols != "numeric")[1]
    numeric_cols <- which(numeric_cols == "numeric")
    for(i in 1:length(un)){
        ret <- data.frame(rbind(ret, c(colMeans(dataframe[which(dataframe$network_type == un[i]),numeric_cols], na.rm= TRUE),dataframe[which(dataframe$network_type == un[i])[1],character_cols])), stringsAsFactors = F)
    }
    #now make sure columns are numeric
    for(j in 1:length(numeric_cols)){
        ret[,numeric_cols[j]] <- as.numeric(ret[,numeric_cols[j]])
    }

    colnames(ret)[character_cols] <- "network_type"
    collapser <- function(str){
        paste0(stringr::str_split(str," ")[[1]],
               collapse = "_")
    }

    row.names(ret) <- sapply(ret$network_type, collapser)
    return(ret)
}

replac <- function(str){
    if(nchar(str) > 12){
        stringr::str_replace_all(str,"_","\n")
    }else{
        str
    }
}

cor.mtest <- function(mat, conf.level = 0.95) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    diag(lowCI.mat) <- diag(uppCI.mat) <- 1
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
            lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
            uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
        }
    }
    return(list(p.mat, lowCI.mat, uppCI.mat))
}

#normalizes each row of a data frame
normalize_values <- function(data,
                             columns = c(1:10)){
    for(i in columns){
        data[,i] <- data[,i]/max(abs(data[,i]))
    }
    return(data)
}