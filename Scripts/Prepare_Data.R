#combine data
load("./Data/Manager_Email_Networks.Rdata")
load("./Data/Influence_Network_Data_97_108.Rdata")
for(i in 1:length(Influence_Network_Data_97_108)){
    Influence_Network_Data_97_108$mode = "directed"
    Influence_Network_Data_97_108$weighted = TRUE
    Influence_Network_Data_97_108$type = "influence"

}
for(i in 1:length(Manager_Email_Networks)){
    Manager_Email_Networks[[i]]$weighted = TRUE
    Manager_Email_Networks[[i]]$type = "communication"
    data <- data.frame(Manager_Email_Networks[[i]]$department,stringsAsFactors = F)
    index <- which(tolower(data[,1]) == "manager")
    Manager <- rep(0,nrow(data))
    Manager[index] <- 1
    data <- cbind(data,Manager)
    colnames(data)[1] <- "Department"
    Manager_Email_Networks[[i]]$node_level_data = data
    Manager_Email_Networks[[i]]$department = NULL
}

for(i in 18:length(Network_Data)){
    Network_Data[[i]]$weighted = TRUE
    Network_Data[[i]]$type = "influence"
    Network_Data[[i]]$mode = "directed"
    Network_Data[[i]]$sociomatrix <- Network_Data[[i]]$network
    Network_Data[[i]]$network <- NULL
}

for(i in 1:length(Network_Data)){
    print(i)
    if(Network_Data[[i]]$type == "communication"){
        Network_Data[[i]]$leadership <- as.numeric(Network_Data[[i]]$node_level_data$Manager)
    }
    if(Network_Data[[i]]$type == "influence"){
        temp <- as.numeric(Network_Data[[i]]$node_level_data$Leader)
        repl <- which(is.na(temp))
        if(length(repl) > 0){
            temp[repl] <- 0
        }
        Network_Data[[i]]$leadership <- temp
    }
}

#hack to fix network list
nms <- names(Network_Data)[18:29]
for(i in 18:length(Network_Data)){
    Network_Data[[i]] <- list(sociomatrix = Network_Data[[i]]$sociomatrix,
                              mode = Network_Data[[i]]$mode,
                              weighted = Network_Data[[i]]$weighted,
                              type = Network_Data[[i]]$type,
                              node_level_data = Network_Data[[i]]$node_level_data,
                              leadership = Network_Data[[i]]$leadership)
#     Network_Data[[i]]$weighted = TRUE
#     Network_Data[[i]]$type = "influence"
#     Network_Data[[i]]$mode = "directed"
#     Network_Data[[i]]$sociomatrix <- Network_Data[[i]]$network
#     Network_Data[[i]]$network <- NULL
}
Network_Data <- append(Manager_Email_Networks,Influence_Network_Data_97_108)
save(Network_Data,file = "./Data/Network_Data.Rdata")