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

# load in cosponsorship data and transform to sociomatrices
load("~/Dropbox/Research/Senate_Influence/Data/Master_List.Rdata")
Cosponsorship_Data <- vector(mode = "list", length = 18)
for(i in 1:18){
    print(i)
    index <- 10*(i-1) + 1
    temp <- Master_List[[index]]
    sociomatrix <- matrix(0,nrow(temp),nrow(temp))
    for(j in 1:ncol(temp)){
        sponsor <- which(temp[,j] == 1)
        if(length(sponsor) == 1){
            cosponsors <- which(temp[,j] == 2)
            if(length(cosponsors) > 0){
                sociomatrix[cosponsors,sponsor] <- sociomatrix[cosponsors,sponsor] + 1
            }
        }
    }
    Cosponsorship_Data[[i]] <- list(sociomatrix = sociomatrix,
                                    mode = "directed",
                                    weighted = TRUE,
                                    type = "cosponsorship")
}
nms <- paste0("Congress_",93:110,sep = "")
names(Cosponsorship_Data) <- nms
load("./Data/Network_Data.Rdata")
# now we are going to get rid of influence networks
Network_Data <- append(Network_Data[1:17],Cosponsorship_Data)
save(Network_Data,file = "./Data/Network_Data.Rdata")
