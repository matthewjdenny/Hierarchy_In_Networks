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
Network_Data <- append(Manager_Email_Networks,Influence_Network_Data_97_108)
save(Network_Data,file = "./Data/Network_Data.Rdata")