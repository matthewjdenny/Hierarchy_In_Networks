#load in functions
source("./Scripts/calculate_analytical_hierarchy_measures.R")
source("./Scripts/score_leadership_rank.R")
source("./Scripts/multi_plot.R")

#load data
load("./Data/Network_Data.Rdata")
load("./Data/Influence_Network_Data_97_108.Rdata")
load("./Data/Manager_Email_Networks.Rdata")

#creating list with only sociomatricies
d1=length(Network_Data);d2=length(Influence_Network_Data_97_108);d3=length(Manager_Email_Networks)
D=d1+d2+d3

socio_all=list()

for(i in 1:d1){socio_all[[i]]=Network_Data[[i]]$sociomatrix}
for(i in (d1+1):(d1+d2)){socio_all[[i]]=Influence_Network_Data_97_108[[i-d1]]$network}
for(i in (d1+d2+1):D){socio_all[[i]]=Manager_Email_Networks[[i-(d1+d2)]]$sociomatrix}

#Calculating global measures for all data
save_G=matrix(nrow=D,ncol=10)
save_LR=list()
save_L=list()
for (i in 1:D){
  save_G[i,1]=landau(socio_all[[i]])$global
  save_G[i,2]=kendall(socio_all[[i]])$global
  save_G[i,3]=m_degree(socio_all[[i]])$global
  save_G[i,4]=m_close(socio_all[[i]])$global
  save_G[i,5]=GRC(socio_all[[i]])$global
  save_G[i,6]=D_root(socio_all[[i]])$global
  save_G[i,7:10]=as.numeric(calculate_analytical_hierarhy_measures(
      socio_all[[i]])$global)
}

colnames(save_G)=c("landau","kendall","m_degree","m_close","GRC","D_root","degree",
                   "closeness","betweenness","eigenvector")
pairs(save_G[,-c(3,4,6)])


#Calculating both ranls and actual scores for the all local measures
for (i in 1:D){
  s=matrix(nrow=dim(socio_all[[i]])[1],ncol=10)
  s[,1]=rep(-1,dim(socio_all[[i]])[1])
  s[,2]=rep(-1,dim(socio_all[[i]])[1])
  s[,3]=rank(as.numeric(m_degree(socio_all[[i]])$local))
  s[,4]=rank(as.numeric(m_close(socio_all[[i]])$local))
  s[,5]=rank(as.numeric(GRC(socio_all[[i]])$local))
  s[,6]=rep(-1,dim(socio_all[[i]])[1])
  s[,7]=as.numeric(calculate_analytical_hierarhy_measures(
      socio_all[[i]])$local$degree_centrality$rank)
  s[,8]=as.numeric(calculate_analytical_hierarhy_measures(
      socio_all[[i]])$local$closeness_centrality$rank)
  s[,9]=as.numeric(calculate_analytical_hierarhy_measures(
      socio_all[[i]])$local$betweenness_centrality$rank)
  s[,10]=as.numeric(calculate_analytical_hierarhy_measures(
      socio_all[[i]])$local$eigenvector_centrality$rank)
  save_LR[[i]]=s
}

count=rep(0,29)
for(i in 1:29){
    leader=which(Network_Data[[i]]$leader==1)
    for (j in 1:10){
        if (which(save_LR[[i]][,j]==1)==integer(0)) log=leader+1
        if(which(save_LR[[i]][,j]!=integer(0))  log=which(save_LR[[i]][,j]
        if (leader==log) count[1]=count[1]+1
    }
}


for (i in 1:D){
  s=matrix(nrow=dim(socio_all[[i]])[1],ncol=10)
  s[,1]=landau(socio_all[[i]])$local
  s[,2]=kendall(socio_all[[i]])$local
  s[,3]=as.numeric(m_degree(socio_all[[i]])$local)
  s[,4]=as.numeric(m_close(socio_all[[i]])$local)
  s[,5]=as.numeric(GRC(socio_all[[i]])$local)
  s[,6]=as.numeric(D_root(socio_all[[i]])$local)
  s[,7]=as.numeric(calculate_analytical_hierarhy_measures(
      socio_all[[i]])$local$degree_centrality$score)
  s[,8]=as.numeric(calculate_analytical_hierarhy_measures(
      socio_all[[i]])$local$closeness_centrality$score)
  s[,9]=as.numeric(calculate_analytical_hierarhy_measures(
      socio_all[[i]])$local$betweenness_centrality$score)
  s[,10]=as.numeric(calculate_analytical_hierarhy_measures(
      socio_all[[i]])$local$eigenvector_centrality$score)
  save_L[[i]]=s
}

pairs(save_L[[6]][,-c(1,2)],cex.labels=2,labels=c("landau","kendall",
                                                  "m_degree","m_close","GRC","D_root","degree","closeness","betweenness",
                                                  "eigenvector"))




load("./Data/global_hierarchy_measures.Rdata")

hold=matrix(nrow=length(Network_Data),ncol=2)
for(i in 1:length(Network_Data)){
  hold[i,]=c(names(Network_Data)[i],Network_Data[[i]]$type)
}
facts=as.factor(hold[-c(100,52,54,128,53,78),2])

pairs(global_measures[1:7], main = "Global Hierarchy Measures",
      pch = 21, bg = c("black","yellow","red","orange","green","blue",
      "violetred4","maroon1","tan2")[unclass(facts)],cex.labels=2,labels=c("landau",
      "kendall","m_degree","m_close","GRC","D_root","degree","closeness","betweenness",
          "eigenvector"))


Network_Data[-c(100,52:54,128,78)]




data_list <- generate_hierarchy_dataset(Network_Data[-c(100,52:54,128,78)])

global_measures <- data_list$global_measure_dataframe
#deal with weird networks
global_measures <- global_measures[-100,]
global_measures <- global_measures[-which(row.names(global_measures) == "drugnet"),]
global_measures <- global_measures[-which(row.names(global_measures) == "Terro_4275"),]
global_measures <- global_measures[-which(row.names(global_measures) == "drug_net_61"),]
global_measures <- global_measures[-which(row.names(global_measures) == "Koster_data8"),]
global_measures <- global_measures[-which(row.names(global_measures) == "CITIES"),]

save(global_measures, file = "./Data/global_hierarchy_measures.Rdata")

multi_plot(data = global_measures,
           pdf_name = "Global_Measures",
           output_pdf = F,
           c(2:5,7))

multi_plot(data = data_list$leadership_ranking_scores,
           pdf_name = "Measure_Scores",
           output_pdf = F)

# calculate some descriptive statistics
descriptive_stats <- calculate_descriptive_statistics(Network_Data[-c(100,52:54,128,78)])
network_descriptive_statistics <- descriptive_stats[[1]]
type_descriptive_statistics <- descriptive_stats[[2]]


