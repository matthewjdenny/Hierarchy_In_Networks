D=length(Network_Data)
D
save_G=matrix(nrow=D,ncol=10)  
save_LR=list()
save_L=list()
for (i in 1:D){
  save_G[i,1]=landau(Network_Data[[i]]$sociomatrix)$global
  save_G[i,2]=kendall(Network_Data[[i]]$sociomatrix)$global
  save_G[i,3]=m_degree(Network_Data[[i]]$sociomatrix)$global
  save_G[i,4]=m_close(Network_Data[[i]]$sociomatrix)$global
  save_G[i,5]=GRC(Network_Data[[i]]$sociomatrix)$global
  save_G[i,6]=D_root(Network_Data[[i]]$sociomatrix)$global
  save_G[i,7:10]=as.numeric(calculate_analytical_hierarhy_measures(
    Network_Data[[i]]$sociomatrix)$global)
}

colnames(save_G)=c("landau","kendall","m_degree","m_close","GRC","D_root","degree",
                   "closeness","betweenness","eigenvector")
pairs(save_G[,-c(3,4,6)])

for (i in 1:D){
  s=matrix(nrow=dim(Network_Data[[i]]$sociomatrix)[1],ncol=10)
  s[,1]=landau(Network_Data[[i]]$sociomatrix)$local
  s[,2]=kendall(Network_Data[[i]]$sociomatrix)$local
  s[,3]=rank(as.numeric(m_degree(Network_Data[[i]]$sociomatrix)$local))
  s[,4]=rank(as.numeric(m_close(Network_Data[[i]]$sociomatrix)$local))
  s[,5]=rank(as.numeric(GRC(Network_Data[[i]]$sociomatrix)$local))
  s[,6]=rank(as.numeric(D_root(Network_Data[[i]]$sociomatrix)$local))
  s[,7]=as.numeric(calculate_analytical_hierarhy_measures(
    Network_Data[[i]]$sociomatrix)$local$degree_centrality$rank)
  s[,8]=as.numeric(calculate_analytical_hierarhy_measures(
    Network_Data[[i]]$sociomatrix)$local$closeness_centrality$rank)
  s[,9]=as.numeric(calculate_analytical_hierarhy_measures(
    Network_Data[[i]]$sociomatrix)$local$betweenness_centrality$rank)
  s[,10]=as.numeric(calculate_analytical_hierarhy_measures(
    Network_Data[[i]]$sociomatrix)$local$eigenvector_centrality$rank)
  save_LR[[i]]=s
}

for (i in 1:D){
  s=matrix(nrow=dim(Network_Data[[i]]$sociomatrix)[1],ncol=10)
  s[,1]=landau(Network_Data[[i]]$sociomatrix)$local
  s[,2]=kendall(Network_Data[[i]]$sociomatrix)$local
  s[,3]=as.numeric(m_degree(Network_Data[[i]]$sociomatrix)$local)
  s[,4]=as.numeric(m_close(Network_Data[[i]]$sociomatrix)$local)
  s[,5]=as.numeric(GRC(Network_Data[[i]]$sociomatrix)$local)
  s[,6]=as.numeric(D_root(Network_Data[[i]]$sociomatrix)$local)
  s[,7]=as.numeric(calculate_analytical_hierarhy_measures(
    Network_Data[[i]]$sociomatrix)$local$degree_centrality$score)
  s[,8]=as.numeric(calculate_analytical_hierarhy_measures(
    Network_Data[[i]]$sociomatrix)$local$closeness_centrality$score)
  s[,9]=as.numeric(calculate_analytical_hierarhy_measures(
    Network_Data[[i]]$sociomatrix)$local$betweenness_centrality$score)
  s[,10]=as.numeric(calculate_analytical_hierarhy_measures(
    Network_Data[[i]]$sociomatrix)$local$eigenvector_centrality$score)
  save_L[[i]]=s
}

pairs(save_L[[6]][,-c(1,2)],cex.labels=2,labels=c("landau","kendall",
                                                  "m_degree","m_close","GRC","D_root","degree","closeness","betweenness",
                                                  "eigenvector"))

