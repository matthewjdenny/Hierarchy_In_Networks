# function to calculate analytical metrics of hierarchy on a sociomatrix

calculate_analytical_hierarhy_measures <- function(sociomatrix,
                                                   mode = "directed"){
    #statistics using igraph
    require(igraph)
    sociomatrix <- graph.adjacency(sociomatrix, mode = mode)

    #calculate global scores
    global <- list()
    global$degree_centralization <- centralization.degree (sociomatrix, mode = "all")$centralization
    global$closeness_centralization <- centralization.closeness (sociomatrix, mode = "all")$centralization
    global$betweenness_centralization <-  centralization.betweenness (sociomatrix, directed = TRUE)$centralization
    global$eigenvector_centralization <- centralization.evcent (sociomatrix, directed = TRUE)$centralization

    #calculate local scores
    local <- list()
    local$degree_centrality$score <- centralization.degree (sociomatrix, mode = "all")$res
    local$degree_centrality$rank <- order(local$degree_centrality$score, decreasing = T)
    local$closeness_centrality$score <- centralization.closeness (sociomatrix, mode = "all")$res
    local$closeness_centrality$rank <- order(local$closeness_centrality$score, decreasing = T)
    local$betweenness_centrality$score <-  centralization.betweenness (sociomatrix, directed = TRUE)$res
    local$betweenness_centrality$rank <- order(local$betweenness_centrality$score, decreasing = T)
    local$eigenvector_centrality$score <- centralization.evcent (sociomatrix, directed = TRUE)$vector
    local$eigenvector_centrality$rank <- order(local$eigenvector_centrality$score, decreasing = T)

    # return a list object with a $global and $local sublist, each of which contains the output from all of the different measures which are appropriately named. In the $local sublist, we provide the $rank and $score for each node in the network.
    return_list <- list(global = global, local = local)
    return(return_list)
}

#######################################################################################
#Landau's h-outputs global stat only works for directed graphs

landau <- function(matrix,directed=TRUE){
  
  if(directed==FALSE){
    print("error: this measure may only be used with directed networks")
    break;
  }
  
  N=dim(matrix)[1]
  S=apply(matrix,1,sum)
  sum(S-((N-1)/2))
  h=12/(N^3-N) * sum(S-((N-1)/2))
  results=list(global=h,local=rep(NA,N))
  return(results)
}

#######################################################################################
#Kendall's K-outputs global stat only works for directed graphs
 
kendall <- function(matrix,directed=TRUE){
  
  if(directed==FALSE){
    print("error: this measure may only be used with directed networks")
    break;
  }
  
  N=dim(matrix)[1]
  S=apply(matrix,1,sum)
  d=(N*(N-1)*(2*N-1))/12-(0.5*sum(S^2))
  
  if((N%%2)==0){
    #even
    d_max=(1/24)*(N^3-4*N)
  } else{ 
    #odd
    d_max=(1/24)*(N^3-N) 
      }
  
  K=1-(d/d_max)
  results=list(global=K,local=rep(NA,N))
  return(results)
}

#######################################################################################
#m reach degree-requires the keyplayer package in R; outputs local stats

m_degree <- function(matrix,directed=TRUE){
  require("keyplayer")
  l=mreach.degree(matrix,cmode="outdegree")
  results=list(global=NA,local=l)
  return(results)
}
#######################################################################################
#m reach closeness-requires the keyplayer package in R; outputs local stats

m_close <- function(matrix,directed=TRUE){
  require("keyplayer")
  l=mreach.closeness(matrix,cmode="outdegree")
  results=list(global=NA,local=l)
  return(results)
}
#######################################################################################
#GRC-requires the keyplayer package in R; outputs global and local stats

GRC <- function(matrix,directed=TRUE){
  require("keyplayer")
  N=dim(matrix)[1]
  
  if(directed==TRUE){
    C=mreach.degree(matrix,cmode="outdegree")
  }
  
  if(directed==FALSE){
    C=mreach.degree(matrix,cmode="all")[,3]
  }
  
  C_max=max(C)
  GRC=(sum(C_max-C))/(N-1)
  results=list(global=GRC,local=C)
  return(results)
}

#######################################################################################
#Rooted Depth-requires the igraph package in R; outputs global and local stats

D_root <- function(matrix,directed=TRUE){
  require(igraph)
  
  if(directed==FALSE){
    print("error: this measure may only be used with directed networks")
    break;
  }
  
  roots=which(apply(matrix,1,sum)==0)
  if(length(roots)==0){
    print("error:There are no roots in your network")
    break;
  }
  
  N=dim(matrix)[1]
  graph=graph_from_adjacency_matrix(matrix,mode="directed") 
  paths=as.matrix(shortest.paths(graph,v=roots))
  l=apply(paths,2,mean)
  D_root=mean(as.vector(paths))
  results=list(global=D_root,local=l)
  return(results)
}