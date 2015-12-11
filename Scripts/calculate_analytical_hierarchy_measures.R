# function to calculate analytical metrics of hierarchy on a sociomatrix

calculate_analytical_hierarhy_measures <- function(sociomatrix,
                                                   mode = "directed"){
    #statistics using igraph
    require(igraph)
    adjacency <- sociomatrix
    sociomatrix <- graph.adjacency(sociomatrix, mode = mode, weighted = TRUE)

    isDirected <- FALSE
    if(mode == "directed"){
        isDirected <- TRUE
    }

    dominance <- matrix(0,nrow(adjacency),nrow(adjacency))
    dominance_tri <- matrix(0,nrow(adjacency),nrow(adjacency))
    for(i in 1:nrow(adjacency)){
        for(j in 1:nrow(adjacency)){
            if(adjacency[i,j] > adjacency[i,j]){
                dominance[i,j] <- 1
                dominance_tri[i,j] <- 1
            }
            if(adjacency[i,j] == adjacency[i,j]){
                dominance[i,j] <- 0.5
                dominance_tri[i,j] <- 1
            }
        }
    }

    #calculate global scores
    global <- list()
    global$degree_centralization <- centralization.degree (sociomatrix, mode = "all")$centralization
    global$closeness_centralization <- centralization.closeness (sociomatrix, mode = "all")$centralization
    global$betweenness_centralization <-  centralization.betweenness (sociomatrix, directed = isDirected)$centralization
    global$eigenvector_centralization <- centralization.evcent (sociomatrix, directed = isDirected)$centralization
    if(isDirected){
        global$krackhardt <- as.numeric(sna::hierarchy(adjacency,"krackhardt"))
        global$triangle_transitivity <- as.numeric(dominance_tri)
        global$landau <- landau(dominance)$global
        global$kendall <- kendall(dominance)$global
        global$GRC <- GRC(adjacency)$global
        global$D_root <- D_root(adjacency)$global
        global$m_degree <- m_degree(adjacency)$global
        global$m_close <- m_close(adjacency)$global
    }

    #calculate local scores
    local <- list()
    local$degree_centrality$score <- centralization.degree (sociomatrix, mode = "all")$res
    local$degree_centrality$rank <- order(local$degree_centrality$score, decreasing = T)
    local$closeness_centrality$score <- centralization.closeness (sociomatrix, mode = "all")$res
    local$closeness_centrality$rank <- order(local$closeness_centrality$score, decreasing = T)
    local$betweenness_centrality$score <-  centralization.betweenness (sociomatrix, directed = isDirected)$res
    local$betweenness_centrality$rank <- order(local$betweenness_centrality$score, decreasing = T)
    local$eigenvector_centrality$score <- centralization.evcent (sociomatrix, directed = isDirected)$vector
    local$eigenvector_centrality$rank <- order(local$eigenvector_centrality$score, decreasing = T)

    if(isDirected){
        local$m_degree$score <- m_degree(adjacency)$local
        local$m_degree$rank <- order(local$m_degree$score, decreasing = T)
        local$m_close$score <- m_close(adjacency)$local
        local$m_close$rank <- order(local$m_close$score, decreasing = T)
        local$GRC$score <- GRC(adjacency)$local
        local$GRC$rank <- order(local$GRC$score, decreasing = T)
        local$D_root$score <- D_root(adjacency)$local
        if(!is.na(local$D_root$score[1])){
            local$D_root$rank <- order(local$D_root$score, decreasing = T)
        }else{
            local$D_root$rank <- NA
        }
    }

    # return a list object with a $global and $local sublist, each of which contains the output from all of the different measures which are appropriately named. In the $local sublist, we provide the $rank and $score for each node in the network.
    return_list <- list(global = global, local = local)
    return(return_list)
}

#######################################################################################
#Landau's h-outputs global stat only works for directed graphs

landau <- function(matrix,
                   directed=TRUE){

  if(directed==FALSE){
    print("error: this measure may only be used with directed networks")
    break;
  }

  N=nrow(matrix)
  S=apply(matrix,1,sum)
  h=12/(N^3-N) * sum((S-((N-1)/2))^2)
  results=list(global=h,local=rep(NA,N))
  return(results)
}

# from: A social network perspective on measurements of dominance hierarchies
triangle_transitivity <- function(matrix){
    m=as.matrix(matrix)
    g=network::network(m,directed=TRUE)
    tri=sna::triad.census(g) #The full triad census as an 16-element vector
    w=as.vector(c(0,0,0,0,0,0,0,0,1,0,0,1,1,0.5,0.75,0.75)) # The weighting vector for transitivity
    N.triangle=sum(tri*as.vector(c(0,0,0,0,0,0,0,0,1,1,0,1,1,1,1,1))) #Count sum the number of triangles
    Pt=sum(tri*w)/N.triangle
    t.tri=4*(Pt-0.75)
    return(t.tri)
}

#######################################################################################
#Kendall's K-outputs global stat only works for directed graphs

kendall <- function(matrix,
                    directed=TRUE){

  if(directed==FALSE){
    print("error: this measure may only be used with directed networks")
    break;
  }

  N=nrow(matrix)
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
  g <- ineq::ineq(l,type="Gini")
  results=list(global=g,local=l)
  return(results)
}
#######################################################################################
#m reach closeness-requires the keyplayer package in R; outputs local stats

m_close <- function(matrix,directed=TRUE){
  require("keyplayer")
  l=mreach.closeness(matrix,cmode="outdegree")
  g <- ineq::ineq(l,type="Gini")
  results=list(global=g,local=l)
  return(results)
}
#######################################################################################
#GRC-requires the keyplayer package in R; outputs global and local stats

GRC <- function(matrix,directed=TRUE){
  require("keyplayer")
  N=dim(matrix)[1]

  #if the matrix is weighted -- hand rolled
  if(length(unique(matrix)) > 2){
      d <- data.matrix(matrix, rownames.force = NA)
      rownames(d) <- c(colnames(d))
      distances = sna::geodist(d, ignore.eval = F)$gdist
      colnames(distances) <- c(colnames(d))
      rownames(distances) <- c(colnames(d))
      diag(distances) = Inf
      sociomatrix <- graph.adjacency(matrix, mode = "directed", weighted = TRUE)
      weights <- shortest.paths(sociomatrix)
      #weights = weights/max(weights[which(is.finite(weights))])
      weights[which(!is.finite(weights))] <- 0

      #now calculate for each node
      C <- rep(0,N)
      for(i in 1:N){
          C[i] <- (1/(N-1))*sum((weights[i,]/distances[i,]))
      }
  }else{
      if(directed==TRUE){
          C=mreach.degree(matrix,cmode="outdegree")
      }

      if(directed==FALSE){
          C=mreach.degree(matrix,cmode="all")[,3]
      }
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
    print("There are no roots in your network. Cannot calculate measure.")
    results=list(global=NA,local=NA)
    return(results)
  }else{
      N=dim(matrix)[1]
      graph=graph_from_adjacency_matrix(matrix,mode="directed")
      paths=as.matrix(shortest.paths(graph,v=roots))
      l=apply(paths,2,mean)
      global_val =mean(as.vector(paths))
      if(is.finite(global_val)){
          results=list(global=global_val,local=l)
      }else{
          results=list(global=NA,local=l)
      }
      return(results)
  }
}