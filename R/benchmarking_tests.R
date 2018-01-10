requireNamespace("Rcpp",quietly = TRUE)
requireNamespace("microbenchmark",quietly = TRUE)

dijkstraCPPMain <- function(adjMatrix,source=1)
{
  N <- dim(adjMatrix)[1]
  result<- dijkstraCPP(adjMatrix, N, (source-1))
  dist <- unlist(result[1], use.names = FALSE)
  prev <- unlist(result[2], use.names = FALSE)
  return(dist)
}

dijkstraRMain <- function(adjMatrix,source=1)
{
  ########## Initialization #########
  N <- dim(adjMatrix)[1]                            #Save amount of nodes
  Q <- c(1:N)                                       #create vertex set Q and two tracking lists, prefilled
  #dist <- adjMatrix[sourceNode,]                    #Distance to node from source node. Initially set the distance to source as the row of the source node in the adjency matrix.
  dist <- rep(Inf,N)
  dist[which(Q == source)] <- 0                 #Set distance of source node to 0, forcing it to be the first node considered.
  prev <- rep(NA,N)                                 #List of previous nodes that lead to shortest path from source node. We leave the source node as NA valued as this makes logical sense


  ########## MAIN CODE ##########
  while(length(Q) > 0){                             #Loop until we have visited every node in the graph

    currentNode <- (which(dist == min(dist[Q])))     #Pick the shortest edge from the source from the list of unvisited nodes

    Q<- Q[! Q %in% currentNode]                     #Remove Node picked from unvisited nodes

    for (i in 1:N) {
      consideredPath <- dist[currentNode] + adjMatrix[currentNode, i]

      if(consideredPath[1]<dist[i]) {
        dist[i] <- consideredPath[1]
        prev[i] <- currentNode[1]
      }

    }
  }
  return(dist)
}

test <- function(times, n) {
  Rcpp::sourceCpp("dijkstraCPP.cpp")
  A <- graph(n)
  A$randFill(min=5,max=100)
  A <- A$getAdjMatrix()
  res <- microbenchmark::microbenchmark(dijkstraRMain(A),dijkstraCPPMain(A),times=times,control=list(warmup=100))
  return(res)
}
