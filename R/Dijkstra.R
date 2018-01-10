Dijkstra <- function(adjMatrix, sourceNode = 1)
{
  thisEnv <- environment()

  ######### ERROR CHECKING #########
  if (!is.matrix(adjMatrix)&is(adjMatrix,"graph"))
  {
    adjMatrix<- adjMatrix$getAdjMatrix()
  }
  else if (!is.matrix(adjMatrix) | dim(adjMatrix)[1] < 2 | dim(adjMatrix)[1]!=dim(adjMatrix)[2])
  {
    stop("Please provide a valid adjency matrix, that is a NxN matrix, where N > 1.")
  }

  if (!is.numeric(sourceNode) | sourceNode%%1!=0)     #If source node is in not in numeric form, check for nodeList
  {
    stop("Please provide a valid source node, as an integer corresponding to the column order in the adjency matrix.")
  }

  adjMatrix<-adjMatrix

  ########## Initialization #########
  N <- dim(adjMatrix)[1]                            #Save amount of nodes
  Q <- c(1:N)                                       #create vertex set Q and two tracking lists, prefilled
  #dist <- adjMatrix[sourceNode,]                    #Distance to node from source node. Initially set the distance to source as the row of the source node in the adjency matrix.
  dist <- rep(Inf,N)
  dist[which(Q == sourceNode)] <- 0                 #Set distance of source node to 0, forcing it to be the first node considered.
  prev <- rep(NA,N)                                 #List of previous nodes that lead to shortest path from source node. We leave the source node as NA valued as this makes logical sense


  ########## MAIN CODE ##########
  while(length(Q) > 0){                             #Loop until we have visited every node in the graph

    currentNode <- (which(dist == min(dist[Q])))     #Pick the shortest edge from the source from the list of unvisited nodes

    if(dist[currentNode[1]] < 0) stop("The matrix has element less than 0. Please use BellmanFord(matrix,source) instead.")

    Q<- Q[! Q %in% currentNode]                     #Remove Node picked from unvisited nodes

    for (i in 1:N) {
      consideredPath <- dist[currentNode] + adjMatrix[currentNode, i]

      if(consideredPath[1]<dist[i]) {
        dist[i] <- consideredPath[1]
        prev[i] <- currentNode[1]
      }

    }
  }

  me <- list(
              thisEnv = thisEnv,
              getEnv = function()
              {
                return(get("thisEnv",thisEnv))
              },
              getAdjMatrix = function(){
                return(get("adjMatrix",thisEnv))
              },
              getDist = function()
              {
                return(get("dist",thisEnv))
              },
              getPrev = function()
              {
                return(get("prev",thisEnv))
              }
            )

  assign('this',me,envir=thisEnv)
  # Set the name for the class
  class(me) <- append(class(me),"Dijkstra")
  return(me)

}
print.Dijkstra = function(sol){
  ### Calculating ###
  dist <- sol$getDist()
  prev <- sol$getPrev()
  n <- length(dist)
  paths <- rep(list(c()),n)
  for (i in 1:n) {
    nextItem <- i
    if(is.na(prev[i])) source=i
    while(!is.na(prev[nextItem])){
      paths[[i]] <- c(nextItem, paths[[i]])
      nextItem <- prev[nextItem]
    }
  }


  ### Printing ###
  print("The shortest paths from i to j are:")
  for (i in 1:n){
    paths[[i]] <- c(source, paths[[i]])
    print(paste(source, "to", i, ":",(paste(paths[[i]],collapse ="-")),sep=" "))
  }
}

plot.Dijkstra <- function(sol){
  dist <- sol$getDist()
  prev <- sol$getPrev()
  n <- length(dist)
  paths <- rep(list(c()),n)
  for (i in 1:n) {
    nextItem <- i
    if(is.na(prev[i])) source=i
    while(!is.na(prev[nextItem])){
      paths[[i]] <- c(nextItem, paths[[i]])
      nextItem <- prev[nextItem]
    }
  }

  ##Load igraph
  requireNamespace("igraph",quietly = TRUE)
  g <- igraph::make_empty_graph(directed=FALSE,n = n)
  igraph::set_vertex_attr(g,"label", value = c(1:n))

  for (i in 1:n){
    paths[[i]] <- c(source, paths[[i]])
    color<-"grey"
    g<-g + igraph::path(paths[[i]], color=color)

  }
  g <- igraph::simplify(g, remove.multiple = TRUE,remove.loops = TRUE)
  plot(g,
       edge.arrow.size=.5,
       vertex.color="orange",
       vertex.frame.color="#555555",
       vertex.label.color="black"
       )


}




