BellmanFord <- function(adjMatrix, sourceNode = 1)
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
  adjMatrix <- adjMatrix

  ########## Initialization #########
  N <- dim(adjMatrix)[1]                            #Amount of nodes
  dist<-matrix(Inf,nrow=N,ncol=N)                   #Create Matrix to track shortest distance to node through iterations
  prev<-matrix(nrow=N,ncol=N)                       #Create Matrix to track previous node through iterations
  dist[1,sourceNode] <- 0

  ########## MAIN CODE ##########
  for(k in 2:N) {
    dist[k,] <- dist[k-1,]
    prev[k,] <- prev[k-1,]

    for(i in 1:N) {
      for(j in 1:N) {
        if((dist[k-1,j] + adjMatrix[j,i]) < dist[k,i]){

          dist[k,i] <- dist[k-1,j] + adjMatrix[j,i]
          prev[k,i] <- j

        }
      }
    }

  }

  ########## Check for negative cycle in graph ##########
  negCycleExists <- FALSE
  for(i in 1:N) {
    if(dist[N-1,i] > dist[N,i]) {
      negCycleExists <- TRUE
    }
  }
  nodeInNegCycle <- NA
  negCycle <- NA
  if(negCycleExists) {
    cycleFound = FALSE
    negCycle <- c(sourceNode)
    tempNode <- sourceNode
    while (!cycleFound){
      if(prev[N,tempNode]==sourceNode){
        cycleFound = TRUE
      }
      negCycle <- c(negCycle,prev[N,tempNode])
      tempNode <- prev[N,tempNode]
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
    getNegCycle = function(){
      return(get("negCycle",thisEnv))
    },
    getDist = function()
    {
      return(get("dist",thisEnv))
    },
    getPrev = function()
    {
      return(get("prev",thisEnv))
    },
    negCycleExists = function()
    {
      return(get("negCycleExists",thisEnv))
    }
  )

  assign('this',me,envir=thisEnv)
  # Set the name for the class
  class(me) <- append(class(me),"BellmanFord")
  return(me)
}
print.BellmanFord = function(sol){
  ### Printing ###
  if(sol$negCycleExists()) {
    print(paste("A negative cycle was found in the graph reachable from the source, it is: ",
                (paste(rev(sol$getNegCycle()),collapse ="-")), sep=" "))
  } else {
    ### Calculating ###
    dist <- sol$getDist()
    prev <- sol$getPrev()
    n <- dim(dist)[1]
    dist <- dist[n,]
    prev <- prev[n,]
    paths <- rep(list(c()),n)
    for (i in 1:n) {
      nextItem <- i
      if(is.na(prev[i])) source=i
      while(!is.na(prev[nextItem])){
        paths[[i]] <- c(nextItem, paths[[i]])
        nextItem <- prev[nextItem]
      }
    }
    print("The shortest paths from i to j are:")
    for (i in 1:n){
      paths[[i]] <- c(source, paths[[i]])
      print(paste(source, "to", i, ":",(paste(paths[[i]],collapse ="-")),sep=" "))
    }
  }
}

plot.BellmanFord <- function(sol){
  dist <- sol$getDist()
  prev <- sol$getPrev()
  n <- dim(dist)[1]
  dist <- dist[n,]
  prev <- prev[n,]
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


