Dijkstra2 <- function(adjMatrix, sourceNode = 1)
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

  ########## MAIN CODE ##########
  requireNamespace("Rcpp",quietly = TRUE)

  N <- dim(adjMatrix)[1]
  Rcpp::cppFunction('
              Rcpp::List dijkstraCPP(NumericMatrix graph, int N, int src)
              {
                NumericVector parent(N);
                NumericVector dist(N);

                bool sptSet[N];

                for (int i = 0; i < N; i++) dist[i] = INT_MAX, parent[i] = NA_INTEGER, sptSet[i] = false;

                dist[src] = 0;
                parent[src] = 0;

                for (int count = 0; count < N-1; count++)
                {
                  int min = INT_MAX, min_index = INT_MAX;
                  for (int v = 0; v < N; v++)
                  {
                    if (sptSet[v] == false && dist[v] <= min) {
                      min = dist[v];
                      min_index = v;
                    }
                  }

                  int u = min_index;

                  sptSet[u] = true;

                  for (int v = 0; v < N; v++)
                  {
                    if (!sptSet[v] && dist[u] != INT_MAX && dist[u] + graph(u,v) < dist[v]) {
                      dist[v] = dist[u] + graph(u,v), parent[v] = u+1;
                    }
                  }
                }

                Rcpp::List ret;
                ret["0"] = dist;
                ret["1"] = parent;
                return ret;
              }

              ')

  result<- dijkstraCPP(adjMatrix, N, (sourceNode-1))
  dist <- unlist(result[1], use.names = FALSE)
  prev <- unlist(result[2], use.names = FALSE)

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

  ## Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)

  ## Set the name for the class
  class(me) <- append(class(me),"Dijkstra2")
  return(me)

}
print.Dijkstra2 = function(sol){
  ### Calculating ###
  dist <- sol$getDist()
  prev <- sol$getPrev()
  n <- length(dist)
  paths <- rep(list(c()),n)
  for (i in 1:n) {
    nextItem <- i
    if(prev[i]==0) source=i
    while(prev[nextItem]!=0){
      paths[[i]] <- c(nextItem, paths[[i]])
      nextItem <- prev[nextItem]
    }
  }


  ### Printing ###
  print("Shortest distance from source:")
  print(sol$getDist())
  cat("\n")
  print("Previous vertex:")
  print(sol$getPrev())
  cat("\n")
  print("The shortest paths from i to j are:")
  cat("\n")
  for (i in 1:n){
    paths[[i]] <- c(source, paths[[i]])
    print(paste(source, "to", i, ":",(paste(paths[[i]],collapse ="-")),sep=" "))
  }
}

plot.Dijkstra2 <- function(sol){
  dist <- sol$getDist()
  prev <- sol$getPrev()
  n <- length(dist)
  paths <- rep(list(c()),n)
  for (i in 1:n) {
    nextItem <- i
    if(prev[i]==0) source=i
    while(prev[nextItem]!=0){
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


