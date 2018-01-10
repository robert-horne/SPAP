######### Main Graph Class #########
graph <- function(amountOfNodes,isGraphDirected=TRUE)
{
  thisEnv <- environment()

  ##Validation
  if(!hasArg(amountOfNodes))
    stop("Please provide a number of nodes. e.g. graph(8,...)")
  if(!check.integer(amountOfNodes))
    stop("The number of nodes must be an integer. e.g. graph(8,...)")
  if(amountOfNodes<2|amountOfNodes>9999)
    stop("The number of nodes must be greater than 1 and less than 9999. e.g. graph(8,...)")

  n <- amountOfNodes
  nodeNames <- c(1:n)


  if(!is.logical(isGraphDirected)) {
    directed <- isGraphDirected
  }

  adjMatrix<-matrix(Inf,n,n)

  ## Create the list used to represent a graph
  me <- list(
    thisEnv = thisEnv,
    getEnv = function()
    {
      return(get("thisEnv",thisEnv))
    },
    getSize = function()
    {
      return(get("n",thisEnv))
    },
    setSize = function(n)
    {
      return(assign("n",n,thisEnv))
    },
    getNodeNames = function()
    {
      return(get("nodeNames",thisEnv))
    },
    setNodeNames = function(n)
    {
      return(assign("nodeNames",n,thisEnv))
    },
    isDirected = function()
    {
      return(get("directed",thisEnv))
    },

    getAdjMatrix = function()
    {
      return(get("adjMatrix",thisEnv))
    },
    setIsDirected = function(Boolean)
    {
      if(!is.logical(Boolean)) stop("Please provide either TRUE or FALSE as an argument.")
      if(Boolean) {
        matrix<- get("adjMatrix",thisEnv)
        nrows<-nrow(matrix)
        ncols<-ncol(matrix)
        for (i in 1:nrows){
          for (j in 1:ncols) {
            if(i!=j & i<j) {
              matrix[j,i]<-matrix[i,j]
            }
          }
        }
        assign("adjMatrix",matrix, thisEnv)
        assign("directed",TRUE,thisEnv)
      }
    },
    setAdjMatrix = function(matrix,directed=FALSE)
    {
      if(!hasArg(matrix)){
        A <- createMatrix("i")
        assign("adjMatrix", A, thisEnv)
        assign("nodeNames",c(1:nrow(A)),thisEnv)
        return("Matrix set.")
      }
      if(!is.matrix(matrix)) stop("Please provide a valid matrix.")
      nrows<-nrow(matrix)
      ncols<-ncol(matrix)
      if(nrows!=ncols | !check.integer(nrows)) stop("Please provide a NxN matrix, where N is an integer greater than 1.")
      if(directed) {
        for (i in 1:nrows){
          for (j in 1:ncols) {
            if(i!=j & i<j) {
              matrix[j,i]<-matrix[i,j]
            }
          }
        }
        assign("directed",TRUE,thisEnv)
      }
      diag(matrix) <- Inf
      assign("n",nrows,thisEnv)
      assign("adjMatrix",matrix, thisEnv)
      assign("nodeNames",c(1:nrows),thisEnv)
      return("Matrix set.")
    },

    constFill = function(const)
    {
      if(!is.numeric(const)|const==0) return("Matrix not filled. Please provide a non-zero number.")
      assign("adjMatrix",createMatrix("f", get("n", thisEnv), const=const),thisEnv)
      assign("nodeNames",c(1:get("n",thisEnv)),thisEnv)
      return("Matrix filled.")
    },

    randFill = function(min=0,max=100)
    {
      if(!is.numeric(min)|!is.numeric(max)) return("Matrix not filled. Please provide a numeric minimum and maximum. e.g. graph$fillWithRandom(min=0,max=5)")
      n <- get("n", thisEnv)
      assign("adjMatrix",createMatrix("r", n, min=min,max=max),thisEnv)
      assign("directed",FALSE,thisEnv)
      assign("nodeNames",c(1:n),thisEnv)
      return("Matrix filled.")
    }
  )

  ## Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)

  ## Set the name for the class
  class(me) <- append(class(me),"graph")
  return(me)
}

######### GRAPH CLASS PRINT AND PLOT DEFINITIONS #########
print.graph = function(graph){
  matrix <- graph$getAdjMatrix()
  print(matrix)
}

plot.graph = function(graph,
                      edge.arrow.size=.1,
                      edge.curved=0,
                      edge.arrow.mode="-",
                      vestex.size=10,
                      vertex.color="orange",
                      vertex.frame.color="#555555",
                      vertex.label=FALSE,
                      vertex.label.color="black",
                      vertex.label.cex=1
                      )
{
##Load igraph
requireNamespace("igraph",quietly = TRUE)

A<-igraph::graph_from_adjacency_matrix(graph$getAdjMatrix(), mode = "directed")

vertexLabels <- graph$getNodeNames()
if(vertex.label!=FALSE) {
  vertexLabels = vertex.label
}

plot(A,
     edge.arrow.size=edge.arrow.size,
     edge.curved=edge.curved,
     # edge.arrow.mode=edge.arrow.mode, ##If it is a character vector then “<” and “<-” specify backward, “>” and “->” forward arrows and “<>” and “<->” stands for both arrows. All other values mean no arrows, perhaps you should use “-” or “–” to specify no arrows.
     vestex.size=vestex.size,
     vertex.color=vertex.color, ##colors() to get R colors
     vertex.frame.color=vertex.frame.color,
     vertex.label=vertexLabels,
     vertex.label.color=vertex.label.color,
     vertex.label.cex=vertex.label.cex
    )
}
