\name{graph}
\alias{graph}
\title{The graph Object}
\description{
This object represents a graph and has class methods for easy manipulation.

The graph class has the following class methods:
\itemize{
  \item getAdjMatrix() - Returns the adjacency matrix supplied to the algorithm.
  \item getSize() - Returns the size of the graph
  \item setSize(n) - Sets the size of the graph and enters interactive matrix creation.
  \item getNodeNames() - Returns the saved node names for the graph.
  \item setNodeNames(names) - Sets the node names for the graph, where names is a vector.
  \item isDirected() - Returns TRUE if the graph is directed.
  \item setIsDirected(Boolean) - Set if the graph is directed or not, input is either TRUE or FALSE.
  \item setAdjMatrix(matrix,directed=FALSE) - Replace the graph by providing an adjacency matrix.
  \item constFill(const) - Assign a constant as the cost of every arc in the graph.
  \item randFill(min=0,max=100) - Assign random costs to every arc in the graph.
}
}
\usage{
graph(n)
}
\arguments{
  \item{amountOfNodes}{The Number of nodes to make a graph with.}
  \item{isGraphDirected}{ Create directed graph? Default is TRUE.}
}
\examples{
\dontrun{
  A <- graph(6)
}
}
