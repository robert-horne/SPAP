\name{BellmanFord}
\alias{BellmanFord}
\title{
The Bellman-Ford algorithm.
}
\description{
This Bellman-Ford implementation solves the Shortest Path Problem (SPP), requiring an adjacency matrix and a source node to be supplied, creating an object of class Bellman-Ford with the solution stored within, this can the be accessed by printing the object or plotting it, for the raw solution data use the class method getPrev().
The Bellman-Ford algorithm discovers negative cycles reachable from the source and reports them.

The graph class has the following class methods:
\itemize{
  \item getAdjMatrix() - Returns the adjacency matrix supplied to the algorithm.
  \item getDist() - Returns the matrix of distances from the source to each node at each iteration.
  \item getPrev() - Returns the matrix of previous nodes for every node at each iteration.
  \item negCycleExists() - Returns TRUE if the algorithm found a negative cycle reachable from the source.
  \item getNegCycle() - If a negative cycle was found it can be accessed using this method.
}
}
\usage{
BellmanFord(matrix,1)
}
\arguments{
  \item{adjMatrix}{The ajacency matrix representing the graph to be solved, instead of a matrix the graph class can be supplied, and the adjacency matrix will be extracted.}
  \item{sourceNode}{The source node, corresponds to the node's row in the matrix supplied. Default is 1.}
}
\examples{
\dontrun{
  solution <- BellmanFord(matrix,1)
  print(solution)
  plot(solution)
  }
}
