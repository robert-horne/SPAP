#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
List dijkstraCPP(NumericMatrix graph, int N, int src)
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
