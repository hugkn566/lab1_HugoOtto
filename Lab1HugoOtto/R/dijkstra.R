#' Compute Dijkstra's algorithm
#' 
#' @param graph A data.frame with three variables containing the edges of a graph and the weight of the edge.
#' @param init_node A node within \code{graph} from which to calculate the shortest path to every other node.
#' @details This function computes Dijkstra's algorithm to find the shortest paths between nodes in a graph (\href{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}{Wikipedia}). 
#' @return A vector containing the shortest paths from \code{init_node} to every other node in the graph.
#' @examples
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3) 

dijkstra <- function(graph, init_node){
  
  ## Performs initial tests to check if the inputs are appropriate to carry out the algorithm  
  if(is.data.frame(graph)!=TRUE){
    stop("The graph is not a data.frame!")
  }
  if(is.numeric(init_node)!=TRUE){
    stop("The initial node is not a numeric scalar!")
  }
  if(sum(init_node == graph$v1)==0){
    stop("The initial node does not exist in the graph!")
  }
  
  ## Creates a vertex set to keep track of which vertices have been explored
  Q <- 1:length(unique(graph$v1))
    
  ##  Creates vector of distances to each vertex 
  dist <- rep(Inf, length(unique(graph$v1)))
  
  ## Element used in pseudocode, does not seem required to for the algorithm to work
  #prev <- rep(NA, length(unique(graph$v1))) 
  
  # Sets the distance to the initial node to 0
  dist[init_node] <- 0
  
  # Loops until the sum of Q is 0, meaning that every vertex has been visited
  while (sum(Q)>0) {
    
    # Selects the vertex u with the lowest distance from among vertices that have not been visited, checked by [Q>0])
    # The second part of this line is used if at some point there are two vertices with equal distance,
    # where if such a case arises it will select the vertex with the highest number, using which.max
    # Technically the function still returns the correct values without this check, but the coming loop
    # will throw warnings if the length of u is more than 1    
    u <- Q[which(dist==min(dist[Q>0]))][which.max(Q[which(dist==min(dist[Q>0]))])]

    
    # Loops over the vertices in v2 that neighbor the current vertex u
    # The second part of this line is a check to only include vertices still in Q, i.e vertices that
    # have not been visited yet. This to avoid exploring edges that have already been explored in the opposite direction
    for (v in wiki_graph$v2[graph$v1==u][wiki_graph$v2[graph$v1==u] %in% Q]) {
      
      # Computes the distance from the initial node to vertex v if the path has to go through vertex u
      alt <- dist[u]+graph$w[graph$v1==u & graph$v2==v]
      
      # If the computed distance is shorter than the current distance, update it
      if(alt<dist[v]){
        dist[v] <- alt
        
        ## Element used in pseudocode, does not seem required to for the algorithm to work
        #prev[v] <- u
      }
      
  }
    # Sets the current vertex to 0, indicating that it has been visited
    Q[u] <- 0  
  }
 return(dist)   
}
