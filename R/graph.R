#' This function generates a random graph in adjacency matrix notation.
#' @param num_nodes Integer; number of nodes available to be connected..
#' @param p Between 0 and 1; additional parameter to control number of edges to be generated. See details.
#' @details An edge is generated using Bernoulli(p) distribution.
#' @export
create_random_graph <- function(num_nodes, p = 0.5) {
  seq(num_nodes) %>%
    purrr::map(
      ~create_random_edges(num_nodes = num_nodes, zero_pos = .x, p = p)
    ) %>%
    do.call(rbind, .) %>%
    make_symmetric()
}

#' This function creates random number of edges
#' @param num_nodes Integer; number of nodes available to be connected.
#' @param zero_pos The position of the node that shouldn't have an edge.
#' @param p Probability of having an edge. This is a parameter to make sure
#' the desired number of edges is generated.
#' @keywords internal
create_random_edges <- function(num_nodes, zero_pos, p = 0.5) {
  edges <- rbinom(num_nodes, 1, p)
  edges[zero_pos] <- 0  # An edge to itself is not allowed.
  edges
}

#' Helper function to create symmetric matrix
#' @keywords internal
make_symmetric <- function(m0) {
  m1 <- m0 + t(m0)
  m1[m1 == 2] <- 1
  m1
}

#' This function plots graph from the function "create_random_graph".
#' @param rgraph matrix; output from "create_random_graph".
#' @export
plot_graph <- function(rgraph, vertex.size = 30, ...) {
  rgraph %>%
    igraph::graph_from_adjacency_matrix(mode = "undirected") %>%
    plot(vertex.size = vertex.size, ...)
}
