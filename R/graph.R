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
#' @param vertex.size plot size of the node.
#' @param ... extra parameters to be passed to the 'igraph::plot.igraph' function.
#' @export
plot_graph <- function(rgraph, vertex.size = 30, ...) {
  rgraph %>%
    igraph::graph_from_adjacency_matrix(mode = "undirected") %>%
    plot(vertex.size = vertex.size, ...)
}
# #' @keywords internal
# ggplot_graph <- function(graph, color) {
#   if (missing(color)) {color = 'grey'}
#   GGally::ggnet2(graph,
#     arrow.size = 6, arrow.gap = 0.03, edge.color = color,
#     label = paste("X", seq(vcount(graph)), sep = ""),
#     color = 'orange',
#    )
# }
#
# plot(a, vertex.size = 35,
#      edge.arrow.size = 0.5, edge.width = 1, edge.arrow.size = 0.15,
#      asp = 1.5)


#' @keywords internal
table_to_edgelist <- function(table0) {
  edgelist <- c()
  for (i in 1:nrow(table0)) {
    current <- table0[i,]
    fixed <- current$fixed[[1]]
    given <- current$given[[1]]
    if (!purrr::is_empty(given)) {
      edgelist <- rbind(edgelist, cbind(fixed, given))
    }
  }
  edgelist
}


#' Compare two graphs
#' @description This function compares two graphs and labels the matched / unmatched
#' edges with different colors.
#' @param mode If "directed", the function uses only the upper triangular part of the
#' adjacency matrix; otherwise, the function uses the full matrix.
#' @param m1 adjacency matrix
#' @param m2 adjacency matrix
#' @param color_vec characters vector; see description for details.
#' @details By default, green color applies to edges that are present in both graphs;
#' black color applies to edges that are present in the first graph but not the second
#' graph; red color applies to edges that are present in the second graph but
#' not the first graph.
#' @export
compare_graphs <- function(m1, m2,  mode = "undirected",
                           color_vec = c("green", "black", "red")) {
  num_nodes <- nrow(m1)

  m3 <- m1 + m2
  m3[m3 != 2] <- 0  #matched edges
  matched_graph <- adj_to_graph(m3 / 2, mode)
  V(matched_graph)$name <- paste("X", seq(num_nodes), sep = "")
  E(matched_graph)$color <- "green"

  m4 <- m1 + m3 - m2
  m4[m4 != 1] <- 0
  unmatched_graph <- adj_to_graph(m4, mode)  #unmatched edges
  V(unmatched_graph)$name <- paste("X", seq(num_nodes), sep = "")
  E(unmatched_graph)$color <- "black"

  m5 <- m2 + m3 - m1
  m5[m5 != 1] <- 0
  extra_graph <- adj_to_graph(m5, mode)  #extra edges
  V(extra_graph)$name <- paste("X", seq(num_nodes), sep = "")
  E(extra_graph)$color <- "red"

  res_graph <- matched_graph + unmatched_graph + extra_graph
  merge_color_attr(res_graph)
}
#' @keywords internal
merge_color_attr <- function(graph) {
  res <- E(graph)$color
  c1 <- E(graph)$color_1
  replace_index <- which(!is.na(c1))
  res[replace_index] <- c1[replace_index]

  c2 <- E(graph)$color_2
  replace_index <- which(!is.na(c2))
  res[replace_index] <- c2[replace_index]

  E(graph)$color <- res
  graph
}
#' Convert a adjacency matrix to graph.
#' @description This is used mainly for converting the estimated matrix to a graph.
#' Using the lower triangular matrix corresponds to following the factorisation
#' in the natural order, i.e. from the first column to the last column of the data.
#' @keywords internal
adj_to_graph <- function(adj_matrix, mode) {
  igraph::graph_from_adjacency_matrix(
    adj_matrix * lower.tri(adj_matrix), mode = mode
  )
}
